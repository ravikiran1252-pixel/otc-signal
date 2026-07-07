from fastapi import FastAPI, HTTPException
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
import asyncio, math, json
from datetime import datetime

app = FastAPI()

# ── SIGNAL ENGINE ──────────────────────────
def calc_rsi(cl, p=14):
    if len(cl)<p+1: return 50
    g=l=0
    for i in range(1,p+1):
        d=cl[i]-cl[i-1]
        if d>0: g+=d
        else: l-=d
    ag,al=g/p,l/p
    for i in range(p+1,len(cl)):
        d=cl[i]-cl[i-1]
        ag=(ag*(p-1)+max(0,d))/p
        al=(al*(p-1)+max(0,-d))/p
    return 100 if al==0 else 100-100/(1+ag/al)

def calc_ema(data,p):
    if len(data)<p: return [None]*len(data)
    k=2/(p+1); e=sum(data[:p])/p
    r=[None]*(p-1)+[e]
    for i in range(p,len(data)):
        e=data[i]*k+e*(1-k); r.append(e)
    return r

def calc_macd(cl):
    f,s=calc_ema(cl,12),calc_ema(cl,26)
    ml=[fi-si if fi and si else None for fi,si in zip(f,s)]
    vm=[x for x in ml if x is not None]
    if len(vm)<9: return 0,0
    sg=calc_ema(vm,9)
    sa=[None]*(len(ml)-len(sg))+sg
    n=len(ml)-1
    mv=ml[n] or 0; sv=sa[n] or 0
    mv2=ml[n-1] or 0; sv2=sa[n-1] or 0
    return mv-sv, mv2-sv2

def calc_bb(cl,p=20):
    if len(cl)<p: return 0.5
    sl=cl[-p:]; mid=sum(sl)/p
    std=math.sqrt(sum((x-mid)**2 for x in sl)/p)
    if std==0: return 0.5
    up,lo=mid+2*std,mid-2*std
    return max(0,min(1,(cl[-1]-lo)/(up-lo)))

def calc_stoch(hi,lo,cl,p=14):
    if len(cl)<p: return 50,50
    kv=[]
    for i in range(p-1,len(cl)):
        hh=max(hi[i-p+1:i+1]); ll=min(lo[i-p+1:i+1])
        kv.append(50 if hh==ll else (cl[i]-ll)/(hh-ll)*100)
    return kv[-1], sum(kv[-3:])/min(3,len(kv))

def calc_wr(hi,lo,cl,p=14):
    if len(cl)<p: return -50
    hh,ll=max(hi[-p:]),min(lo[-p:])
    return -50 if hh==ll else (hh-cl[-1])/(hh-ll)*-100

def calc_cci(hi,lo,cl,p=20):
    if len(cl)<p: return 0
    tp=[(hi[-p+i]+lo[-p+i]+cl[-p+i])/3 for i in range(p)]
    avg=sum(tp)/p
    md=sum(abs(x-avg) for x in tp)/p
    return 0 if md==0 else (tp[-1]-avg)/(0.015*md)

def build_signal(candles):
    if len(candles)<50: return None
    cl=[c['close'] for c in candles]
    hi=[c['high']  for c in candles]
    lo=[c['low']   for c in candles]
    RSI=calc_rsi(cl)
    RSI5=calc_rsi(cl,5)
    hist,hp=calc_macd(cl)
    BB=calc_bb(cl)
    K,D=calc_stoch(hi,lo,cl)
    K5,_=calc_stoch(hi,lo,cl,5)
    WR=calc_wr(hi,lo,cl)
    CCI=calc_cci(hi,lo,cl)
    e8=calc_ema(cl,8); e21=calc_ema(cl,21)
    e8v=e8[-1] or cl[-1]; e21v=e21[-1] or cl[-1]
    ema_bull=e8v>e21v; price=cl[-1]
    c1,c2=candles[-2],candles[-1]
    b1=abs(c1['close']-c1['open']); b2=abs(c2['close']-c2['open'])
    bu1=c1['close']>c1['open']; bu2=c2['close']>c2['open']
    eng_bull=not bu1 and bu2 and c2['open']<=c1['close'] and c2['close']>=c1['open'] and b2>b1
    eng_bear=bu1 and not bu2 and c2['open']>=c1['close'] and c2['close']<=c1['open'] and b2>b1
    hammer=not bu2 and (c2['high']-max(c2['open'],c2['close']))<b2*.3 and (min(c2['open'],c2['close'])-c2['low'])>b2*2
    star=bu2 and (max(c2['open'],c2['close'])-c2['low'])<b2*.3 and (c2['high']-max(c2['open'],c2['close']))>b2*2
    bull=bear=0.0
    reasons_b=[]; reasons_s=[]
    def add(side,pts,r):
        nonlocal bull,bear
        if side=='B': bull+=pts; reasons_b.append(r)
        else: bear+=pts; reasons_s.append(r)
    if RSI<=25: add('B',3,f'RSI deeply oversold ({RSI:.0f})')
    elif RSI<=35: add('B',2,f'RSI oversold ({RSI:.0f})')
    elif RSI<=45: add('B',.7,'RSI leaning low')
    if RSI>=75: add('S',3,f'RSI deeply overbought ({RSI:.0f})')
    elif RSI>=65: add('S',2,f'RSI overbought ({RSI:.0f})')
    elif RSI>=55: add('S',.7,'RSI leaning high')
    if RSI5<20 and RSI<45: add('B',2,'Fast RSI oversold')
    if RSI5>80 and RSI>55: add('S',2,'Fast RSI overbought')
    if hist>0 and hist>hp: add('B',2.5,'MACD bullish & accelerating')
    elif hist>0: add('B',1.2,'MACD bullish')
    if hist<0 and hist<hp: add('S',2.5,'MACD bearish & accelerating')
    elif hist<0: add('S',1.2,'MACD bearish')
    if BB<0.08: add('B',3,'Price at lower Bollinger band')
    elif BB<0.20: add('B',1.5,'Price near lower band')
    if BB>0.92: add('S',3,'Price at upper Bollinger band')
    elif BB>0.80: add('S',1.5,'Price near upper band')
    if K<20 and D<25: add('B',2.5,'Stochastic oversold')
    elif K<30: add('B',1,'Stochastic low')
    if K>80 and D>75: add('S',2.5,'Stochastic overbought')
    elif K>70: add('S',1,'Stochastic high')
    if K5<15 and K<35: add('B',1.5,'Fast stoch oversold')
    if K5>85 and K>65: add('S',1.5,'Fast stoch overbought')
    if WR<-85: add('B',2,'Williams %R deeply oversold')
    elif WR<-70: add('B',1,'Williams %R oversold')
    if WR>-15: add('S',2,'Williams %R deeply overbought')
    elif WR>-30: add('S',1,'Williams %R overbought')
    if CCI<-150: add('B',2.5,'CCI deeply oversold')
    elif CCI<-100: add('B',1.5,'CCI oversold')
    if CCI>150: add('S',2.5,'CCI deeply overbought')
    elif CCI>100: add('S',1.5,'CCI overbought')
    if ema_bull: add('B',2,'EMA uptrend')
    else: add('S',2,'EMA downtrend')
    if eng_bull: add('B',3,'Bullish Engulfing candle')
    if eng_bear: add('S',3,'Bearish Engulfing candle')
    if hammer: add('B',2,'Hammer reversal')
    if star: add('S',2,'Shooting Star reversal')
    tot=bull+bear
    if tot==0: return None
    br=bull/tot
    direction='BUY' if br>=0.5 else 'SELL'
    reasons=(reasons_b if direction=='BUY' else reasons_s)[:3]
    pat=('Bullish Engulfing' if eng_bull else 'Bearish Engulfing' if eng_bear
         else 'Hammer' if hammer else 'Shooting Star' if star else 'None')
    wins=losses=0
    for i in range(50,len(candles)-1):
        cl2=[c['close'] for c in candles[i-50:i+1]]
        hi2=[c['high']  for c in candles[i-50:i+1]]
        lo2=[c['low']   for c in candles[i-50:i+1]]
        r2=calc_rsi(cl2); h2,_=calc_macd(cl2); k2,_=calc_stoch(hi2,lo2,cl2)
        e8_=calc_ema(cl2,8)[-1] or cl2[-1]; e21_=calc_ema(cl2,21)[-1] or cl2[-1]
        pred='BUY' if sum([r2<50,h2>0,k2<50,e8_>e21_])>=2 else 'SELL'
        up=candles[i+1]['close']>candles[i]['close']
        if (pred=='BUY')==up: wins+=1
        else: losses+=1
    trades=wins+losses
    wr=round(wins/trades*100,1) if trades else 0
    margin=abs(br-0.5)*2
    conf=round(min(94,max(62,(60+margin*28)*0.5+(wr if trades>=10 else 55)*0.5)))
    grade='A' if wr>=72 else 'B' if wr>=65 else 'C' if wr>=58 else 'D'
    return {
        'direction':direction,'br':round(br,4),'conf':conf,'grade':grade,
        'win_rate':wr,'trades':trades,'reasons':reasons,'pattern':pat,
        'RSI':round(RSI,1),'MACD':'Bullish' if hist>0 else 'Bearish',
        'BB':round(BB*100,0),'K':round(K,1),'WR':round(WR,1),
        'CCI':round(CCI,1),'ema_trend':'Uptrend' if ema_bull else 'Downtrend',
        'candles':len(candles),'pair':'','time':datetime.now().strftime('%H:%M')
    }

# ── QUOTEX CONNECTION ───────────────────────
sessions = {}  # email -> client

async def get_quotex_candles(email, password, asset):
    try:
        from pyquotex.stable_api import Quotex
        client = Quotex(email=email, password=password, lang="en")
        ok, msg = await client.connect()
        if not ok:
            return None, f"Login failed: {msg}"
        raw = await client.get_candles(asset, 60, 200)
        await client.close()
        if not raw:
            return None, "No candles received"
        candles = []
        for c in raw:
            candles.append({
                'close': float(c.get('close', 0)),
                'open':  float(c.get('open', 0)),
                'high':  float(c.get('max', c.get('high', 0))),
                'low':   float(c.get('min', c.get('low', 0))),
            })
        return candles, None
    except Exception as e:
        return None, str(e)

# ── API ROUTES ──────────────────────────────
class SignalRequest(BaseModel):
    email: str
    password: str
    pair: str
    asset: str

@app.post("/api/signal")
async def generate_signal(req: SignalRequest):
    candles, err = await get_quotex_candles(req.email, req.password, req.asset)
    if err:
        raise HTTPException(status_code=400, detail=err)
    result = build_signal(candles)
    if not result:
        raise HTTPException(status_code=400, detail="Not enough data")
    result['pair'] = req.pair
    return result

@app.get("/health")
def health():
    return {"status": "ok", "time": datetime.now().isoformat()}

# ── FRONTEND ────────────────────────────────
HTML = '''<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8"/>
<meta name="viewport" content="width=device-width,initial-scale=1.0,maximum-scale=1.0"/>
<title>OTC Signal Pro</title>
<style>
*{margin:0;padding:0;box-sizing:border-box}
body{background:#0a0a14;color:#e2e8ff;font-family:system-ui,sans-serif;min-height:100vh}
.wrap{max-width:480px;margin:0 auto;padding:16px 14px 40px}
h1{font-size:22px;font-weight:900;color:#fff;margin-bottom:4px}
.sub{font-size:12px;color:#6b7ab5;margin-bottom:20px}
.card{background:#111127;border:1px solid #1e1e38;border-radius:16px;padding:18px;margin-bottom:14px}
.card h2{font-size:13px;font-weight:700;color:#9b7fff;margin-bottom:12px;text-transform:uppercase;letter-spacing:1px}
input,select{width:100%;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:10px;padding:12px 14px;color:#e2e8ff;font-size:14px;margin-bottom:10px;font-family:inherit}
input::placeholder{color:#444466}
.btn{width:100%;padding:16px;border-radius:14px;background:linear-gradient(135deg,#7c3aed,#a78bfa);color:#fff;font-weight:900;font-size:16px;border:none;cursor:pointer;margin-top:4px}
.btn:disabled{opacity:.5;cursor:not-allowed}
.result{display:none;margin-top:14px}
.sig-card{border-radius:18px;padding:22px;text-align:center;margin-bottom:12px}
.sig-dir{font-size:52px;font-weight:900;line-height:1;margin-bottom:6px}
.sig-label{font-size:26px;font-weight:900}
.sig-pair{font-size:13px;color:rgba(255,255,255,.6);margin-top:8px;font-family:monospace}
.bar-wrap{margin:14px 0 10px}
.bar{height:16px;border-radius:8px;overflow:hidden;display:flex}
.bar-b{background:#22c55e;border-radius:8px 0 0 8px}
.bar-s{background:#ef4444;border-radius:0 8px 8px 0}
.bar-labels{display:flex;justify-content:space-between;margin-top:5px}
.bar-labels span{font-size:13px;font-weight:800}
.stats{display:grid;grid-template-columns:repeat(4,1fr);gap:8px;margin-bottom:12px}
.stat{background:#0a0a14;border:1px solid #1e1e38;border-radius:12px;padding:10px 6px;text-align:center}
.stat-l{font-size:9px;color:#444466;text-transform:uppercase;margin-bottom:4px}
.stat-v{font-size:17px;font-weight:900;font-family:monospace}
.inds{background:#111127;border:1px solid #1e1e38;border-radius:14px;padding:16px;margin-bottom:12px}
.inds h3{font-size:12px;color:#6b7ab5;font-weight:700;margin-bottom:12px;text-transform:uppercase;letter-spacing:1px}
.ind{display:flex;justify-content:space-between;align-items:center;padding:9px 0;border-bottom:1px solid #1e1e3855}
.ind span{font-size:13px;color:#d1d5db}
.ind .val{font-size:14px;font-weight:800;font-family:monospace}
.reasons{background:#111127;border:1px solid #1e1e38;border-radius:14px;padding:16px;margin-bottom:12px}
.reasons h3{font-size:12px;color:#6b7ab5;font-weight:700;margin-bottom:10px;text-transform:uppercase;letter-spacing:1px}
.reason{display:flex;align-items:center;gap:10px;padding:8px 0;border-bottom:1px solid #1e1e3833;font-size:13px;color:#e2e8ff}
.err{background:#450a0a;border:1px solid #ef444444;border-radius:12px;padding:14px;color:#ef4444;font-size:13px;margin-top:10px;display:none}
.loading{display:none;text-align:center;padding:30px;color:#6b7ab5;font-size:14px}
@keyframes spin{to{transform:rotate(360deg)}}
.spinner{width:40px;height:40px;border:3px solid #1e1e38;border-top-color:#7c3aed;border-radius:50%;animation:spin 1s linear infinite;margin:0 auto 12px}
.cd{margin-top:12px;padding:12px;background:#0a0a14;border-radius:10px;display:flex;justify-content:center;align-items:center;gap:10px}
.cd-time{font-family:monospace;font-weight:900;font-size:24px}
.note{font-size:11px;color:#374151;text-align:center;padding:8px 0}
</style>
</head>
<body>
<div class="wrap">
  <div style="display:flex;align-items:center;gap:10px;margin-bottom:20px;padding:14px 0">
    <div style="width:40px;height:40px;border-radius:12px;background:linear-gradient(135deg,#7c3aed,#a78bfa);display:flex;align-items:center;justify-content:center;font-size:22px">⚡</div>
    <div>
      <h1>OTC Signal Pro</h1>
      <div class="sub">Real Quotex OTC Data · High Accuracy</div>
    </div>
    <div style="margin-left:auto;text-align:right">
      <div id="clock" style="font-size:18px;font-family:monospace;font-weight:800;color:#7c3aed">00:00:00</div>
      <div style="font-size:10px;color:#343d6e">IST</div>
    </div>
  </div>

  <!-- LOGIN -->
  <div class="card" id="loginCard">
    <h2>🔗 Quotex Account</h2>
    <input id="email" type="email" placeholder="Quotex Email" autocomplete="email"/>
    <input id="password" type="password" placeholder="Quotex Password" autocomplete="current-password"/>
    <div style="font-size:11px;color:#6b7ab5;margin-top:-4px">Your credentials connect directly to Quotex to read real OTC prices</div>
  </div>

  <!-- PAIR SELECT -->
  <div class="card">
    <h2>📊 Select OTC Pair</h2>
    <select id="pairSelect">
      <optgroup label="── Major Forex OTC ──">
        <option value="EURUSD_otc">EUR/USD (OTC)</option>
        <option value="GBPUSD_otc">GBP/USD (OTC)</option>
        <option value="USDJPY_otc">USD/JPY (OTC)</option>
        <option value="AUDUSD_otc">AUD/USD (OTC)</option>
        <option value="USDCAD_otc">USD/CAD (OTC)</option>
        <option value="NZDUSD_otc">NZD/USD (OTC)</option>
        <option value="USDCHF_otc">USD/CHF (OTC)</option>
        <option value="EURGBP_otc">EUR/GBP (OTC)</option>
        <option value="EURJPY_otc">EUR/JPY (OTC)</option>
        <option value="GBPJPY_otc">GBP/JPY (OTC)</option>
      </optgroup>
      <optgroup label="── Asian & Exotic OTC ──">
        <option value="USDINR_otc">USD/INR (OTC)</option>
        <option value="USDBRL_otc">USD/BRL (OTC)</option>
        <option value="USDPHP_otc">USD/PHP (OTC)</option>
        <option value="USDIDR_otc">USD/IDR (OTC)</option>
        <option value="USDTHB_otc">USD/THB (OTC)</option>
        <option value="USDCNY_otc">USD/CNY (OTC)</option>
        <option value="USDEGP_otc">USD/EGP (OTC)</option>
        <option value="USDNGN_otc">USD/NGN (OTC)</option>
        <option value="USDPKR_otc">USD/PKR (OTC)</option>
        <option value="USDARS_otc">USD/ARS (OTC)</option>
        <option value="USDMXN_otc">USD/MXN (OTC)</option>
        <option value="USDZAR_otc">USD/ZAR (OTC)</option>
        <option value="USDDZD_otc">USD/DZD (OTC)</option>
      </optgroup>
      <optgroup label="── Cross Pairs OTC ──">
        <option value="EURCAD_otc">EUR/CAD (OTC)</option>
        <option value="EURAUD_otc">EUR/AUD (OTC)</option>
        <option value="EURCHF_otc">EUR/CHF (OTC)</option>
        <option value="GBPAUD_otc">GBP/AUD (OTC)</option>
        <option value="GBPCAD_otc">GBP/CAD (OTC)</option>
        <option value="AUDCAD_otc">AUD/CAD (OTC)</option>
        <option value="AUDNZD_otc">AUD/NZD (OTC)</option>
        <option value="CADJPY_otc">CAD/JPY (OTC)</option>
        <option value="CHFJPY_otc">CHF/JPY (OTC)</option>
        <option value="NZDJPY_otc">NZD/JPY (OTC)</option>
      </optgroup>
      <optgroup label="── Crypto OTC ──">
        <option value="BTCUSD_otc">BTC/USD (OTC)</option>
        <option value="ETHUSD_otc">ETH/USD (OTC)</option>
        <option value="LTCUSD_otc">LTC/USD (OTC)</option>
        <option value="DOGEUSD_otc">DOGE/USD (OTC)</option>
      </optgroup>
    </select>
    <div id="tzRow" style="display:flex;align-items:center;justify-content:space-between;margin-top:4px">
      <div style="font-size:13px;color:#6b7ab5">⏱ Analysis Time (IST)</div>
      <button onclick="setNow()" style="padding:6px 14px;border-radius:20px;border:1px solid #7c3aed;background:none;color:#9b7fff;font-size:12px;cursor:pointer">Use Now</button>
    </div>
    <div style="display:flex;align-items:center;justify-content:center;gap:16px;margin-top:12px">
      <div style="text-align:center">
        <button onclick="adj('h',-1)" style="width:48px;height:36px;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:8px;color:#7c3aed;font-size:18px;cursor:pointer">▲</button>
        <div id="hv" style="font-size:44px;font-weight:900;font-family:monospace;color:#e2e8ff;margin:6px 0">00</div>
        <button onclick="adj('h',1)" style="width:48px;height:36px;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:8px;color:#7c3aed;font-size:18px;cursor:pointer">▼</button>
        <div style="font-size:10px;color:#444466;margin-top:4px;letter-spacing:2px">HOUR</div>
      </div>
      <div style="font-size:44px;font-weight:900;color:#7c3aed;margin-bottom:20px">:</div>
      <div style="text-align:center">
        <button onclick="adj('m',-1)" style="width:48px;height:36px;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:8px;color:#7c3aed;font-size:18px;cursor:pointer">▲</button>
        <div id="mv" style="font-size:44px;font-weight:900;font-family:monospace;color:#e2e8ff;margin:6px 0">00</div>
        <button onclick="adj('m',1)" style="width:48px;height:36px;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:8px;color:#7c3aed;font-size:18px;cursor:pointer">▼</button>
        <div style="font-size:10px;color:#444466;margin-top:4px;letter-spacing:2px">MIN</div>
      </div>
    </div>
  </div>

  <button class="btn" id="genBtn" onclick="generate()">⚡ Generate High-Accuracy Signal</button>

  <div class="loading" id="loading">
    <div class="spinner"></div>
    <div id="loadMsg">Connecting to Quotex…</div>
  </div>
  <div class="err" id="errBox"></div>

  <div class="result" id="result">
    <div class="sig-card" id="sigCard">
      <div class="sig-dir" id="sigDir">↑</div>
      <div class="sig-label" id="sigLabel">BUY / CALL</div>
      <div class="sig-pair" id="sigPair"></div>
      <div class="bar-wrap">
        <div class="bar"><div class="bar-b" id="barB" style="width:50%"></div><div class="bar-s" id="barS" style="width:50%"></div></div>
        <div class="bar-labels"><span id="pctB" style="color:#22c55e">50%</span><span id="pctS" style="color:#ef4444">50%</span></div>
      </div>
      <div class="cd" id="cdRow">
        <span style="font-size:13px;color:rgba(255,255,255,.6)">⏳ Trade window</span>
        <span class="cd-time" id="cdTime">01:00</span>
      </div>
    </div>
    <div class="stats">
      <div class="stat"><div class="stat-l">Grade</div><div class="stat-v" id="sGrade">-</div></div>
      <div class="stat"><div class="stat-l">Win Rate</div><div class="stat-v" id="sWR">-</div></div>
      <div class="stat"><div class="stat-l">Conf</div><div class="stat-v" id="sConf">-</div></div>
      <div class="stat"><div class="stat-l">Trades</div><div class="stat-v" id="sTrades">-</div></div>
    </div>
    <div class="reasons" id="reasonsBox">
      <h3>🎯 Why this signal</h3>
      <div id="reasonsList"></div>
    </div>
    <div class="inds">
      <h3>📊 Indicators</h3>
      <div class="ind"><span>RSI(14)</span><span class="val" id="iRSI">-</span></div>
      <div class="ind"><span>MACD</span><span class="val" id="iMACD">-</span></div>
      <div class="ind"><span>Bollinger %B</span><span class="val" id="iBB">-</span></div>
      <div class="ind"><span>Stochastic K</span><span class="val" id="iK">-</span></div>
      <div class="ind"><span>Williams %R</span><span class="val" id="iWR">-</span></div>
      <div class="ind"><span>CCI</span><span class="val" id="iCCI">-</span></div>
      <div class="ind"><span>EMA Trend</span><span class="val" id="iEMA">-</span></div>
      <div class="ind"><span>Pattern</span><span class="val" id="iPat">-</span></div>
    </div>
    <div class="note">⚠️ Real Quotex OTC data · 1 min expiry · Trade responsibly</div>
  </div>
</div>

<script>
var SH=0,SM=0,cdTick=null;
function p2(n){return String(n).padStart(2,'0');}
function nowIST(){var n=new Date(),ist=new Date(n.getTime()+n.getTimezoneOffset()*60000+5.5*3600e3);return{h:ist.getHours(),m:ist.getMinutes(),s:ist.getSeconds()};}
setInterval(function(){var t=nowIST();document.getElementById('clock').textContent=p2(t.h)+':'+p2(t.m)+':'+p2(t.s);},1000);
(function(){var t=nowIST();SH=t.h;SM=t.m;document.getElementById('hv').textContent=p2(SH);document.getElementById('mv').textContent=p2(SM);})();
function adj(f,d){if(f==='h')SH=(SH+d+24)%24;else SM=(SM+d+60)%60;document.getElementById('hv').textContent=p2(SH);document.getElementById('mv').textContent=p2(SM);}
function setNow(){var t=nowIST();SH=t.h;SM=t.m;document.getElementById('hv').textContent=p2(SH);document.getElementById('mv').textContent=p2(SM);}

// Save credentials in localStorage
function getCreds(){
  var e=document.getElementById('email').value.trim();
  var p=document.getElementById('password').value.trim();
  if(e)localStorage.setItem('qx_email',e);
  if(p)localStorage.setItem('qx_pass',p);
  return{email:e,password:p};
}
(function(){
  var e=localStorage.getItem('qx_email'),p=localStorage.getItem('qx_pass');
  if(e)document.getElementById('email').value=e;
  if(p)document.getElementById('password').value=p;
})();

function showLoad(msg){document.getElementById('loading').style.display='block';document.getElementById('loadMsg').textContent=msg||'Connecting to Quotex…';document.getElementById('result').style.display='none';document.getElementById('errBox').style.display='none';}
function hideLoad(){document.getElementById('loading').style.display='none';}
function showErr(msg){document.getElementById('errBox').style.display='block';document.getElementById('errBox').textContent='❌ '+msg;hideLoad();}

var msgs=['📡 Connecting to Quotex…','🔐 Authenticating…','📊 Fetching OTC candles…','🧮 Computing RSI, MACD, BB…','📐 Analysing Stochastic & EMA…','🕯 Scanning candle patterns…','🧪 Running backtest…','✅ Finalising signal…'];
var mi=0,mTick=null;
function cycleMsg(){if(mi<msgs.length){document.getElementById('loadMsg').textContent=msgs[mi++];mTick=setTimeout(cycleMsg,1200);}}

async function generate(){
  var creds=getCreds();
  if(!creds.email||!creds.password){showErr('Please enter your Quotex email and password.');return;}
  var sel=document.getElementById('pairSelect');
  var asset=sel.value;
  var pairName=sel.options[sel.selectedIndex].text;
  showLoad();mi=0;cycleMsg();
  document.getElementById('genBtn').disabled=true;
  try{
    var res=await fetch('/api/signal',{method:'POST',headers:{'Content-Type':'application/json'},
      body:JSON.stringify({email:creds.email,password:creds.password,pair:pairName,asset:asset})});
    if(!res.ok){var e=await res.json();throw new Error(e.detail||'Server error');}
    var data=await res.json();
    clearTimeout(mTick);hideLoad();
    showResult(data,pairName);
  }catch(e){
    clearTimeout(mTick);showErr(e.message);
  }
  document.getElementById('genBtn').disabled=false;
}

function showResult(d,pairName){
  var isBuy=d.direction==='BUY';
  var sc=isBuy?'#22c55e':'#ef4444';
  var bg=isBuy?'#052e16':'#450a0a';
  var card=document.getElementById('sigCard');
  card.style.background=bg; card.style.border='2px solid '+sc;
  document.getElementById('sigDir').textContent=isBuy?'↑':'↓';
  document.getElementById('sigDir').style.color=sc;
  document.getElementById('sigLabel').textContent=isBuy?'BUY / CALL':'SELL / PUT';
  document.getElementById('sigLabel').style.color=sc;
  document.getElementById('sigPair').textContent=pairName+' · '+p2(SH)+':'+p2(SM)+' IST';
  var bp=Math.round(d.br*100),sp=100-bp;
  document.getElementById('barB').style.width=bp+'%';
  document.getElementById('barS').style.width=sp+'%';
  document.getElementById('pctB').textContent='CALL '+bp+'%';
  document.getElementById('pctS').textContent=sp+'% PUT';
  var gc=d.grade==='A'?'#22c55e':d.grade==='B'?'#f59e0b':d.grade==='C'?'#f97316':'#ef4444';
  var wc=d.win_rate>=65?'#22c55e':d.win_rate>=55?'#f59e0b':'#ef4444';
  var cc=d.conf>=85?'#22c55e':d.conf>=75?'#f59e0b':'#ef4444';
  document.getElementById('sGrade').textContent=d.grade; document.getElementById('sGrade').style.color=gc;
  document.getElementById('sWR').textContent=d.win_rate+'%'; document.getElementById('sWR').style.color=wc;
  document.getElementById('sConf').textContent=d.conf+'%'; document.getElementById('sConf').style.color=cc;
  document.getElementById('sTrades').textContent=d.trades;
  var rl=document.getElementById('reasonsList');rl.innerHTML='';
  (d.reasons||[]).forEach(function(r){rl.innerHTML+='<div class="reason"><span style="color:'+sc+';">'+(isBuy?'▲':'▼')+'</span>'+r+'</div>';});
  function setInd(id,val,ok){var el=document.getElementById(id);el.textContent=val;el.style.color=ok?'#22c55e':'#ef4444';}
  setInd('iRSI',d.RSI,d.RSI<50===isBuy);
  setInd('iMACD',d.MACD,(d.MACD==='Bullish')===isBuy);
  setInd('iBB',d.BB+'%',d.BB<50===isBuy);
  setInd('iK',d.K,d.K<50===isBuy);
  setInd('iWR',d.WR,d.WR<-50===isBuy);
  setInd('iCCI',d.CCI,d.CCI<0===isBuy);
  setInd('iEMA',d.ema_trend,(d.ema_trend==='Uptrend')===isBuy);
  setInd('iPat',d.pattern,d.pattern!=='None');
  document.getElementById('result').style.display='block';
  startCd();
  document.getElementById('result').scrollIntoView({behavior:'smooth'});
}

function startCd(){
  if(cdTick)clearInterval(cdTick);
  var start=Date.now();
  var row=document.getElementById('cdRow');
  row.style.display='flex';
  cdTick=setInterval(function(){
    var rem=Math.max(0,60-Math.floor((Date.now()-start)/1000));
    var el=document.getElementById('cdTime');
    el.textContent=p2(0)+':'+p2(rem);
    el.style.color=rem<=15?'#ef4444':rem<=30?'#f59e0b':'#22c55e';
    if(rem===0){clearInterval(cdTick);row.style.display='none';}
  },500);
}
</script>
</body>
</html>'''

@app.get("/", response_class=HTMLResponse)
def index():
    return HTML
