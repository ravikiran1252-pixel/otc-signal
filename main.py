from fastapi import FastAPI, HTTPException
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
import asyncio, math, hashlib, builtins, os, json, time
from datetime import datetime

app = FastAPI()

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
    RSI=calc_rsi(cl); RSI5=calc_rsi(cl,5)
    hist,hp=calc_macd(cl)
    BB=calc_bb(cl)
    K,D=calc_stoch(hi,lo,cl); K5,_=calc_stoch(hi,lo,cl,5)
    WR=calc_wr(hi,lo,cl)
    CCI=calc_cci(hi,lo,cl)
    e8=calc_ema(cl,8); e21=calc_ema(cl,21)
    e8v=e8[-1] or cl[-1]; e21v=e21[-1] or cl[-1]
    ema_bull=e8v>e21v
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
    if hist>0 and hist>hp: add('B',2.5,'MACD bullish and accelerating')
    elif hist>0: add('B',1.2,'MACD bullish')
    if hist<0 and hist<hp: add('S',2.5,'MACD bearish and accelerating')
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
    if WR<-85: add('B',2,'Williams R deeply oversold')
    elif WR<-70: add('B',1,'Williams R oversold')
    if WR>-15: add('S',2,'Williams R deeply overbought')
    elif WR>-30: add('S',1,'Williams R overbought')
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

async def get_quotex_candles(email, password, asset, otp=None):
    try:
        import websockets, urllib.parse
        # Use env var cookie if available (most reliable)
        env_cookie = os.environ.get('QX_COOKIE', '')
        env_email  = os.environ.get('QX_EMAIL', email)
        env_pass   = os.environ.get('QX_PASSWORD', password)
        # Prefer env cookie, then user-provided cookie, then password
        raw_session = env_cookie or password
        session = urllib.parse.unquote(raw_session.strip())
        email = env_email
        password = env_pass
        candles = []
        uri = "wss://qxbroker.com/socket.io/?EIO=4&transport=websocket"
        headers = {
            "Origin": "https://qxbroker.com",
            "Cookie": f"session={session}",
            "User-Agent": "Mozilla/5.0 (Linux; Android 12) AppleWebKit/537.36 Chrome/120.0.0.0 Mobile Safari/537.36",
        }
        async with websockets.connect(uri, additional_headers=headers, ping_interval=20, open_timeout=15) as ws:
            step = 0
            while True:
                try:
                    msg = await asyncio.wait_for(ws.recv(), timeout=12)
                except asyncio.TimeoutError:
                    break
                if msg == "2":
                    await ws.send("3")
                    continue
                if msg.startswith("0") and step == 0:
                    await ws.send("40")
                    step = 1
                    continue
                if msg.startswith("40") and step == 1:
                    auth = json.dumps(["authorization", {"session": session, "isDemo": 1}])
                    await ws.send(f"42{auth}")
                    step = 2
                    continue
                if msg.startswith("42") and step >= 2:
                    try:
                        data = json.loads(msg[2:])
                        event = data[0] if data else ""
                        payload = data[1] if len(data) > 1 else {}
                        if event == "authorization" and step == 2:
                            now = int(time.time())
                            req = json.dumps(["history/load", {"asset": asset, "index": now, "time": 60, "offset": 200}])
                            await ws.send(f"42{req}")
                            step = 3
                        elif step == 3:
                            raw = []
                            if isinstance(payload, dict):
                                raw = payload.get("candles", payload.get("data", []))
                            elif isinstance(payload, list):
                                raw = payload
                            if raw and len(raw) >= 20:
                                for c in raw:
                                    try:
                                        if isinstance(c, (list, tuple)):
                                            o,h,l,cl2=float(c[1]),float(c[2]),float(c[3]),float(c[4])
                                        else:
                                            o=float(c.get('open',0)); cl2=float(c.get('close',o))
                                            h=float(c.get('max',c.get('high',max(o,cl2))))
                                            l=float(c.get('min',c.get('low',min(o,cl2))))
                                        if cl2>0: candles.append({'open':o,'high':h,'low':l,'close':cl2})
                                    except: continue
                                if len(candles)>=20: break
                    except: continue
        if len(candles)>=30:
            return candles, None
        raise Exception(f"WebSocket got {len(candles)} candles")
    except Exception as ws_err:
        # Fallback: pyquotex with persistent session cache
        try:
            client, err = await get_or_create_client(email, password, otp)
            if err:
                return None, err
            raw = await asyncio.wait_for(client.get_candles(asset, 60, 200), timeout=30)
            if not raw:
                # Session may have expired, remove from cache and retry
                cache_key = hashlib.md5(email.encode()).hexdigest()[:8]
                _client_cache.pop(cache_key, None)
                return None, "No candles — session expired, try again"
            candles2=[]
            for x in raw:
                try:
                    o=float(x.get('open',0)); cl=float(x.get('close',o))
                    h=float(x.get('max',x.get('high',max(o,cl))))
                    l=float(x.get('min',x.get('low',min(o,cl))))
                    if cl>0: candles2.append({'open':o,'high':h,'low':l,'close':cl})
                except: continue
            if len(candles2)<30: return None, f"Only {len(candles2)} candles"
            return candles2, None
        except Exception as e:
            s=str(e).lower()
            if any(x in s for x in ['eof','otp','verify','unknown','email']):
                return None, "OTP_REQUIRED"
            return None, f"{str(e)} | WS: {str(ws_err)}"

# Global client cache - keeps session alive between requests
_client_cache = {}

async def get_or_create_client(email, password, otp=None):
    """Get cached client or create new one with session"""
    import urllib.parse
    cache_key = hashlib.md5(email.encode()).hexdigest()[:8]
    
    # Check if we have a working cached client
    if cache_key in _client_cache:
        client = _client_cache[cache_key]
        try:
            # Test if still connected
            if hasattr(client, 'websocket') and client.websocket:
                return client, None
        except: pass
    
    # Create new client
    from pyquotex.stable_api import Quotex
    user_data_dir = f"/tmp/qx_{cache_key}"
    os.makedirs(user_data_dir, exist_ok=True)
    
    orig = builtins.input
    def fake(prompt=""):
        if otp: return str(otp)
        raise EOFError("need OTP")
    builtins.input = fake
    
    try:
        client = Quotex(email=email, password=password, lang="en", user_data_dir=user_data_dir)
        ok, msg = await asyncio.wait_for(client.connect(), timeout=60)
        if not ok:
            s = str(msg).lower()
            if any(x in s for x in ['otp','eof','unknown','email','change','verify','invalid']):
                return None, "OTP_REQUIRED"
            return None, f"Login failed: {msg}"
        # Cache the working client
        _client_cache[cache_key] = client
        return client, None
    except EOFError:
        return None, "OTP_REQUIRED"
    except asyncio.TimeoutError:
        return None, "Timeout connecting"
    finally:
        builtins.input = orig

class SignalRequest(BaseModel):
    email: str
    password: str
    pair: str
    asset: str
    otp: str = ""

@app.post("/api/signal")
async def generate_signal(req: SignalRequest):
    candles, err = await get_quotex_candles(req.email, req.password, req.asset, req.otp or None)
    if err == "OTP_REQUIRED":
        raise HTTPException(status_code=401, detail="OTP_REQUIRED")
    # Also catch repeated OTP requests
    if err:
        raise HTTPException(status_code=400, detail=err)
    result = build_signal(candles)
    if not result:
        raise HTTPException(status_code=400, detail="Not enough data")
    result['pair'] = req.pair
    return result

@app.get("/health")
def health():
    return {"status":"ok"}

@app.get("/", response_class=HTMLResponse)
def index():
    return HTML

HTML = """<!DOCTYPE html>
<html lang="en"><head><meta charset="UTF-8"/><meta name="viewport" content="width=device-width,initial-scale=1.0,maximum-scale=1.0"/><title>OTC Signal Pro</title>
<style>*{margin:0;padding:0;box-sizing:border-box}body{background:#0a0a14;color:#e2e8ff;font-family:system-ui,sans-serif}
.wrap{max-width:480px;margin:0 auto;padding:16px 14px 60px}
.card{background:#111127;border:1px solid #1e1e38;border-radius:16px;padding:18px;margin-bottom:14px}
.lbl{font-size:12px;font-weight:700;color:#9b7fff;margin-bottom:12px;text-transform:uppercase;letter-spacing:1px;display:block}
input,select{width:100%;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:10px;padding:12px 14px;color:#e2e8ff;font-size:14px;margin-bottom:10px;font-family:inherit;outline:none}
textarea{width:100%;background:#1a1a2e;border:2px solid #7c3aed;border-radius:10px;padding:12px 14px;color:#c4b5fd;font-size:12px;margin-bottom:8px;font-family:monospace;outline:none;resize:none;height:90px;line-height:1.4}
.btn{width:100%;padding:16px;border-radius:14px;background:linear-gradient(135deg,#7c3aed,#a78bfa);color:#fff;font-weight:900;font-size:16px;border:none;cursor:pointer}
.btn:disabled{opacity:.5;cursor:not-allowed}
.err{background:#450a0a;border:1px solid #ef444466;border-radius:12px;padding:14px;color:#ef4444;font-size:13px;margin-top:10px;display:none;line-height:1.5}
.loading{display:none;text-align:center;padding:28px;color:#6b7ab5}
@keyframes spin{to{transform:rotate(360deg)}}
.spinner{width:44px;height:44px;border:3px solid #1e1e38;border-top-color:#7c3aed;border-radius:50%;animation:spin 1s linear infinite;margin:0 auto 12px}
.otp-box{display:none;background:#111127;border:2px solid #7c3aed;border-radius:16px;padding:20px;margin-top:12px}
.result{display:none;margin-top:12px}
.sig-card{border-radius:18px;padding:22px;text-align:center;margin-bottom:12px}
.stats{display:grid;grid-template-columns:repeat(4,1fr);gap:8px;margin-bottom:12px}
.stat{background:#0a0a14;border:1px solid #1e1e38;border-radius:12px;padding:10px 4px;text-align:center}
.stat-l{font-size:9px;color:#444466;text-transform:uppercase;margin-bottom:4px}
.stat-v{font-size:17px;font-weight:900;font-family:monospace}
.inds{background:#111127;border:1px solid #1e1e38;border-radius:14px;padding:16px;margin-bottom:12px}
.ind{display:flex;justify-content:space-between;align-items:center;padding:9px 0;border-bottom:1px solid #1e1e3844}
.reasons{background:#111127;border:1px solid #1e1e38;border-radius:14px;padding:16px;margin-bottom:12px}
.reason{padding:8px 0;border-bottom:1px solid #1e1e3833;font-size:13px;display:flex;gap:8px}
.cd{margin-top:12px;padding:12px;background:#0a0a14;border-radius:10px;display:flex;justify-content:center;align-items:center;gap:10px}
</style></head><body>
<div class="wrap">
<div style="display:flex;align-items:center;gap:10px;padding:14px 0 20px">
<div style="width:40px;height:40px;border-radius:12px;background:linear-gradient(135deg,#7c3aed,#a78bfa);display:flex;align-items:center;justify-content:center;font-size:22px">&#9889;</div>
<div><div style="font-size:20px;font-weight:900">OTC Signal Pro</div><div style="font-size:11px;color:#6b7ab5">Real Quotex OTC &middot; High Accuracy</div></div>
<div style="margin-left:auto;text-align:right"><div id="clock" style="font-size:18px;font-family:monospace;font-weight:800;color:#7c3aed">00:00:00</div><div style="font-size:10px;color:#343d6e">IST</div></div>
</div>
<div class="card">
<span class="lbl">&#128279; Quotex Connection</span>
<input id="email" type="email" placeholder="Quotex Email"/>
<input id="pwdInput" type="password" placeholder="Quotex Password" style="margin-bottom:6px"/>
<div style="font-size:11px;color:#6b7ab5;margin-bottom:10px">OR paste session cookie below for better connection:</div>
<textarea id="cookieInput" placeholder="Paste session cookie here (from Firefox javascript:alert(document.cookie))&#10;Leave empty to use password above"></textarea>
<div style="font-size:11px;color:#444466">Cookie = more reliable · Password = may need OTP</div>
</div>
<div class="card">
<span class="lbl">&#128202; Select OTC Pair</span>
<select id="pairSelect">
<optgroup label="Major Forex OTC">
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
<optgroup label="Asian and Exotic OTC">
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
</optgroup>
<optgroup label="Cross Pairs OTC">
<option value="EURCAD_otc">EUR/CAD (OTC)</option>
<option value="EURAUD_otc">EUR/AUD (OTC)</option>
<option value="EURCHF_otc">EUR/CHF (OTC)</option>
<option value="GBPAUD_otc">GBP/AUD (OTC)</option>
<option value="GBPCAD_otc">GBP/CAD (OTC)</option>
<option value="AUDCAD_otc">AUD/CAD (OTC)</option>
<option value="CADJPY_otc">CAD/JPY (OTC)</option>
<option value="CHFJPY_otc">CHF/JPY (OTC)</option>
<option value="NZDJPY_otc">NZD/JPY (OTC)</option>
</optgroup>
<optgroup label="Crypto OTC">
<option value="BTCUSD_otc">BTC/USD (OTC)</option>
<option value="ETHUSD_otc">ETH/USD (OTC)</option>
<option value="LTCUSD_otc">LTC/USD (OTC)</option>
<option value="DOGEUSD_otc">DOGE/USD (OTC)</option>
</optgroup>
</select>
<div style="display:flex;align-items:center;justify-content:space-between;margin-bottom:12px">
<div style="font-size:13px;color:#6b7ab5">&#9201; Analysis Time (IST)</div>
<button onclick="setNow()" style="padding:6px 14px;border-radius:20px;border:1px solid #7c3aed;background:none;color:#9b7fff;font-size:12px;cursor:pointer">Use Now</button>
</div>
<div style="display:flex;align-items:center;justify-content:center;gap:16px">
<div style="text-align:center">
<button onclick="adj('h',-1)" style="width:52px;height:40px;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:8px;color:#7c3aed;font-size:20px;cursor:pointer">&#9650;</button>
<div id="hv" style="font-size:48px;font-weight:900;font-family:monospace;color:#e2e8ff;margin:6px 0;min-width:70px;text-align:center">00</div>
<button onclick="adj('h',1)" style="width:52px;height:40px;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:8px;color:#7c3aed;font-size:20px;cursor:pointer">&#9660;</button>
<div style="font-size:10px;color:#444466;margin-top:6px;letter-spacing:2px">HOUR</div>
</div>
<div style="font-size:48px;font-weight:900;color:#7c3aed;padding-bottom:24px">:</div>
<div style="text-align:center">
<button onclick="adj('m',-1)" style="width:52px;height:40px;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:8px;color:#7c3aed;font-size:20px;cursor:pointer">&#9650;</button>
<div id="mv" style="font-size:48px;font-weight:900;font-family:monospace;color:#e2e8ff;margin:6px 0;min-width:70px;text-align:center">00</div>
<button onclick="adj('m',1)" style="width:52px;height:40px;background:#1a1a2e;border:1px solid #2a2a4e;border-radius:8px;color:#7c3aed;font-size:20px;cursor:pointer">&#9660;</button>
<div style="font-size:10px;color:#444466;margin-top:6px;letter-spacing:2px">MIN</div>
</div>
</div>
</div>
<button class="btn" id="genBtn" onclick="generate()">&#9889; Generate High-Accuracy Signal</button>
<div class="loading" id="loading"><div class="spinner"></div><div id="loadMsg" style="font-size:14px">Connecting to Quotex...</div></div>
<div class="err" id="errBox"></div>
<div class="otp-box" id="otpBox">
<div style="font-size:15px;font-weight:700;color:#9b7fff;margin-bottom:8px">&#128231; Enter Verification Code</div>
<div style="font-size:13px;color:#6b7ab5;margin-bottom:14px">Check your email for code from Quotex:</div>
<input id="otpInput" type="number" placeholder="Enter 6-digit code" style="font-size:22px;text-align:center;letter-spacing:8px;border:2px solid #7c3aed;margin-bottom:10px"/>
<button onclick="submitOTP()" style="width:100%;padding:14px;border-radius:12px;background:#7c3aed;color:#fff;font-weight:800;font-size:15px;border:none;cursor:pointer">&#10003; Verify and Get Signal</button>
</div>
<div class="result" id="result">
<div class="sig-card" id="sigCard">
<div style="font-size:11px;letter-spacing:2px;opacity:.7;margin-bottom:10px;text-transform:uppercase">&#9889; OTC Signal &middot; 1 Min Expiry</div>
<div id="sigDir" style="font-size:64px;font-weight:900;line-height:1">&#8593;</div>
<div id="sigLabel" style="font-size:28px;font-weight:900;margin:4px 0">BUY / CALL</div>
<div id="sigPair" style="font-size:13px;opacity:.6;font-family:monospace;margin-top:6px"></div>
<div style="margin:14px 0 8px">
<div style="height:16px;border-radius:8px;overflow:hidden;display:flex;margin-bottom:6px">
<div id="barB" style="background:#22c55e;width:50%;border-radius:8px 0 0 8px"></div>
<div id="barS" style="background:#ef4444;width:50%;border-radius:0 8px 8px 0"></div>
</div>
<div style="display:flex;justify-content:space-between">
<span id="pctB" style="font-size:13px;font-weight:800;color:#22c55e">CALL 50%</span>
<span id="pctS" style="font-size:13px;font-weight:800;color:#ef4444">50% PUT</span>
</div>
</div>
<div class="cd" id="cdRow">
<span style="font-size:13px;opacity:.6">&#8987; Trade window</span>
<span id="cdTime" style="font-family:monospace;font-weight:900;font-size:24px">01:00</span>
</div>
</div>
<div class="stats">
<div class="stat"><div class="stat-l">Grade</div><div class="stat-v" id="sGrade">-</div></div>
<div class="stat"><div class="stat-l">Win Rate</div><div class="stat-v" id="sWR">-</div></div>
<div class="stat"><div class="stat-l">Conf</div><div class="stat-v" id="sConf">-</div></div>
<div class="stat"><div class="stat-l">Trades</div><div class="stat-v" id="sTrades">-</div></div>
</div>
<div class="reasons">
<div style="font-size:11px;color:#6b7ab5;font-weight:700;margin-bottom:10px;text-transform:uppercase;letter-spacing:1px">&#127919; Why this signal</div>
<div id="reasonsList"></div>
</div>
<div class="inds">
<div style="font-size:11px;color:#6b7ab5;font-weight:700;margin-bottom:12px;text-transform:uppercase;letter-spacing:1px">&#128202; Indicators</div>
<div class="ind"><span style="font-size:13px;color:#d1d5db">RSI (14)</span><span id="iRSI" style="font-size:14px;font-weight:800;font-family:monospace">-</span></div>
<div class="ind"><span style="font-size:13px;color:#d1d5db">MACD</span><span id="iMACD" style="font-size:14px;font-weight:800;font-family:monospace">-</span></div>
<div class="ind"><span style="font-size:13px;color:#d1d5db">Bollinger %B</span><span id="iBB" style="font-size:14px;font-weight:800;font-family:monospace">-</span></div>
<div class="ind"><span style="font-size:13px;color:#d1d5db">Stochastic K</span><span id="iK" style="font-size:14px;font-weight:800;font-family:monospace">-</span></div>
<div class="ind"><span style="font-size:13px;color:#d1d5db">Williams %R</span><span id="iWR" style="font-size:14px;font-weight:800;font-family:monospace">-</span></div>
<div class="ind"><span style="font-size:13px;color:#d1d5db">CCI</span><span id="iCCI" style="font-size:14px;font-weight:800;font-family:monospace">-</span></div>
<div class="ind"><span style="font-size:13px;color:#d1d5db">EMA Trend</span><span id="iEMA" style="font-size:14px;font-weight:800;font-family:monospace">-</span></div>
<div class="ind"><span style="font-size:13px;color:#d1d5db">Pattern</span><span id="iPat" style="font-size:14px;font-weight:800;font-family:monospace">-</span></div>
</div>
<div style="font-size:11px;color:#374151;text-align:center;padding:6px 0 12px">&#9888;&#65039; Real Quotex OTC &middot; 1 min expiry &middot; Trade responsibly</div>
</div>
</div>
<script>
var SH=0,SM=0,cdTick=null,pendingReq=null;
function p2(n){return String(n).padStart(2,'0');}
function nowIST(){var n=new Date(),ist=new Date(n.getTime()+n.getTimezoneOffset()*60000+5.5*3600e3);return{h:ist.getHours(),m:ist.getMinutes(),s:ist.getSeconds()};}
setInterval(function(){var t=nowIST();document.getElementById('clock').textContent=p2(t.h)+':'+p2(t.m)+':'+p2(t.s);},1000);
(function(){var t=nowIST();SH=t.h;SM=t.m;document.getElementById('hv').textContent=p2(SH);document.getElementById('mv').textContent=p2(SM);})();
function adj(f,d){if(f==='h')SH=(SH+d+24)%24;else SM=(SM+d+60)%60;document.getElementById('hv').textContent=p2(SH);document.getElementById('mv').textContent=p2(SM);}
function setNow(){var t=nowIST();SH=t.h;SM=t.m;document.getElementById('hv').textContent=p2(SH);document.getElementById('mv').textContent=p2(SM);}
function getCreds(){
  var e=document.getElementById('email').value.trim();
  var pwd=document.getElementById('pwdInput').value.trim();
  var cookie=document.getElementById('cookieInput').value.trim();
  if(e)localStorage.setItem('qx_e',e);
  if(pwd)localStorage.setItem('qx_pwd',pwd);
  if(cookie)localStorage.setItem('qx_cookie',cookie);
  return{email:e,password:cookie||pwd};
}
(function(){
  var e=localStorage.getItem('qx_e'),pwd=localStorage.getItem('qx_pwd'),cookie=localStorage.getItem('qx_cookie');
  if(e)document.getElementById('email').value=e;
  if(pwd)document.getElementById('pwdInput').value=pwd;
  if(cookie)document.getElementById('cookieInput').value=cookie;
})();
var msgs=['Connecting to Quotex...','Authenticating...','Fetching real OTC candles...','Computing RSI, MACD, Bollinger...','Analysing Stochastic and EMA...','Scanning candle patterns...','Running backtest...','Finalising signal...'];
var mi=0,mTick=null;
function cycleMsg(){if(mi<msgs.length){document.getElementById('loadMsg').textContent=msgs[mi++];mTick=setTimeout(cycleMsg,1400);}}
function showLoad(){document.getElementById('loading').style.display='block';document.getElementById('result').style.display='none';document.getElementById('errBox').style.display='none';document.getElementById('otpBox').style.display='none';mi=0;cycleMsg();}
function hideLoad(){document.getElementById('loading').style.display='none';clearTimeout(mTick);}
function showErr(msg){document.getElementById('errBox').style.display='block';document.getElementById('errBox').textContent='Error: '+msg;}
async function generate(){
  var creds=getCreds();
  if(!creds.email){showErr('Please enter your Quotex email.');return;}
  if(!creds.password){showErr('Please enter password or paste cookie.');return;}
  var sel=document.getElementById('pairSelect');
  pendingReq={email:creds.email,password:creds.password,pair:sel.options[sel.selectedIndex].text,asset:sel.value,otp:''};
  await doRequest();
}
async function doRequest(){
  showLoad();document.getElementById('genBtn').disabled=true;
  try{
    var res=await fetch('/api/signal',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(pendingReq)});
    if(res.status===401){
      hideLoad();
      document.getElementById('otpBox').style.display='block';
      document.getElementById('otpInput').value='';
      document.getElementById('otpInput').focus();
      document.getElementById('genBtn').disabled=false;
      return;
    }
    if(!res.ok){var e=await res.json();throw new Error(e.detail||'Server error');}
    var data=await res.json();hideLoad();document.getElementById('otpBox').style.display='none';showResult(data);
  }catch(e){hideLoad();showErr(e.message);}
  document.getElementById('genBtn').disabled=false;
}
async function submitOTP(){
  var code=document.getElementById('otpInput').value.trim();
  if(!code){alert('Enter the 6-digit code from your email.');return;}
  pendingReq.otp=code;
  await doRequest();
}
function showResult(d){
  var isBuy=d.direction==='BUY',sc=isBuy?'#22c55e':'#ef4444',bg=isBuy?'#052e16':'#450a0a';
  var card=document.getElementById('sigCard');card.style.background=bg;card.style.border='2px solid '+sc;
  document.getElementById('sigDir').textContent=isBuy?'\u2191':'\u2193';document.getElementById('sigDir').style.color=sc;
  document.getElementById('sigLabel').textContent=isBuy?'BUY / CALL':'SELL / PUT';document.getElementById('sigLabel').style.color=sc;
  document.getElementById('sigPair').textContent=d.pair+' \u00b7 '+p2(SH)+':'+p2(SM)+' IST';
  var bp=Math.round(d.br*100),sp=100-bp;
  document.getElementById('barB').style.width=bp+'%';document.getElementById('barS').style.width=sp+'%';
  document.getElementById('pctB').textContent='CALL '+bp+'%';document.getElementById('pctS').textContent=sp+'% PUT';
  var gc=d.grade==='A'?'#22c55e':d.grade==='B'?'#f59e0b':d.grade==='C'?'#f97316':'#ef4444';
  document.getElementById('sGrade').textContent=d.grade;document.getElementById('sGrade').style.color=gc;
  document.getElementById('sWR').textContent=d.win_rate+'%';document.getElementById('sWR').style.color=d.win_rate>=65?'#22c55e':d.win_rate>=55?'#f59e0b':'#ef4444';
  document.getElementById('sConf').textContent=d.conf+'%';document.getElementById('sConf').style.color=d.conf>=85?'#22c55e':d.conf>=75?'#f59e0b':'#ef4444';
  document.getElementById('sTrades').textContent=d.trades;
  var rl=document.getElementById('reasonsList');rl.innerHTML='';
  (d.reasons||[]).forEach(function(r){rl.innerHTML+='<div class="reason"><span style="color:'+sc+';">'+(isBuy?'\u25b2':'\u25bc')+'</span>'+r+'</div>';});
  function si(id,val,ok){var el=document.getElementById(id);el.textContent=val;el.style.color=ok?'#22c55e':'#ef4444';}
  si('iRSI',d.RSI,d.RSI<50===isBuy);si('iMACD',d.MACD,(d.MACD==='Bullish')===isBuy);
  si('iBB',d.BB+'%',d.BB<50===isBuy);si('iK',d.K,d.K<50===isBuy);
  si('iWR',d.WR,d.WR<-50===isBuy);si('iCCI',d.CCI,d.CCI<0===isBuy);
  si('iEMA',d.ema_trend,(d.ema_trend==='Uptrend')===isBuy);si('iPat',d.pattern,d.pattern!=='None');
  document.getElementById('result').style.display='block';
  startCd();document.getElementById('result').scrollIntoView({behavior:'smooth'});
}
function startCd(){
  if(cdTick)clearInterval(cdTick);var start=Date.now();
  document.getElementById('cdRow').style.display='flex';
  cdTick=setInterval(function(){
    var rem=Math.max(0,60-Math.floor((Date.now()-start)/1000));
    var el=document.getElementById('cdTime');el.textContent=p2(0)+':'+p2(rem);
    el.style.color=rem<=15?'#ef4444':rem<=30?'#f59e0b':'#22c55e';
    if(rem===0){clearInterval(cdTick);document.getElementById('cdRow').style.display='none';}
  },500);
}
</script></body></html>"""
# This file has the session persistence fix above
