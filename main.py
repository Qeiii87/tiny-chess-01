import os,json,urllib.request as u
V=[0,100,320,330,500,900,0]
D=[33,31,18,14,-14,-18,-31,-33,1,-1,16,-16,15,17,-15,-17]
SL=D[8:]
B=[0]*128;S=1;C=0;E=-1;K=[4,116];N=0;LIM=4e4;I=9**9
Ab=Exception
O=lambda m:B[m>>7&127]
def sq(n):return chr(97+(n&7))+str((n>>4)+1)
def uci(m):
 f=m&127;t=m>>7&127;p=m>>14
 return sq(f)+sq(t)+('qrbn'[5-p] if p else '')
def att(s,w):
 p=1+6*(w<0)
 for i,o in enumerate(D):
  t=s+o
  if not t&136 and B[t]==p+(1 if i<8 else 5):return 1
 for o in (-15*w,-17*w):
  t=s+o
  if not t&136 and B[t]==p:return 1
 for o in SL:
  t=s+o
  while not t&136:
   q=B[t]
   if q:
    if q==p+4 or q==p+3-(o in D[12:]):return 1
    break
   t+=o
 return 0
def gen():
 m=[];d=16*S;pr=7*(S>0)
 for s in range(128):
  p=B[s]
  if not p or (p<7)!=(S>0):continue
  a=p if p<7 else p-6
  if a==1:
   t=s+d
   if not t&136 and not B[t]:
    m.append(s|t<<7|(5<<14 if t>>4==pr else 0))
    if s>>4==6-5*(S>0):
     x=s+2*d
     if not B[x]:m.append(s|x<<7)
   for x in (s+d-1,s+d+1):
    if x&136:continue
    q=B[x]
    if (q and (q<7)!=(S>0)) or x==E:m.append(s|x<<7|(5<<14 if x>>4==pr else 0))
  elif a==2 or a==6:
   for o in D[:8] if a==2 else D[8:16]:
    t=s+o
    if not t&136 and (not B[t] or (B[t]<7)!=(S>0)):m.append(s|t<<7)
   if a==6:
    for b,di in ((1,1),(2,-1),(4,1),(8,-1)):
     kf=4+112*(b>3);kt=kf+2*di;rf=kf+3*di if di>0 else kf-4
     e1=kf+di;e3=kf+3*di if di<0 else 127
     if C&b and not(B[e1] or B[kt] or B[e3]) and B[rf]==4+6*(S<0) and not(att(kf,-S)|att(e1,-S)|att(kt,-S)):m.append(kf|kt<<7)
  else:
   ds=D[12:] if a==3 else D[8:12] if a==4 else SL
   for o in ds:
    t=s+o
    while not(t&136):
     q=B[t]
     if q:
      if (q<7)!=(S>0):m.append(s|t<<7)
      break
     m.append(s|t<<7);t+=o
 return m
def make(m):
 global S,C,E
 f=m&127;t=m>>7&127;p=m>>14
 pc=B[f];cap=B[t]
 B[t]=p+6*(S<0) if p else pc
 B[f]=0
 if pc%6==1:
  if t==E and not cap:B[t-16*S]=0
  E=(f+t)//2 if abs(t-f)==32 else -1
 else:E=-1
 if pc%6==0:
  K[S<0]=t
  if t-f==2:B[t-1]=B[t+1];B[t+1]=0
  elif f-t==2:B[t+1]=B[t-2];B[t-2]=0
  C&=~(3<<2*(S<0))
 for q,b in zip((0,7,112,119),(2,1,8,4)):
  if f==q or t==q:C&=~b
 S=-S
def snap():return B[:],S,C,E,K[:]
def rest(x):
 global S,C,E
 B[:],S,C,E,K[:]=x
def ev():
 s=0
 for q in range(128):
  p=B[q]
  if not p:continue
  a=p if p<7 else p-6;w=1 if p<7 else -1
  v=V[a];f=q&7;r=q>>4
  if w<0:r=7-r
  d=abs(f-3.5)+abs(r-3.5)
  if a==1:v+=r*4+int('00366300'[f])
  else:v+=(7-d)*[0,0,4,3,0,2,-3][a]
  s+=v if w>0 else -v
 return s*S
def sr(d,a,b,pl):
 global N
 N+=1
 if N>LIM:raise Ab
 if d<=0:
  if att(K[S<0],-S):ms=gen()
  else:
   x=ev()
   if x>=b:return b
   if x>a:a=x
   ms=[m for m in gen() if B[m>>7&127]]
 else:ms=gen()
 ms.sort(key=O,reverse=1)
 best=-I
 for m in ms:
  x=snap();make(m)
  if att(K[S>0],S):rest(x);continue
  sc=-sr(d-1,-b,-a,pl+1)
  rest(x)
  if sc>best:best=sc
  if sc>a:a=sc
  if a>=b:break
 if d<=0:
  if best==-I and att(K[S<0],-S):return -100000+pl
  return a
 if best==-I:return -100000+pl if att(K[S<0],-S) else 0
 return best
def root(legal):
 global N
 ms=[m for m in gen() if uci(m) in legal]
 ms.sort(key=O,reverse=1)
 bm=ms[0];N=0
 for D in range(1,7):
  try:
   sc=[]
   for m in ms:
    x=snap();make(m)
    if att(K[S>0],S):rest(x);sc.append((-I,m));continue
    sc.append((-sr(D-1,-I,I,0),m))
    rest(x)
   ms=[m for _,m in sorted(sc,reverse=1)]
   bm=ms[0]
  except Ab:break
 return uci(bm)
def setfen(f):
 global S,C,E
 p=f.split();B[:]=[0]*128
 i=0
 for ch in p[0].replace('/',''):
  if ch.isdigit():i+=int(ch)
  else:
   v='PNBRQK'.index(ch.upper())+1+6*(ch.islower())
   s=(7-i//8)*16+i%8;B[s]=v
   if v%6==0:K[v//7]=s
   i+=1
 S=1-2*(p[1]=='b')
 C=0
 for c,b in zip('KQkq',(1,2,4,8)):
  if c in p[2]:C|=b
 E=ord(p[3][0])-97+16*(ord(p[3][1])-49) if p[3]!='-' else -1
U=os.getenv('MAKEFUN_ENV_URL','')
def post(r,o):
 return json.load(u.urlopen(u.Request(U+r,data=json.dumps(o).encode())))
while U:
 g=post('/game',{})
 while g.get('status')=='your_move':
  setfen(g['fen'])
  g=post('/move',{'game':g['game'],'move':root(g['legal_moves'])})
 if g.get('status')=='done':break
