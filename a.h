#define l0 5   //clang-13 -Os -ok a.c -w -fno-builtin -funsigned-char -fno-unwind-tables -mavx2 -mfma -mpclmul -mbmi2 -nostdlib -s
#define n0 (1LL<<l0)//gcc -Os -ok a.c -w -fno-builtin -funsigned-char -fno-unwind-tables -mavx2 -mfma -mpclmul -mbmi2 -nostdlib -s -flax-vector-conversions
#define ZU static U//_abc.efg.ij.lmno..r.t...x..
#define ZV static V//$ABCD.FGHI....NOPQR.TUVW...
#define AV(n) __attribute((vector_size(1<<n),aligned(1)))
typedef char X AV(4),d2 AV(l0-2),V AV(l0);typedef unsigned u,d1 AV(l0-1),Vu AV(l0);typedef unsigned long long U,U4 AV(4),VU AV(l0),UU AV(l0+1),(*Uf)(U),(*UF)(U,U),(*Uns)(u,char*);
typedef int Vi AV(l0);typedef float Ve AV(l0);ZU w_(U,...),W,M[30],D[32],S[1<<11],*c=S+(1<<11);static Ve e0;static VU U0;static Vu u0;ZV Z,
I0={0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31};//,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};
#define Zc static char
Zc*_P,*PP="+- *    %     &and  |or   <    >     =      ~ .! @at    ?find #take  _drop  ^cut   ,$\n+- *sqr %sqrt &flip |flop <asc >desc =group ~ .! @first ?nub  #count _floor ^order ,$",B=32;
#define c(z) _(cc[1<<20];ss=c;z;ns(s-c,c))
#define cn(g,z) U(g,z,cc,in,ss)
#define uf unsigned f
#define Ua U a
#define Ux U x
#define Va V a
#define Vb V b
#define Vv V v
#define e(z) ({z;})
#define g(g,z)  U(g,z,uf,Ux)
#define VF(g,z) V(g,z,Va,Vb)
#define AN(f,s,x...) __attribute((naked))U f(x){asm(s);}
#define bi(f) __builtin_##f
#define B(v,a,b) ((v)&(a)|~(v)&(b))
#define F(g,z) U(g,z,Ua,Ux)
#define U(g,z,x...) ZU g(x){return   e(z);}
#define V(g,z,x...) ZV g(x){return(V)e(z);}
#if __arm64
#define z(f) bi(neon_v##f##q_v)  //VF(mu,X2(o(max)(X(a),X(b),48)))
#define S(f,i) static AN(f,"mov x16,"#i"\nldr x1,[sp]\nldr x2,[sp,#8]\nsvc #0\nret",Ux,...)
ZU A=1;S(w_,4)S(_w,3)S(f_,5)S(_f,6)S(_l,338)S(_k,1)S(m_,197)AN(ut,"mrs x0,cntvct_el0\nret")g(X9,-f^bi(neon_vmull_p64)(x,-1L))X x8={1,2,4,8,16,32,64,128,1,2,4,8,16,32,64,128};
U(b_,0,Va)//X z=z(padd)(x0&x8,x1&x8,48);i(2,z=z(padd)(z,z,48))*(u*)&z,Va)
U(B_,0,Va)V(sq,a,Va)VF(lg,a)VF(mg,a)VF(lu,a)VF(mu,a)F(cmp,0)VF(gg,a) //Vv={};i(2,asm("fcmla.4s %0,%1,%2,0\nfcmla.4s %0,%1,%2,90":"+w"(X(v)):"w"(X(a)),"w"(X(b))))v)VF(A0,a)
#else
#define z(f) bi(ia32_##f##256)
#define S(f,i) static AN(f,"mov %rcx,%r10;mov $"#i",%rax;syscall;ret",Ux,...)
ZU A;S(_w,0)S(w_,1)S(f_,2)S(_f,3)S(_l,4)S(_k,60)S(m_,9)AN(ut,"rdtsc;shl $32,%rdx;or %rdx,%rax;ret")AN(_start,"lea 8(%rsp),%rsi;call main")g(X9,-f^bi(ia32_pclmulqdq128)((U4){x},~(U4){},0)[0])
F(cmp,bi(ia32_pext_di)(x,a))U(b_,z(pmovmskb)(a),Va)U(B_,z(movmskps)(a),Ve a)
V(sq,z(sqrtps)(a),Ve a)VF(lg,z(pminub)(a,b))VF(mg,z(pmaxub)(a,b))VF(lu,z(pminud)(a,b))VF(mu,z(pmaxud)(a,b))
VF(gg,z(vfmaddsubps)((Ve)a,(Ve)z(pshufd)(b,0xa0),(Ve)z(pshufd)(a,0xb1)*(Ve)z(pshufd)(b,0xf5)))VF(A0,B(I0/16==b/16,z(pshufb)(a,b),z(pshufb)(z(permvarsi)(a,4+I0/4),b)))
#endif
#define s0(o,b) Va=b;I(Nx,a=o(a,O))a=o(a,s(1,a,b));a=o(a,s(2,a,b));a=o(a,s(4,a,b));a=o(a,s(8,a,b));a=o(a,s(16,a,b));a[31]
#define S0(o,b) Va=b;_N(Vv=O;      v=o(v,s(1,v,b));v=o(v,s(2,v,b));v=o(v,s(4,v,b));v=o(v,s(8,v,b));v=o(v,s(16,v,b));a=o(v,a[31]+Z))
#define s2(o,b) Vu a=b;I(Nx,a=o(a,O))                              a=o(a,s(4,a,b));a=o(a,s(8,a,b));a=o(a,s(16,a,b));a[7]
#define S2(o,b) Vu a=b;_N(Vv=O;                                    v=o(v,s(4,v,b));v=o(v,s(8,v,b));v=o(v,s(16,v,b));a=o(v,u0+a[7]))
#define bI(z)    R(1,nx,I(Nx,rc=B_(e(z))))
#define bC(z)    R(1,nx,I(Nx,ru=b_(e(z))))
#define _(z)   e(typeof(e(z))y=e(z);_r(x);y)
#define _a(z)  e(typeof(e(z))y=e(z);_r(a);y)
#define a(z,y) e(typeof(z)a=z;y)
#define b(z)   a(z,(64>a?1LL<<a:0)-1)
#define f(g,z) U(g,z,Ux)
#define i(a,z) {unsigned _n=a;ii=-1;W(++i<_n){z;}}
#define j(a,z) {unsigned _n=a;ij=-1;W(++j<_n){z;}}
#define l(a,z) e(typeof(z)e_=z;(a)<e_?(a):e_)
#define m(a,z) e(typeof(z)e_=z;(a)>e_?(a):e_)
#define n(z)   i(nx,z)
#define r(z,y) e(typeof(z)r=z;y;r)
#define t(t,x) ((U)(t)<<61|(x))
#define x(z,y) e(typeof(z)x=z;y)
#define A(z) (26>(z)-97)
#define C(t,x) bi(convertvector)(x,t)
#define D(z) (10>(z)-48)
#define G(g,z) U(g,z,uf,Ua,Ux)
#define I(n,z) i(n0-1+(n)>>l0,z)
#define K(n,z)   R(0,n,i(nr,rU=e(z)))
#define L(t,n,z) R(t,n,I(Nr,rV=(V)e(z)))
#define N(z) L(Tx,nx,z)
#define P(b,z) if(e(b))return e(z);
#define Q(z...) P(96==(z),96)
#define R(t,n,z)  r(T(t,n),z)
#define W(z) while(e(z))
#define _N(z)     _(N(z))
#define _K(z)     _(K(nx,z))
#define _R(t,n,z) _(R(t,n,z))
#define _T(t,z)   _(L(t,nx,z))
#define v1($,z) f($,tx?e(Q(x=$(  v_(x)))_v(x)):Tx?e(z):f1($,  x))
#define V1($,z) F($,tx?e(Q(x=$(a,v_(x)))_v(x)):Tx?e(z):fr($,a,x))
#define _W(z) ii=nx;W(i--){z;}
#define Ui(g,z) U(g,z,ii)
#define Us(g,z) U(g,z,ss)
#define Zs(g,a,x...) static char*g(x){return e(a);}
#define Zf(f,x...) static Uf f[]={x};
#define ZF(f,x...) static UF f[]={x};
#define Zns(f,x...) static Uns f[]={x};

#define ds(s,n,x) (char*)dn((U)(s),n,x)
#define nb(z) bi(popcountll)(z)
#define ib(z) bi(ctzll)(z)
#define lb(z) (64-bi(clzll)(z))
#define a3(t) $3(f-1,t##a+t##x,t##a-t##x,t##a*t##x)
#define b3(a,x) $3(f-7,(a)<x,(a)>x,(a)==x)
#define N6(z) i(63+nx>>6,z)
#define nS(g,z) U(g,z,in,ss)
#define ve(g,z) U(g,z,ee)
#define IB(a) r(ib(a),a&=a-1)
#define Qx(f) Ux=f(s);Q(x)
#define Qs(e,s) P(e,err((U)__func__,(U)s))
#define o xc
#define O ((V*)x)[i]
#define _o sx[(int)i-1]
#define _O ((V*)(x-1))[i]
#define O_ ((V*)(x+1))[i]
#define QZ e(Qz(1)0)
#define Nx tn(Tx,nx)
#define Nr x(r,Nx)
#define Uu     C(VU,((d1*)x)[i])
#define _u (Ve)C(Vu,((UU*)x)[i])
#define u_ (Ve)C(Vu,((UU*)x)[i]>>32)
#define Ix (Vu)Cx
#define Ex (Ve)Cx
#define xx (*UX)
#define mx sx[-6]
#define RH UR[-1]
#define si s[i]
#define xb ((u*)x)[i/32]
#define Ca x(a,tx?Z+cx:Cx)
#define Ia x(a,tx?u0+ux:Ix)
#define Ea x(a,tx?e0+ex:Ex)
#define ca x(a,cx)
#define AU r_(aU)
#define XU r_(xU)
#define UR ((U*)r)
#define UA ((U*)a)
#define UX ((U*)x)
#define aU ((U*)a)[i]
#define rU ((U*)r)[i]
#define xU ((U*)x)[i]
#define nr ((u*)r)[-1]
#define na ((u*)a)[-1]
#define nx ((u*)x)[-1]
#define ac (sa)[i]
#define xc sx[i]
#define rc (sr)[i]
#define ru ((u*)r)[i]
#define xu ((u*)x)[i]
#define rV ((V*)r)[i]
#define aV ((V*)a)[i]
#define sr (char*)r
#define sa (char*)a
#define sx ((char*)x)
#define rx ((u*)x)[-2]
#define Tx sx[-5]
#define Bx (1==Tx)
#define tx (x>>61)
#define Cx O
#define ss char*s
#define cc char c
#define ee float e
#define ii unsigned i
#define ij unsigned j
#define ik unsigned k
#define im unsigned m
#define in unsigned n
#define it unsigned t
#define Ur U r
#define Un U n
#define oo w_(2,"oo\n",3)
#define x_ sx[nx-1]
#define Zh static short
#define Zu static u
Zu N=1<<31,L[]={3,0,0,2,2,3};Ui(tb,t(1,i))Ui(tc,t(2,i))Ui(ti,t(3,i))g(tn,1==f?7+x>>3:x<<L[f])static float E[]={1,10,100,1e3,1e4,1e5,1e6,1e7,1e8,1e9};
f(q0,10>x-48)f(qa,26>x-97)U(sc,ii=0;W(si&&c!=si)++i;i,ss,cc)U(nsc,i(n,P(c==si,i))n,in,ss,cc)ve(ue,*(u*)&e)ve(te,t(4,ue(e)))ve(eq,1-1e-6<e&&1+1e-6>e)U(dn,I(n,rV=O)r+n,Ur,Un,Ux)
Ui(g_,in=95<<23;ij=N&i;j>>2|b(29)&m(n,i)-n)Ui(_g,in=95<<23;ij=i&b(29);j?N&i<<2|j+n:0)f(G_,g_(x)|g_(x>>32)<<30)f(_G,_g(x)|_g(x>>30)<<32)VF(ag,a+b)VF(ai,(Vu)a+(Vu)b)VF(ae,(Ve)a+(Ve)b)
Zs(ws,w_(2,(U)s,sc(s,0));s,ss)f(wc,w_(2,(U)&x,1))F(err,if(a)ws(sa);wc(58);ws(sx);wc(10);96)V(_b,Va=1<<I0%8&A0(U0+x,I0/8);1&0<a,Ux)V(s,Vv=I0-c;B(v<n0,A0(a,v),b),cc,Va,Vb)
#define pt (px||tx)
#define ix ux
#define ia ua
#define cx (char)x
#define px (128>x)
#define lx L[tx]
#define Lx L[Tx]
#define ux (unsigned)x
#define ex a(ux,*(float*)&a)
#define au ((u*)a)[i]
#define pa x(a,px)
#define ta x(a,tx)
#define ua x(a,ux)
#define ea x(a,ex)
#define Ta x(a,Tx)
#define Qz(e) Qs(e,"nyi")
#define Qr(e) Qs(e,"rank")
#define Qt(e) Qs(e,"type")
#define Qn(e) Qs(e,"count")
#define Qd(e) Qs(e,"domain")
#define nU ((u*)xU)[-1]
#define sU ((char*)xU)
#define $3(z,a,b,c)       e(unsigned y=z;!y?e(a):1==y?e(b):e(c))
#define $4(z,a,b,c,d)     e(unsigned y=z;!y?e(a):1==y?e(b):2==y?e(c):e(d))
#define $5(z,a,b,c,d,x)   e(unsigned y=z;!y?e(a):1==y?e(b):2==y?e(c):3==y?e(d):e(x))
#define $6(z,a,b,c,d,x,f) e(unsigned y=z;!y?e(a):1==y?e(b):2==y?e(c):3==y?e(d):4==y?e(x):e(f))
#define IF(z,a) if(z){a;}else//V(Wu,i(8,wi(a[i]))a,Vu a)
ZU r_(),_r(),T(u,U),wx();f(wi,r(x,cc[21];ss=c+20;*s=10;do*--s=48+x%10;W(x/=10);w_(2,s,c+21-s)))f(Y,wx(r_(x));x)V(wv,i(32,wi(a[i]))a,Va)
nS(ns,R(2,n,dn(r,n,(U)s)))Us(xs,ns(sc(s,0),s))U(v,_r(*_);*_=x,U*_,Ux)U(OO,w_(2,"OO\n",3);_k(0))F(k2,K(2,i?x:a))f(z4,3+lb(x)>>2)
g(_M,W-=1<<f;xx=M[f];M[f]=x)Ui(M_,Ux=M[i];x?(W+=1<<i,M[i]=xx,x):30>i?_M(i,M_(i+1))+(2*n0<<i):OO())f(_r,pt?x:65535&rx?(--rx,x):e(if(!Tx)n(_r(xU))_M(mx,x-n0);x))f(r_,pt?x:65535&++rx?x:OO())
g(a_,ii=f;t(Tx,$6(Tx,XU,1&xb>>i%32,xc,xu,xu,G_(xU))))f(v_,R(tx,1,*UR=5==tx?_G(x):x))f(_v,_(a_(0,x)))U(f1,_K($(a_(i,x))),Uf $,Ux)U(fr,_K($(a,a_(i,x))),UF $,Ua,Ux)
F(a0,ii=0;$4(Tx-1,xc&=254,xc=a,xu=a,xU=a);x)g(T,ii=x?lb(n0+tn(f,x)-1)-l0-1:0;r(M_(i)+n0,RH=i<<16|f<<24|x<<32))v1(_t,3>Tx?_N(mg(B+Z,O+(26>O-65&32))):_T(Tx-1,5>Tx?C(Vi,Ex):(Vi)sq(_u*_u+u_*u_)))
g(t_,P(tx,t(f,$4(f-2,ux,ux,ue((int)ux),G_(4>tx?(u)t_(4,x):ux))))x=f-1>Tx?t_(f-1,x):x;_T(f,$4(f-2,_b(xu),(V)C(Vu,((d2*)x)[i]),(V)C(Ve,(Vi)Ix),(V)Uu)))
f(gi,in=ux;R(3,n,i(n,ru=i)))f(ge,in=ux;R(4,n,i(n,ru=ue((i+.5)/n))))f(gE,in=ux;K(n,Ua=ue((i+.5)/n)<<32;R(5,n,i(n,rU=a|ue((i+.5)/n)))))f(R_,if(!Tx)n(XU)x)
U(jk,R_(R(Tx,n-i,dn(r,(U)n-i<<Lx,x+(i<<Lx)))),int i,in,Ux)F(pr,a0(a,jk(-1,nx-1,x)))F(dx,Un=Nx;ij=n%n0;a=dn(a,n-=j,x);i(j,ac=sx[n+i])a+j)F(V_,Qz(Bx)_a(R_(_R(Ta,na+nx,dx(dn(r,x(a,Nx),a),x)))))
g(fs,Lx?$3(f,S2(lu,~Z),S2(mu,Z),4>Tx?e(S2(ai,Z)):e(S2(ae,Z))):$3(f,S0(lg,~Z),S0(mg,Z),S0(ag,Z)))g(bs,Qz(2>f)fs(2,t_(2,x)))f(sb,ii=nx/64;if(nx%64)xU&=b(nx%64);r(0,N6(r+=nb(xU))))
g(fo,in=32>>Lx;ii=nx/n;cc=Nx%32;if(c)O=B(c>I0,O,f?Z:~Z);Lx?$3(f,s2(lu,~Z),s2(mu,Z),4>Tx?e(s2(ai,Z)):e(s2(ae,Z))):$3(f,s0(lg,~Z),s0(mg,Z),s0(ag,Z)))g(bo,in=sb(x);$3(f,nx==n,0<n,n))
f(n_,_R(3,sb(x),u*s=(u*)sr;N6(Ua=xU;j(nb(a),*s++=64*i+IB(a)))))F(_V,_a(_(K(na,jk(au,i+1<na?((u*)a)[i+1]:nx,x)))))Ui(c1,v_(tc(i)))g(vc,V_(x,c1(f)))
f(Ss,Ua=pr(x_,x);cc=*sa;a=x(a,_(bC(c==O)));a=n_(a);x=_V(a,x);n(a(xU,sa)[--nU]=0)x)g(sS,in=f?nx:0;ij;n(Qt(a(xU,ta||2!=Ta))n+=j=nU)_R(a(xx,Ta),n,Ua=r;n(if(a=dx(a,xU),f)*sa++=f)))
nS(i$,48>*s?-i$(n-1,s+1):r(0,i(n,r=si%16+10*r)))nS(e$,P(48>*s,N|e$(n-1,s+1))ii=nsc(n,s,101);ue(n>i?i$(i,s)*E[s[i+1]%16]:n>(i=nsc(n,s,46))?(n-=i+1,(i$(i,s)*E[n]+i$(n,s+i+1))/E[n]):i$(n,s)))F(c2,r(T(2,2),*UR=a|x<<8))
Ui($i,N&i?1|$i(-i)<<4:r(0LL,do r=r<<4|4+i%10;W(i/=10)))ve($e,0>e?1|$e(-e)<<4:1e-9>e?4:1e9<e?52:1>e?3|$e(1/e)<<4:eq((int)e/e)?$i(e):x($i(.5+1e4*e),in=z4(x)-4<<2;2<<n|b(n)&x|(~b(n)&x)<<4))
f($a,x=$3(tx-3,$i(ux),$e(ex),x=_G(x);Ua=$e(ex);x>>=32;a|(14|$e(ex)<<4)<<4*z4(a));Vv=44+(15&A0(*(V*)&x,I0/2)>>I0%2*4);ns(z4(x),(char*)&v))
f($,px?c1(_P[x]):tx?lx?$a(x):$(v_(x)):$3(Tx-1,_T(2,48+_b(xu)),10>fo(0,x)?_N(48+O):x,f1($,x)))g(nz,Vv=1==tx?ux?~Z:Z:3>tx?cx+Z:5>tx?(V)(u0+ux):(V)(U0+_G(x));L(tx,f,v))
f(vk,Q(x)it=a(xx,ta);P(!t,x)n(P(t!=a(xU,ta),x))Qz(2!=Lx)_T(t,_u))f(kv,_K(a_(i,x)))
v1(uu,P(4<Tx,_T(4,u_))x=4>Tx?t_(4,x):x;_T(5,Uu<<32))v1(no,_($3(Tx-1,N(~Ix),bC(33>O),bI(0==Ix))))V1(j4,Qt(4<Tx)x=4>Tx?t_(4,x):x;_N(sq(Ex)))V1(d_,Qz(3!=Tx)_N(Ix/ua))V1(_d,Qz(3!=Tx)_N(Ix%ua))
F(C_,a=ta?v_(a):a;x=tx?v_(x):x;Ta*Tx?V_(Tx>Ta?t_(Tx,a):a,Ta>Tx?t_(Ta,x):x):V_(kv(a),kv(x)))F(Ll,C_(a,v_(x)))F(_C,Qr(ta||tx)Qt(3!=Ta||Bx)_V(a,x))
F(mm,QZ)F(No,QZ)F(Dt,QZ)f(dt,QZ)f(ff,P(tx?:Tx,uu(x))QZ)G(NN,Qr(!ta)P(tx,nz(ua,x))Qn(ua>nx)_(jk(f?ua:0,f?nx:ua,x)))f(rr,Qz(2!=Tx)_R(2,nx,n(rc=sx[nx-1-i])))f(qr,Qr(1)0)f(ty,t(3,pt?tx:Tx))
g(LMS,QZ)F(N_,Qt(3!=ta)_d(ua,x))ZU io(u,U),qz(),z_(U*,U),jJ(u,U,U),a2(u,U,U),O3(u,U,U);
G(e2,_K(a2(f,ta?a:AU,tx?x:XU)))f(or,QZ)f(ou,QZ)f(og,QZ)f(_n,_(ti(nx)))F(A_,Qt(3!=tx)a_(ux,a))F(_A,QZ)f(h_,QZ)f(_h,QZ)
G(AA,P(2==ta,ij=a-101;Qd(3==f)j4(f+2*j,x))ta?f&&3==ta?d_(a,x):jJ(3,f?te(1/ea):a,x):qz(a)?z_(&x,a):!tx&&(!Tx||4==Tx)?mm(a,x):(!f?A_:_A)(a,x))
Us(lf,it[36];!_l((U)s,(U)t)&&1&t[A?1:6]>>15?t[A?24:12]:0)Us(_1,in=lf(s);Qs(!n,s);ii=f_((U)s,0);R(2,n,_w(i,(U)sr,n);_f(i)))Us(_0,Qx(_1)Qd(10!=x_)ii=59==sx[nx-2];if(i)I(--nx,O=mg(B+Z,O))Ss(x))
G(aA,Qz(Bx||2>f-5)7>f?N($4(Tx-2,a3(C),(V)a3(I),(V)(4>f?a3(E):Ea/Ex),Ve v=ta?(Ve)(U0+_G(a)):Ea;(V)$3(f-1,v+Ex,v-Ex,(Ve)gg((V)v,(V)Ex)))):3>Tx?bC(b3(Ca,Cx)):bI(b3(Ia,Ix)))
G(aa,5>f?4>tx?t(tx,a3(i)):te(4>f?a3(e):ea/ex):t(7>f?tx:1,$3(f-5,l(ua,ux),m(ua,ux),b3(ua,ux))))
G(jJ,ii=ta?:Ta;ij=tx?:Tx;P(i*j,it=m(4>f?2:4==f?4:0,m(i,j));a=t>i?t_(t,a):a;x=t>j?t_(t,x):x;tx?ta?5>tx?aa(f,a,x):_v(jJ(f,a,v_(x))):2==f||4==f||2>f-7?QZ:jJ(f,x,a):_a(_(aA(f,a,x))))e2(f,a,x))
g(bp,Un=0;_R(1,nx,N6(Ua=xU;n|=a<<1;rU=f?$6(f-5,a&n,a|n,~a&n,a&~n,0,a^n):n;n=a>>63)))g(fp,P(f,Ua=xx;Ur=x-(1<<Lx);Un=RH;RH=UX[-1];x=a0(a,aA(f,x,r));RH=n;x)pr(0,x))
G(p3,P(!f--,Bx?bp(a,x):_(fp(a,x)))P(18==a,Qr(Tx)sS(0,x))P(10==a,Qt(!Bx)Un=0;_R(1,nx,N6(rU=n=X9(n>>63,xU))))Qd(!pa||1!=a&&1<a-5)a-=5;f?(Bx?bs:fs)(a,x):_(t(m(2>a?0:3,Tx),(Bx?bo:fo)(a,x))))
G(i3,$3(f,P(2==Tx,x=vc(0,x);_((ua?_1:_0)(sx)))QZ,io(a,x),QZ))G(c3,$3(f,QZ,Qr(Tx)sS(a,x),Qr(!Tx)nx?Ss(vc(a,x)):T(0,0)))
Zf(g,ff,qr,qr,qr,qr,no,dt,gi,qr,qr,gE,_t,ge,v_,$)Zf(G,ff,rr,qr,qr,og,no,dt,n_,_v,ou,_n,_t,or,v_,$)g(tz,Ua=ut();i(f,Q(_r(z_(0,x))))_(ti(((ut()-a)/(A?2.4e4:2.4e6)-f/2e5))))
g(a1,P(20>f,Qr(px&&17>f-1)$6(f,x,3==tx?++x:jJ(1,tc(1),x),3==tx?--x:jJ(1,tc(-1),x),jJ(3,r_(x),x),j4(-1,x),(tx?g:G)[f-5](x)))LMS(f-20,x))ZF(J,No,Dt,N_,0,0,0,0,_C,C_,0)
G(a2,P(20>f,P(!f,Ll(a,x))P(pa||px,Qr(13!=f)a1(a,x))10>f?jJ(f,a,x):4>f-13?(15>f?AA:NN)(1&f-1,a,x):J[f-10](a,x))Qz(29>f)Qr(tx)f-=29;!f&&9>a-11?vk(f1(G[a-5],x)):$3(ta-2,c3,i3,pa?p3:O3)(f,a,x))
U(z_,Ua=x;x=xx;Ur;_W(ij=o%32;Q(r=*c=$4(o/32,--c;22>j?a_(j,a):ti(j-22),a1(j,r),a2(j,r,*++c),U*a=_&&23<j?_+j-24:D+j;r_(64==_o?--i,v(a,r):(--c,*a)))))*c++,U*_,Ux)
f(ext,Ua=N(B<O);_a(_R(2,(char)-fo(2,a),ss=sr;i(7+nx>>3,*(U*)s=cmp(aU,xU);s+=nb(aU)/8))))
#define H 14
ZU fd(u,U),zs(),os(),ls(),ys(),tk(),pk();ZU r,a,f;Zh b[1<<H];f(h0,f=0;i(2<<H-l0,((V*)b)[i]=~Z)r=T(0,1<<H-1);a=T(3,nr);nr=0)
U(h1,Ux=b(8*n)&*(U*)s;ii=b(H)&2654435769*(x^x>>32)>>32-H;IF(b[i]>>15,b[i]=nr;UR[nr++]=x)f+=rU!=x;i=b[i];++au;i,in,ss)f(h2,wi(f);n(xU=ns(7+lb(xU)>>3,(char*)&xU))k2(x,a))
g(io,Qt(2!=Tx)h0(0);ss=sx;char*t=s+nx;*t=0;W(s<t){Ua=b_(A(*(V*)s));a=s+64>t?b(t-s)&a:a;a^=a<<1;i(nb(a)/2,ij=IB(a);h1(IB(a)-j,s+j))s+=a?ib(a):64;}_(h2(r)))
