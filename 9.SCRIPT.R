library(readxl)
data=read_excel("D:\\datapenelitian.xlsx")
data=data.frame(x1=data$TPAK,x2=data$PDRB,x3=data$TPT,x4=data$JPMS,x5=data$UHH,x6=data$PPK,x7=data$IK,
                y=data$PE,u=data$Latitude,v=data$Longitude)
View(data) 
str(data)

#estimasi regresi linear berganda
regglo1=lm(y~x1+x2+x3+x4+x5+x6+x7,data=data)

#Uji F
anova(regglo1)

#Uji t
summary(regglo1)
AIC(regglo1)

#Uji Asumsi Klasik
##Uji Normalitas
library(nortest)
lillie.test(regglo1$residuals)
##Uji Multikolinearitas
library(car)
vif(regglo1)
##Uji Autokorelasi
durbinWatsonTest(regglo1)

##Uji Heteroskedastisitas
library(lmtest)
bptest(regglo1, data=data)

#GWR
library(spgwr)
library(sp) 
library(GWmodel)
library(maptools) 
library(foreign) 
library(lattice) 
library(zoo) 
library(lmtest) 
library(ape) 
library(Matrix) 
library(mvtnorm) 
library(emulator)

#jarak euclidean
ll<-as.matrix(data[9:10]) #koordinat
ll
options(max.print = 1000000)
dij<-dist(ll, method = "euclidean")
print(as.matrix(dij))

#FIXED GAUSSIAN
#bandwidth
coords<-cbind(data$u,data$v)
fixgauss<-gwr.sel(y~x1+x2+x3+x4+x5+x6+x7, data = data,adapt=FALSE, coords = coords,gweight=gwr.Gauss)
print(fixgauss)
#estimasi parameter
gwr.fixgauss<-gwr(y~x1+x2+x3+x4+x5+x6+x7,data=data,bandwidth=fixgauss,coords = coords,hatmatrix=TRUE,gweight=gwr.Gauss)
print(gwr.fixgauss)

#FIXED BISQUARE
#bandwidth
fixbisquare<-gwr.sel(y~x1+x2+x3+x4+x5+x6+x7,data=data,adapt=FALSE,coords=coords,gweight=gwr.bisquare)
print(fixbisquare)
#estimasi parameter
gwr.fixbisquare=gwr(y~x1+x2+x3+x4+x5+x6+x7,data=data,bandwidth=fixbisquare,coords=coords,hatmatrix=TRUE,gweight=gwr.bisquare)
gwr.fixbisquare

#FIXED TRICUBE
#bandwidth
fixtricube=gwr.sel(y~x1+x2+x3+x4+x5+x6+x7,data=data,adapt= FALSE,coords=coords,gweight=gwr.tricube) 
#estimasi parameter
gwr.fixtricube=gwr(y~x1+x2+x3+x4+x5+x6+x7,data=data,bandwidth=fixtricube,coords=coords,hatmatrix=TRUE, gweight=gwr.tricube)
gwr.fixtricube

#GAUSSIAN Addaptive
#bandwidth
adaptgauss=gwr.sel(y~x1+x2+x3+x4+x5+x6+x7,data=data,adapt
                   =TRUE,coords=coords,gweight=gwr.Gauss) 
#estimasi parameter
gwr.adaptgauss=gwr(y~x1+x2+x3+x4+x5+x6+x7,data=data,adapt=adaptgauss,
                   coords=coords,hatmatrix=TRUE,gweight=gwr.Gauss)
gwr.adaptgauss 

#BISQUARE ADAPT
#bandwidth 
adaptbisquare=gwr.sel(y~x1+x2+x3+x4+x5+x6+x7, data=data,adapt=TRUE,coords=coords,gweight=gwr.bisquare) 
#estimasi parameter
gwr.adaptbisquare=gwr(y~x1+x2+x3+x4+x5+x6+x7,data=data,adapt=adaptbisquare,coords=coords,hatmatrix=TRUE,gweight=gwr.bisquare)
gwr.adaptbisquare

# TRICUBE ADAPTIVE
#bandwidth
adapttricube=gwr.sel(y~x1+x2+x3+x4+x5+x6+x7,data=data,adapt=TRUE,coords=coords,gweight=gwr.tricube) 
#estimasi parameter
gwr.adapttricube=gwr(y~x1+x2+x3+x4+x5+x6+x7,data=data,adapt=adapttricube,coords=coords,hatmatrix=TRUE,gweight=gwr.tricube)
gwr.adapttricube

##GWR FUNGSI PEMBOBOT FIXED GAUSSIAN##
#bandwidth
fixgauss=gwr.sel(y~x1+x2+x3+x4+x5+x6+x7, data = data,adapt=FALSE, coords = coords,gweight=gwr.Gauss) 
#Estimasi Parameter
gwr1=gwr(y~x1+x2+x3+x4+x5+x6+x7,data=data,bandwidth=fixgauss,coords = coords,hatmatrix=TRUE,gweight=gwr.Gauss)
gwr1
gwr1$bandwidth #menampilkan nilai bandwidth

#Menampilkan hasil estimasi parameter
Intersep=gwr1$SDF$`(Intercept)`
Intersep
TPAK=gwr1$SDF$x1
TPAK
PDRB=gwr1$SDF$x2
PDRB
TPT=gwr1$SDF$x3
TPT
JPMS=gwr1$SDF$x4 
JPMS
UHH=gwr1$SDF$x5 
UHH
PPK=gwr1$SDF$x6
PPK
IK=gwr1$SDF$x7
IK
Kabupaten_Kota=data$data.Kabupaten_Kota 
Bandwidth=gwr1$bandwidth 
R2=gwr1$SDF$localR2 #nilai r2 lokal

#Uji Kecocokan Model
BFC02.gwr.test(gwr1)

#Merubah data ke Spasial Titik Data Frame
data.sp.GWR=data
coordinates(data.sp.GWR)<-9:10 
class(data.sp.GWR)  
head(data.sp.GWR)

hasil<-gwr.basic(y~x1+x2+x3+x4+x5+x6+x7,data = data.sp.GWR,bw=fixgauss, 
                 kernel = "gaussian")
hasil

#P-Value
p.value.GWR=gwr.t.adjust(hasil)$results$p
p.value.GWR

#Menampilkan t-hitung
t_TPAK=gwr1$SDF$x1/gwr1$SDF$x1_se
t_PDRB=gwr1$SDF$x2/gwr1$SDF$x2_se
t_TPT=gwr1$SDF$x3/gwr1$SDF$x3_se 
t_JPMS=gwr1$SDF$x4/gwr1$SDF$x4_se
t_UHH=gwr1$SDF$x5/gwr1$SDF$x5_se
t_PPK=gwr1$SDF$x6/gwr1$SDF$x6_se
t_IK=gwr1$SDF$x7/gwr1$SDF$x7_se

t_TPAK
t_PDRB
t_TPT
t_JPMS
t_UHH
t_PPK
t_IK

#Prediksi/yhat
GWRfixpred=gwr1$SDF$pred
GWRfixpred

#Uji Variabilitas Spasial
LMZ.F3GWR.test(gwr1)

#Model MGWR 
library(car)
library(lmtest)
gwr.fixgauss=gwr(y~x1+x2+x3+x4+x5+x6+x7,data=data,bandwidth=fixgauss,coords=cbind(data$u,data$v),
                 hatmatrix=TRUE,gweight=gwr.Gauss)
bw<-gwr.fixgauss$bandwidth 
#Variabel pada data
y=as.matrix(data$y)
lat=as.matrix(data$u)
lon=as.matrix(data$v)
xl=as.matrix(cbind(data$x4))
xg=as.matrix(cbind(data$x1,data$x2,data$x3,data$x5,data$x6,data$x7))
x=as.matrix(cbind(xl,xg))
ng=ncol(xg)
nl=ncol(xl)
n=length(y)
I=diag(1,n,n) 
W=matrix(0,n,n) 
d=matrix(0,n,n) 
for (i in 1:n)
{ 
  for (j in 1:n)
  { 
    d[i,j]=sqrt((lat[i,1]-lat[j,1])^2+(lon[i,1]-lon[j,1])^2)
    
    W[i,j]=exp(-1/2*((d[i,j]/bw)^2))
  } 
} 
#Mencari estimasi parameter global MGWR
beta.l=matrix(0,nl,n)
Sl=matrix(0,n,n) 
for (i in 1:n)
{ 
  Sl[i,]=((xl[i,]%*%(solve((t(xl)%*%diag(W[,i]))%*%xl)))%*%t(xl))%*%diag(W[,i])
}
beta.g=((((solve(((t(xg)%*%t(I-Sl))%*%(I-Sl))%*%xg))%*%t(xg))%*%t(I-Sl))%*%(I-Sl))%*%y
View(beta.g)
#Mencari estimasi parameter lokal MGWR
for (i in 1:n)
{ 
  beta.l[,i]=((solve((t(xl)%*%diag(W[,i]))%*%xl)%*%t(xl)%*%diag(W[,i]))%*%(y-(xg%*%beta.g)))
} 
Sg=(xg%*%solve(t(xg)%*%xg))%*%t(xg)
S=Sl+((((((I-Sl)%*%xg)%*%solve(((t(xg)%*%t(I-Sl))%*%(I-Sl))%*%xg))%*%t(xg))%*%t(I-Sl))%*%(I-Sl))
S
y.hat=S%*%y
residual=(I-S)%*%y
H=(x%*%solve(t(x)%*%x))%*%t(x)
beta.l
y.hat
View(beta.l)
View(y.hat)

#Uji Model MGWR
v=c(0,0) 
u=c(0,0) 
r=c(0,0) 
t=c(0,0) 
for (i in 1:2)
{ 
  v[1]=tr(((I-H)-(t(I-S)%*%(I-S)))^i)
  u[i]=tr((t(I-S)%*%(I-S))^i)
  r[i]=tr((t(I-Sl)%*%(I-Sl)-t(I-S)%*%(I-S))^i)
  t[i]=tr((t(I-Sg)%*%(I-Sg)-t(I-S)%*%(I-S))^i)
} 

#Uji Kesesuaian Model MGWR
F3=as.vector((((t(y)%*%((I-H)-(t(I-S)%*%(I-S))))%*%y)/v[1])/((((t(y)%*%t(I-S))%*%(I-S))%*%y)/u[1]))
df1.1=(v[1]^2/v[2])
df2=(u[1]^2)/u[2]

#Uji Serentak Parameter Global MGWR
F4=as.vector((((t(y)%*%(((t(I-Sl)%*%(I-Sl))-(t(I-S)%*%(I-S)))))%*%y)/r[1])/((((t(y)%*%t(I-S))%*%(I-S))%*%y)/u[1]))
df1.2=(r[1]^2/r[2])
#Uji Serentak Parameter Lokal MGWR
F5=as.vector((((t(y)%*%(((t(I-Sg)%*%(I-Sg))-(t(I-S)%*%(I-S)))))%*%y)/t[1])/((((t(y)%*%t(I-S))%*%(I-S))%*%y)/u[1]))
df1.3=(t[1]^2/t[2])
df=(u[1]^2/t[2])

F=as.vector(rbind(F3,F4,F5)) 
df1=c(df1.1,df1.2,df1.3) 

p.value=as.vector(matrix(0,3,1)) 
for (i in 1:3)
{ 
  p.value[i]=1-(pf(F[i], df1=df1[i], df2=df2)) 
} 
Uji.Serentak=cbind(F,df1,df2,p.value) 
Uji.Serentak
ftabel1=qf(.05, df1=21.86387, df2=35.69383) 
ftabel1
ftabel2=qf(.05, df1=21.69680, df2=35.69383) 
ftabel2
ftabel3=qf(.05, df1=21.86387, df2=35.69383) 
ftabel3
#Pengujian parameter secara parsial
#Pengujian parameter global secara parsial
G=((solve(((t(xg)%*%t(I-Sl))%*%(I-Sl))%*%xg)%*%t(xg))%*%t(I-Sl))%*%(I-Sl)
gkk=diag(G%*%t(G))
t.g=as.vector(matrix(0,ng,1)) 
p.val=as.vector(matrix(0,ng,1)) 
sigma=as.vector(sqrt(((((t(y)%*%t(I-S))%*%(I-S))%*%y)/n)))
for (i in 1:ng)
{ 
  t.g[i]=beta.g[i]/(sigma*sqrt(gkk[i])) 
} 
Uji.Parsial.Global=cbind(t.g,df,p.val)
Uji.Parsial.Global
ttabel=qt(.025, 496.5161)
ttabel
sigma=as.vector(sqrt(((((t(y)%*%t(I-S))%*%(I-S))%*%y)/n)))
t.hit.l=matrix(0,nl,n)
pvalue=matrix(0,nl,n)
#Pengujian parameter lokal secara parsial
ringkasan=matrix(0,n,2*nl)
for (i in 1:n)
{ 
  
  M=((((solve(((t(xl)%*%diag(W[,i]))%*%xl)))%*%t(xl))%*%diag(W[,i]))%*%(I-(xg%*%G)))
  m=diag(M%*%t(M))
  m=as.matrix(m)
  for (j in 1:nl)
  { 
    t.hit.l[j,i]=beta.l[j,i]/(sigma*(sqrt(m[j,])))
    pvalue[j,i]=pt(t.hit.l[j,i],df=df2,lower.tail=TRUE)
  } 
  ringkasan[i,]=t(cbind(t.hit.l[,i],pvalue[,i]))
} 
ringkasan #nilai t, nilai pval, dst#
ttabel=qt(.025, 496.5161)
ttabel
AICc=(2*n*log(sigma))+(n*log(2*pi))+((n*((n+tr(S)))/(n-2-tr(S))))
AIC=(2*n*log(sigma))+(n*log(2*pi))+n+tr(S) 
resid=y-y.hat 
sigu=(t(resid))%*%resid
ym=y-mean(y)
rsqrt1=sigu 
rsqrt2=t(ym)%*%ym
rsqrt=1-(rsqrt1/rsqrt2) #r-squared#
rsqrt1=rsqrt1/(n-ng-nl) 
rsqrt2=rsqrt2/(n-1) 
rbar=1-(rsqrt1/rsqrt2) #rbar-squared# 
AICc
AIC
rsqrt
rbar

