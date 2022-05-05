#=======================
#Rozkład GPD. Metoda POT
#=======================

#------------ LATO ------------
# Rozklad nadwyzek i analiza dopasowania


XL=dataL
quantile(XL,c(0.85,0.9,0.95))
ismev::gpd.fitrange(XL,quantile(XL,0.85),quantile(XL,0.95))

uL=26
uL

#wykresy rozrzutu z zaznaczonym progiem u 
par(mfrow=c(2,1))
plot(XL,type="h")
abline(h=uL,lwd=2,col='red')   

#nadwyzki nad prog u
Y_L=XL[XL>uL]-uL
plot(Y_L,type='h')

#estymujemy parametry rozkladu GPD
#gpd(dane,u) - podajemy prog lub liczbe nadwyzek
library(ismev)


fitGPD_L=ismev::gpd.fit(XL,uL)    
xi_L=fitGPD_L$mle[[2]]
beta_L=fitGPD_L$mle[[1]]
xi_L; beta_L


#wykresy diagnostyczne 
ismev::gpd.diag(fitGPD_L)




# Poziom zwrotu (return level)

fitGPD_L=gpd(XL,uL)
xi_L=fitGPD_L$par.est[[1]]
beta_L=fitGPD_L$par.est[[2]]
xi_L; beta_L


Nu_L=length(Y_L)   #licznosc nadwyzek
N_L=length(XL)    #licznosc probki

k20_L <- 20*92*24*6
k50_L <- 50*92*24*6

XL20=uL+((k20_L*Nu_L/N_L)^xi_L-1)*beta_L/xi_L #jesli przyjmiemy, ze xi rozne od zera 
XL20

XL50=uL+((k50_L*Nu_L/N_L)^xi_L-1)*beta_L/xi_L #jesli przyjmiemy, ze xi rozne od zera 
XL50

evir::riskmeasures(fitGPD_L,1-(1/k20_L))[2]
evir::riskmeasures(fitGPD_L,1-(1/k50_L))[2]

#------------ WIOSNA ------------

X_W=dataW 

quantile(X_W,c(0.85,0.9,0.95))
ismev::gpd.fitrange(X_W,quantile(X_W,0.85),quantile(X_W,0.95))

uW=20
uW


Y_W=X_W[X_W>uW]-uW

# Poziom zwrotu (return level)

fitGPD_W=gpd(X_W,uW)
xi_W=fitGPD_W$par.est[[1]]
beta_W=fitGPD_W$par.est[[2]]
xi_W; beta_W


Nu_W=length(Y_W)   #licznosc nadwyzek
N_W=length(X_W)    #licznosc probki

k20_W <- 20*92*24*6
k50_W <- 50*92*24*6
X_W20=uW+((k20_W*Nu_W/N_W)^xi_W-1)*beta_W/xi_W #jesli przyjmiemy, ze xi rozne od zera 
X_W20


X_W50=uW+((k50_W*Nu_W/N_W)^xi_W-1)*beta_W/xi_W #jesli przyjmiemy, ze xi rozne od zera 
X_W50



#------------ JESIEŃ ------------

X_J=dataJ 

quantile(X_J,c(0.85,0.9,0.95,0.99))
ismev::gpd.fitrange(X_J,quantile(X_J,0.85),quantile(X_J,0.99))

uJ=19

Y_J=X_J[X_J>uJ]-uJ

par(mfrow=c(2,1))
plot(X_J,type="h")
abline(h=uJ,lwd=2,col='red')   

plot(Y_J,type='h')


# Poziom zwrotu (return level)

fitGPD_J=gpd(X_J,uJ)
xi_J=fitGPD_J$par.est[[1]]
beta_J=fitGPD_J$par.est[[2]]
xi_J; beta_J


Nu_J=length(Y_J)   #licznosc nadwyzek
N_J=length(X_J)    #licznosc probki

k20_J <- 20*91*24*6
k50_J <- 50*91*24*6

X_J20=uJ+((k20_J*Nu_J/N_J)^xi_J-1)*beta_J/xi_J #jesli przyjmiemy, ze xi rozne od zera 
X_J20

X_J50=uJ+((k50_J*Nu_J/N_J)^xi_J-1)*beta_J/xi_J #jesli przyjmiemy, ze xi rozne od zera 
X_J50



#------------ ZIMA ------------

X_Z=dataZ 

quantile(X_Z,c(0.85,0.9,0.95))
ismev::gpd.fitrange(X_Z,quantile(X_Z,0.85),quantile(X_Z,0.95))

uZ=7

Y_Z=X_Z[X_Z>uZ]-uZ
Y_Z


# Poziom zwrotu (return level)

fitGPD_Z=gpd(X_Z,uZ)
xi_Z=fitGPD_Z$par.est[[1]]
beta_Z=fitGPD_Z$par.est[[2]]
xi_Z; beta_Z


Nu_Z=length(Y_Z)   #licznosc nadwyzek
N_Z=length(X_Z)    #licznosc probki

k20_Z <- 20*91*24*6
k50_Z <- 50*91*24*6

X_Z20=uZ+((k20_Z*Nu_Z/N_Z)^xi_Z-1)*beta_Z/xi_Z #jesli przyjmiemy, ze xi rozne od zera 
X_Z20


X_Z50=uZ+((k50_Z*Nu_Z/N_Z)^xi_Z-1)*beta_Z/xi_Z #jesli przyjmiemy, ze xi rozne od zera 
X_Z50