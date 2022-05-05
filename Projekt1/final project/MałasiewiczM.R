
library(tidyr)
library(gamlss)
library(maps)

coord233_alt=(read.csv2(file="coord233_alt.csv",sep=",", quote="\"\""))
Psz <- coord233_alt[coord233_alt$place=="PSZENNO",]
path_to_files<- "D:/Studia/MMAD/sem6/Zdarzenia ekstremalne/Zdarzenia ekstremalne/temp.stations-all"
L <- as.list(list.files(path=path_to_files))
L <- paste0(path_to_files,"\\",L)
data0 <- lapply(L,read.csv)

poland <- map('world','poland', fill=T, col='gray')
points(Psz[c("lon","lat")],pch=19,col=2)	


wiosna <- c()
zima <- c()
jesien <- c()
lato<- c()

for(i in 0:10){
  for(j in 1:12){
    if(j>2 && j<6){
      wiosna <-c(wiosna,j+(12*i))
    }
    else if(j>5 && j<9){
      lato <-c(lato,j+(12*i))
    }
    else if(j>8 && j<12){
      jesien <-c(jesien,j+(12*i))
    }
    else{
      zima<-c(zima,j+(12*i))
    }
  }
}




max10_wiosna <- c()
datetime_wiosna <- c()
max10_lato <- c()
datetime_lato <- c()
max10_jesien<- c()
datetime_jesien <- c()
max10_zima <- c()
datetime_zima <- c()

for(i in 1:33){
  max10_wiosna <- c(max10_wiosna,data0[[wiosna[i]]]$X250160090)
  datetime_wiosna <- c(datetime_wiosna,as.character(data0[[wiosna[i]]]$datetime))
  max10_lato <- c(max10_lato,data0[[lato[i]]]$X250160090)
  datetime_lato <- c(datetime_lato,as.character(data0[[lato[i]]]$datetime))
  max10_jesien <- c(max10_jesien,data0[[jesien[i]]]$X250160090)
  datetime_jesien <- c(datetime_jesien,as.character(data0[[jesien[i]]]$datetime))
  max10_zima <- c(max10_zima,data0[[zima[i]]]$X250160090)
  datetime_zima <- c(datetime_zima,as.character(data0[[zima[i]]]$datetime))
  
}


#===== przeksztalcenie kolumny datetime_lato

#zapisanie w formacie tylko Date
max10_lato <- data.frame(date=as.Date(datetime_lato),max10_lato=max10_lato)
head(max10_lato)
tail(max10_lato)
rownames(max10_lato) <- c()

max10_lato <- separate(max10_lato,date,c("year","mth","day"), convert=TRUE)
head(max10_lato)

max10_lato <- data.frame(datetime=datetime_lato,max10_lato)


max10_lato<-na.omit(max10_lato)

max10_lato$max10_lato[118831]<-NA

max10_lato<-na.omit(max10_lato)

hist(max10_lato$max10_lato, prob=TRUE)


max10_wiosna <- data.frame(date=as.Date(datetime_wiosna),max10=max10_wiosna)
head(max10_wiosna)
tail(max10_wiosna)
rownames(max10_wiosna) <- c()

max10_wiosna <- separate(max10_wiosna,date,c("year","mth","day"), convert=TRUE)
head(max10_wiosna)

max10_wiosna <- data.frame(datetime=datetime_wiosna,max10_wiosna)
max10_wiosna<-na.omit(max10_wiosna)



max10_jesien <- data.frame(date=as.Date(datetime_jesien),max10=max10_jesien)
head(max10_jesien)
tail(max10_jesien)
rownames(max10_jesien) <- c()

max10_jesien <- separate(max10_jesien,date,c("year","mth","day"), convert=TRUE)
head(max10_jesien)

max10_jesien <- data.frame(datetime=datetime_jesien,max10_jesien)
max10_jesien<-na.omit(max10_jesien)

#=======================
# GAMLSS
#=======================

#---------LATO ------
t1 <- Sys.time()
fit <- fitDist(max10_lato$max10,type="realline")
t2 <- Sys.time()
t2-t1

#obejrzyjmy wyniki estymacji

#--- Jaki rozklad ma najmniejsze AIC? 
#Dostaniemy tez  skrot rozkladu, potrzebny dalej do np. gestosci dGT(x,parametry)
fit$family  #np. "GT"            "Generalized t"


#--- ,,dopasowane rozklady'' posortowane wedlug malejacej wartosci AIC
fit$fits    

#--- skroty nazw  parametrow ,,najlepszego rozkladu'' i wartosci parametrow
fit$parameters  

mu <- fit$mu
sigma <- fit$sigma
nu <- fit$nu
tau <- fit$tau



layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), heights = c(12, 10), widths = c(2, 2))

#--- histogram-gestosc
hist(max10_lato$max10_lato, prob=TRUE,xlab=NA, ylab="Gęstość", main="Histogram maksimów 10-minutowych dla miesięcy letnich")
curve(dSHASH(x,mu,sigma,nu,tau),add=T,col=2)

#--- wykres kwantyl-kwantyl
alpha=ppoints(100)

kwantyle_teo <- qSHASH(alpha,mu,sigma,nu,tau)
kwantyle_emp <- quantile(max10_lato$max10_lato,alpha,na.rm=TRUE)

plot(kwantyle_emp,kwantyle_teo, main="Q-Q", xlab="Kwantyle empiryczne", ylab="Kwantyle teoretyczne")
abline(a=0,b=1,col=2)

#do wykresow diagnostycznych mozna wykorzystac biblioteke: fitdistrplus
library(fitdistrplus)

#trzeba ponownie wyestymowac parametry rozkladu GT
X <- as.numeric(na.omit(max10_lato$max10_lato))

#fSHASH <- fitdist(X, "SHASH", start =list(mu=mu,sigma=sigma,nu=nu,tau=tau))




#plot(fSHASH) #to moze potrwac

#-- wykres dystrybuanta emp.-teo.
plot(ecdf(max10_lato$max10_lato), main="Dystrybuanta empiryczna")
curve(pSHASH(x,mu,sigma,nu,tau), xlim=c(-10,35),col=2,add=TRUE)



#poziom zwrotu

k20 <- 20*92*24*6
k50 <- 50*92*24*6


x20 <-qSHASH(1-1/k20, mu=mu, sigma=sigma, nu=nu, tau=tau)
x50 <- qSHASH(1-1/k50, mu=mu, sigma=sigma, nu=nu, tau=tau)

x20; x50


#---------WIOSNA ------

t1 <- Sys.time()
fit_w <- fitDist(max10_wiosna$max10,type="realline")
t2 <- Sys.time()
t2-t1

fit_w$family


fit_w$fits    

fit_w$parameters  

mu_w <- fit_w$mu
sigma_w <- fit_w$sigma
nu_w <- fit_w$nu
tau_w <- fit_w$tau


k20_w <- 20*92*24*6
k50_w <- 50*92*24*6


x20_w <-qSEP1(1-1/k20_w, mu=mu_w, sigma=sigma_w, tau=tau_w)
x50_w <- qSEP1(1-1/k50_w, mu=mu_w, sigma=sigma_w, tau=tau_w)

x20_w; x50_w



#---------JESIEŃ------
t1 <- Sys.time()
fit_j <- fitDist(max10_jesien$max10,type="realline")
t2 <- Sys.time()
t2-t1


fit_j$family 


fit_j$fits    


fit_j$parameters  

mu_j <- fit_j$mu
sigma_j <- fit_j$sigma
nu_j <- fit_j$nu
tau_j <- fit_j$tau




k20_j <- 20*91*24*6
k50_j <- 50*91*24*6


x20_j <-qSEP1(1-1/k20_j, mu=mu_j, sigma=sigma_j,nu=nu_j, tau=tau_j)
x50_j <- qSEP1(1-1/k50_j, mu=mu_j, sigma=sigma_j,nu=nu_j, tau=tau_j)

x20_j; x50_j


#---------ZIMA------

t1 <- Sys.time()
fit_z <- fitDist(max10_zima$max10,type="realline")
t2 <- Sys.time()
t2-t1


fit_z$family  


fit_z$fits    


fit_z$parameters  

mu_z <- fit_z$mu
sigma_z <- fit_z$sigma
nu_z <- fit_z$nu
tau_z <- fit_z$tau




k20_z <- 7305*24*6
k50_z <- 14610*24*6


x20_z <-qSEP4(1-1/k20_z, mu=mu_z, sigma=sigma_z,nu=nu_z, tau=tau_z)
x50_z <- qSEP4(1-1/k50_z, mu=mu_z, sigma=sigma_z, nu=nu_z, tau=tau_z)

x20_z; x50_z



#=======================
#Metoda BMM
#=======================


#------ ROCZNE ----------
n <- length(data0)

#pierwszy miesiac


max10 <- c()
datetime <- c()

for(i in 1:n){
  max10 <- c(max10,data0[[i]]$X250160090)
  datetime <- c(datetime,as.character(data0[[i]]$datetime))
}

max10 <- data.frame(date=as.Date(datetime),max10=max10)
head(max10)
tail(max10)
rownames(max10) <- c()

max10 <- separate(max10,date,c("year","mth","day"), convert=TRUE)
head(max10)

data <- max10[,4]
hist(data)
data <- data[!is.na(data)]


library(evir)

#obliczmy maksima roczne dla uproszczenia przyjmujemy,że każdy rok ma 365dni
b <- 6*24*365
fit1 <- evir::gev(data,b)  
fit1

#parametry rozkladu GEV
parGEV <- fit1$par.ests; parGEV

xi <- parGEV[[1]]
sigma <- parGEV[[2]]
mu <- parGEV[[3]]




Max <- fit1$data


#rysujemy wykresy diagnostyczne, ale najpierw estymujemy jeszcze raz parametry

library(ismev)

fit2 <- ismev::gev.fit(Max)

print("   mu  |    sigma   |    xi  ")
fit2$mle

#wykresy diagnostyczne 
ismev::gev.diag(fit2)


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
qgev(c(0.95, 0.98), xi=xi,sigma=sigma,mu=mu)




#------------ LATO ------------
bL <- 6*24*92

dataL <- max10_lato[,5]

fit1L <- evir::gev(dataL,bL)  
fit1L

#parametry rozkladu GEV
parGEV_L <- fit1L$par.ests; parGEV_L

xi_L <- parGEV_L[[1]]
sigma_L <- parGEV_L[[2]]
mu_L <- parGEV_L[[3]]




MaxL <- fit1L$data


#rysujemy wykresy diagnostyczne, ale najpierw estymujemy jeszcze raz parametry

library(ismev)

fit2L <- ismev::gev.fit(MaxL)

print("   mu  |    sigma   |    xi  ")
fit2L$mle

#wykresy diagnostyczne 
ismev::gev.diag(fit2L)


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
qgev(c(0.95, 0.98), xi=xi_L,sigma=sigma_L,mu=mu_L)


#---- wersja 2
Max_L=c()
for (i in 2008:2018){
  Max_L=c(Max_L,max(max10_lato$max10_lato[max10_lato$year==i]))
}

fit2_L <- ismev::gev.fit(Max_L)

print("   mu  |    sigma   |    xi  ")
fit2_L$mle

mu_L=fit2_L$mle[1]
sigma_L=fit2_L$mle[2]
xi_L=fit2_L$mle[3]

#wykresy diagnostyczne 
ismev::gev.diag(fit2L)


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
qgev(c(0.95, 0.98), xi=xi_L,sigma=sigma_L,mu=mu_L)

#--------------- WIOSNA -------------
bW <- 6*24*92
dataW <- max10_wiosna[,5]

fit1W <- evir::gev(dataW,bW)  
fit1W

#parametry rozkladu GEV
parGEV_W <- fit1W$par.ests; parGEV_W

xi_W <- parGEV_W[[1]]
sigma_W <- parGEV_W[[2]]
mu_W <- parGEV_W[[3]]


MaxW <- fit1W$data



library(ismev)

fit2W <- ismev::gev.fit(MaxW)

print("   mu  |    sigma   |    xi  ")
fit2W$mle


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
qgev(c(0.95, 0.98), xi=xi_W,sigma=sigma_W,mu=mu_W)


#---- wersja 2
Max_W=c()
for (i in 2008:2018){
  Max_W=c(Max_W,max(max10_wiosna$max10[max10_wiosna$year==i]))
}

fit2_W <- ismev::gev.fit(Max_W)

print("   mu  |    sigma   |    xi  ")
fit2_W$mle

mu_W=fit2_W$mle[1]
sigma_W=fit2_W$mle[2]
xi_W=fit2_W$mle[3]


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
qgev(c(0.95, 0.98), xi=xi_W,sigma=sigma_L,mu=mu_W)



#--------------- ZIMA --------
bZ <- 6*24*90
dataZ <- max10_zima[,5]

fit1Z <- evir::gev(dataZ,bZ)  
fit1Z

#parametry rozkladu GEV
parGEV_Z <- fit1Z$par.ests; parGEV_Z

xi_Z <- parGEV_Z[[1]]
sigma_Z <- parGEV_Z[[2]]
mu_Z <- parGEV_Z[[3]]




MaxZ <- fit1Z$data




library(ismev)

fit2Z <- ismev::gev.fit(MaxZ)

print("   mu  |    sigma   |    xi  ")
fit2Z$mle


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
qgev(c(0.95, 0.98), xi=xi_Z,sigma=sigma_Z,mu=mu_Z)

# ---- wersja 2

Max_Z=c()
for (i in 2008:2018){
  Max_Z=c(Max_Z,max(max10_zima$max10[max10_zima$year==i]))
}

fit2_Z <- ismev::gev.fit(Max_Z)

print("   mu  |    sigma   |    xi  ")
fit2_Z$mle

mu_Z=fit2_Z$mle[1]
sigma_Z=fit2_Z$mle[2]
xi_Z=fit2_Z$mle[3]


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
qgev(c(0.95, 0.98), xi=xi_Z,sigma=sigma_Z,mu=mu_Z)

#----------------jESIEN ---------
bJ <- 6*24*91
dataJ <- max10_jesien[,5]

fit1J <- evir::gev(dataJ,bJ)  
fit1J

#parametry rozkladu GEV
parGEV_J <- fit1J$par.ests; parGEV_J

xi_J <- parGEV_J[[1]]
sigma_J <- parGEV_J[[2]]
mu_J <- parGEV_J[[3]]




MaxJ <- fit1J$data



library(ismev)

fit2J <- ismev::gev.fit(MaxJ)

print("   mu  |    sigma   |    xi  ")
fit2J$mle


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
qgev(c(0.95, 0.98), xi=xi_J,sigma=sigma_J,mu=mu_J)

# ---- wersja 2

Max_J=c()
for (i in 2008:2018){
  Max_J=c(Max_J,max(max10_jesien$max10[max10_jesien$year==i]))
}


fit2_J <- ismev::gev.fit(Max_J)

print("   mu  |    sigma   |    xi  ")
fit2_J$mle

mu_J=fit2_J$mle[1]
sigma_J=fit2_J$mle[2]
xi_J=fit2_J$mle[3]

#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
qgev(c(0.95, 0.98), xi=xi_J,sigma=sigma_J,mu=mu_J)




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
