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


#rysujemy wykresy diagnostyczne, ale najpierw estymujemy jeszcze raz parametry

library(ismev)

fit2W <- ismev::gev.fit(MaxW)

print("   mu  |    sigma   |    xi  ")
fit2W$mle

#wykresy diagnostyczne 
ismev::gev.diag(fit2W)


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


#rysujemy wykresy diagnostyczne, ale najpierw estymujemy jeszcze raz parametry

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


#rysujemy wykresy diagnostyczne, ale najpierw estymujemy jeszcze raz parametry

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
