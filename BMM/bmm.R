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

data <- max10[,5]
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



#maksima z blokow (w przyblizeniu roczne)
Max <- fit1$data


#rysujemy wykresy diagnostyczne, ale najpierw estymujemy jeszcze raz parametry

library(ismev)

fit2 <- ismev::gev.fit(Max)

print("   mu  |    sigma   |    xi  ")
fit2$mle

#wykresy diagnostyczne 
ismev::gev.diag(fit2)


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
quantile(fit2$data, c(0.95, 0.98))



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



#maksima z blokow (w przyblizeniu roczne)
MaxL <- fit1L$data


#rysujemy wykresy diagnostyczne, ale najpierw estymujemy jeszcze raz parametry

library(ismev)

fit2L <- ismev::gev.fit(MaxL)

print("   mu  |    sigma   |    xi  ")
fit2L$mle

#wykresy diagnostyczne 
ismev::gev.diag(fit2L)


#Poziomy zwrotu x_20=q(0.95) i x_50=q(0,98)
quantile(fit2L$data, c(0.95, 0.98))





