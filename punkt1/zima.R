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

#czy dana stacja jest wsrod stacji z danego miesiaca
x <- c()
for(i in 1:33){
  
  sti <- colnames(data0[[wiosna[i]]])
  x[i] <- "X250160090" %in% sti
}

sum(x)
which(x==TRUE)



#zapisanie w formacie tylko Date
max10_zima <- data.frame(date=as.Date(datetime_zima),max10=max10_zima)
head(max10_zima)
tail(max10_zima)
rownames(max10_zima) <- c()

max10_zima <- separate(max10_zima,date,c("year","mth","day"), convert=TRUE)
head(max10_zima)

max10_zima <- data.frame(datetime=datetime_zima,max10_zima)


max10_zima<-na.omit(max10_zima)

hist(max10_zima$max10, prob=TRUE)


t1 <- Sys.time()
fit_z <- fitDist(max10_zima$max10,type="realline")
t2 <- Sys.time()
t2-t1

#obejrzyjmy wyniki estymacji

#--- Jaki rozklad ma najmniejsze AIC? 
#Dostaniemy tez  skrot rozkladu, potrzebny dalej do np. gestosci dGT(x,parametry)
fit_z$family  #np. "GT"            "Generalized t"


#--- ,,dopasowane rozklady'' posortowane wedlug malejacej wartosci AIC
fit_z$fits    

#--- skroty nazw  parametrow ,,najlepszego rozkladu'' i wartosci parametrow
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