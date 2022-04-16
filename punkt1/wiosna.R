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

#===== przeksztalcenie kolumny datetime_wiosna

#zapisanie w formacie tylko Date
max10_wiosna <- data.frame(date=as.Date(datetime_wiosna),max10=max10_wiosna)
head(max10_wiosna)
tail(max10_wiosna)
rownames(max10_wiosna) <- c()

max10_wiosna <- separate(max10_wiosna,date,c("year","mth","day"), convert=TRUE)
head(max10_wiosna)

max10_wiosna <- data.frame(datetime=datetime_wiosna,max10_wiosna)

max10_wiosna[119412]$max10 = NA

max10_wiosna<-na.omit(max10_wiosna)

hist(max10_wiosna$max10, prob=TRUE)




t1 <- Sys.time()
fit_w <- fitDist(max10_wiosna$max10,type="realline")
t2 <- Sys.time()
t2-t1

#obejrzyjmy wyniki estymacji

#--- Jaki rozklad ma najmniejsze AIC? 
#Dostaniemy tez  skrot rozkladu, potrzebny dalej do np. gestosci dGT(x,parametry)
fit_w$family  #np. "GT"            "Generalized t"


#--- ,,dopasowane rozklady'' posortowane wedlug malejacej wartosci AIC
fit_w$fits    

#--- skroty nazw  parametrow ,,najlepszego rozkladu'' i wartosci parametrow
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