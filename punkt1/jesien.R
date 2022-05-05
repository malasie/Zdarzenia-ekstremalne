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


#===== przeksztalcenie kolumny datetime_jesien

#zapisanie w formacie tylko Date
max10_jesien <- data.frame(date=as.Date(datetime_jesien),max10=max10_jesien)
head(max10_jesien)
tail(max10_jesien)
rownames(max10_jesien) <- c()

max10_jesien <- separate(max10_jesien,date,c("year","mth","day"), convert=TRUE)
head(max10_jesien)

max10_jesien <- data.frame(datetime=datetime_jesien,max10_jesien)


max10_jesien<-na.omit(max10_jesien)





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