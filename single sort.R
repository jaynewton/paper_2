library(data.table)
library(lubridate)
library(dplyr) # use data.table rather dplyr if possible
library(lme4)
library(sandwich) # NeweyWest
library(lmtest) # coeftest
library(zoo)
library(ggplot2)
library(psych)
#library(reshape2) # inclueded and enhanced in data.table

sink(file="C:/Users/Ding/Desktop/主代码.txt",append=T)
#sink(file="C:/Users/Ding/Desktop/稳健性检验.txt",append=T)
sink()
rm(list=ls())

#################################
#### Input the Daily Stock Data
# Note: Here we use adpr to compute the daily return.
# And we using open price and close price to compute illiquidity.
now()
da_all <- NULL 
for (i in 2007:2010) {   
  for (j in 1:12) {
    path <- paste0("D:/daily_all/",i,"/",i,"_",j,".csv")
    da_all <- rbind(da_all,read.csv(path,header=T))
  }
}
da_all_1 <- da_all[,c(3,9,14,15,18,27,55,61)]
colnames(da_all_1) <- c("SecCode","TDate","clpr","adpr","trdsum",
                        "trdshr","rf","PB")
# "Trdsum" denotes the amount of daily trading volume in the unit of money 
# "Trdshr" denotes the volume of outstanding shares 
save(da_all_1,file="C:/Users/Ding/Desktop/da_all_1.RData")
rm(list=ls())

# Note: the colnames of dataframe are different before and after 2011
now()
da_all <- NULL
for (j in 1:12) {
  path <- paste0("D:/daily_all/2011/2011_",j,".csv")
  da_all <- rbind(da_all,read.csv(path,header=T))
}
da_all <- rbind(da_all,read.csv("D:/daily_all/2011/2011_8_2.csv",header=T))
da_all <- rbind(da_all,read.csv("D:/daily_all/2011/2011_12_2.csv",header=T))
da_all_2 <- da_all[,c(3,9,14,15,18,27,55,61)]
colnames(da_all_2) <- c("SecCode","TDate","clpr","adpr","trdsum",
                        "trdshr","rf","PB")
save(da_all_2,file="C:/Users/Ding/Desktop/da_all_2.RData")
rm(list=ls())

now()
da_all <- NULL
for (i in 2012:2013) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_all <- rbind(da_all,read.csv(path,header=T))
    }
  }
}
da_all_3 <- da_all[,c(3,9,14,15,18,27,55,61)]
colnames(da_all_3) <- c("SecCode","TDate","clpr","adpr","trdsum",
                        "trdshr","rf","PB")
save(da_all_3,file="C:/Users/Ding/Desktop/da_all_3.RData")
rm(list=ls())

now()
da_all <- NULL
for (i in 2014:2015) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_all <- rbind(da_all,read.csv(path,header=T))
    }
  }
}
da_all_4 <- da_all[,c(3,9,14,15,18,27,55,61)]
colnames(da_all_4) <- c("SecCode","TDate","clpr","adpr","trdsum",
                        "trdshr","rf","PB")
save(da_all_4,file="C:/Users/Ding/Desktop/da_all_4.RData")
rm(list=ls())

now()
da_all <- NULL
for (i in 2016:2017) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_all <- rbind(da_all,read.csv(path,header=T))
    }
  }
}
da_all_5 <- da_all[,c(3,9,14,15,18,27,55,61)]
colnames(da_all_5) <- c("SecCode","TDate","clpr","adpr","trdsum",
                        "trdshr","rf","PB")
save(da_all_5,file="C:/Users/Ding/Desktop/da_all_5.RData")
rm(list=ls())
now()

#################################
#### Combine the Daily Data
load("F:/我的论文/第三篇/RData/da_all_1.RData")
load("F:/我的论文/第三篇/RData/da_all_2.RData")
load("F:/我的论文/第三篇/RData/da_all_3.RData")
load("F:/我的论文/第三篇/RData/da_all_4.RData")
load("F:/我的论文/第三篇/RData/da_all_5.RData")

da_all <- as.data.table(rbind(da_all_1,da_all_2,da_all_3,da_all_4,da_all_5))
da_all <- da_all[(SecCode>=600000 & SecCode<900000) | SecCode<200000,]
#da_all[,which(trdshr<0)] # integer(0)
#which(is.na(da_all),arr.ind = T)
da_all <- na.omit(da_all)

da_all[,TDate:=ymd(TDate)]
da_all[,`:=`(y=lubridate::year(TDate), 
             m=lubridate::month(TDate))]
da_all[,ym:=ymd(paste0(y,"-",m,"-01"))]
da_all <- da_all[order(SecCode,TDate),]
da_all[,adpr_ld:=c(NA,adpr[-(.N)]),by=SecCode]
da_all <- na.omit(da_all)
da_all[,ret_d:=log(adpr)-log(adpr_ld)] 
nrow(da_all) # 

now()
da_intermediate <- NULL
ym_index <- sort(unique(da_all$ym))
for (i in 1:length(ym_index)) {
  da_sub <- da_all[ym==ym_index[i],]
  selected_code <- da_sub[,.N,by=SecCode][N>=10,SecCode]
  da_sub <- da_sub[SecCode %in% selected_code,]
  da_intermediate <- rbind(da_intermediate,da_sub)
}
now() 
da_all <- da_intermediate
nrow(da_all) # 
save(da_all,file="C:/Users/Ding/Desktop/da_all.RData")
# Note: Data in da_all is form 1995 to 2017.

#################################
#### Transform Daily Data into monthly Data 
load("F:/我的论文/第三篇/RData/da_all.RData")

now()
da_all_m <- da_all[,.(ret=sum(ret_d),rf=sum(rf),
                      size=mean(clpr*trdshr),BM=mean(1/PB),
                      illiq=mean(abs(ret_d)/trdsum*10^10)),
                   keyby=.(ym,SecCode)]
now() # The loop above takes about 10 minutes
nrow(da_all_m) #
 
# rvol denotes the realized volatility.
# BM denotes book to market ratio
#mean(da_all$trdsum) # 
#median(da_all$trdsum) # 

da_all_nm <- da_all_m[,.(ym,SecCode,ret,rf)]
da_all_nm[,ym:=ym-months(1)]
names(da_all_m)[names(da_all_m)=="ret"] <- "ret_tm" # tm denotes this month.
names(da_all_m)[names(da_all_m)=="rf"] <- "rf_tm" 
da_all_m <- merge(da_all_m,da_all_nm,by=c("ym","SecCode"))
da_all_m[,`:=`(ret_e=ret-rf,ret_e_tm=ret_tm-rf_tm)]
#da_all_m <- da_all_m[order(ym,SecCode),] # We have used keyby above.

save(da_all_m,file="C:/Users/Ding/Desktop/da_all_m.RData")

#################################
#### Single Sort
#### Not Adjusted by FF3F
load("F:/我的论文/第三篇/RData/da_all_m.RData")
load("F:/我的论文/第三篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")

da_m <- merge(da_all_m,da_realized_m,keyby=c("ym","SecCode"))

ym_index <- sort(unique(da_m$ym))
k <- 5
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- matrix(NA,nrow=length(ym_index),ncol=k) # vari_level denotes variable level
for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
  da_sub <- da_sub[order(srjv),]
  #da_sub <- da_sub[order(rvol),]
  #da_sub <- da_sub[order(rsk),]
  n_mid <- floor(nrow(da_sub)/k)
  if ((nrow(da_sub)-n_mid*(k-2))%%2==0){
    n_f <- (nrow(da_sub)-n_mid*(k-2))/2 # f denotes first, l denotes last
    n_l <- n_f
  } else {
    n_f <- (nrow(da_sub)-n_mid*(k-2)-1)/2
    n_l <- n_f+1
  }
  x <- seq(from=n_f,to=nrow(da_sub),by=n_mid)[1:(k-1)]
  x <- c(x,nrow(da_sub))
  da_sub$group_n <- cut(1:nrow(da_sub), c(0,x),labels = 1:k)
  for (j in 1:k) {
    ret_p[i,j] <- da_sub[group_n==j,mean(ret_e)]
    #ret_p[i,j] <- da_sub[group_n==j,weighted.mean(ret_e,size)]
    vari_level[i,j] <- da_sub[group_n==j,mean(srjv)]
    #vari_level[i,j] <- da_sub[group_n==j,weighted.mean(srjv,size)]
    #vari_level[i,j] <- da_sub[group_n==j,mean(rvol)]
    #vari_level[i,j] <- da_sub[group_n==j,weighted.mean(rvol,size)]
    #vari_level[i,j] <- da_sub[group_n==j,mean(rsk)]
    #vari_level[i,j] <- da_sub[group_n==j,weighted.mean(rsk,size)]
  }
}

#### the part below calculate the performance of the whole sample period 
ret_p_m <- colMeans(ret_p,na.rm=T) # m denotes mean # full sample 
vari_level_m <- colMeans(vari_level,na.rm=T)
output <- cbind(vari_level_m,ret_p_m) 
output
result <- c(output[,2],hml=output[k,2]-output[1,2])
result

#### the part below calculate the performance in each year
# Note: we only consider the equal-weighted case.
# Note: In the year of 2007, we only have 11 observations for portfolio.
sample_y <- 2008:2017 
# sample_y denotes the year of the sample (the first year is not included)
result <- matrix(NA,nrow=length(sample_y)+1,ncol=k+1)
ret_p_m <- colMeans(ret_p[1:11,],na.rm=T) 
result[1,] <- c(ret_p_m,hml=as.numeric(ret_p_m[k]-ret_p_m[1]))
for (i in 2:(length(sample_y)+1)) {
  ret_p_m <- colMeans(ret_p[((i-1)*12):(i*12-1),],na.rm=T)
  result[i,] <- c(ret_p_m,hml=as.numeric(ret_p_m[k]-ret_p_m[1]))
}
result

## Newey-West t statistic
ret_p_hl <- ret_p[,k]-ret_p[,1] # ret_p_hl denotes high minus low portfolio returns
model_nw <- lm(ret_p_hl ~ 1)
coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]

#### Adjusted by FF3F
FF3F_nm <- copy(FF3F_A_nm)
ret_p <- as.data.table(na.omit(ret_p))
ret_p$ym <- sort(unique(da_m$ym))
ret_p <- merge(ret_p,FF3F_nm,by="ym") 
## Newey-West t statistic
ret_p_FF3F <- matrix(NA,nrow=2,ncol=k+1) 
for (i in 1:k) { # the first column is the ym
  model_FF3F <- lm(ret_p[[i+1]]~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml]) 
  # ret[,i+1] is wrong. See 1.5 of Frequently Asked Questions about data.table
  ret_p_FF3F[1,i] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
  ret_p_FF3F[2,i] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
}
ret_p_hl <- ret_p[[k+1]]-ret_p[[2]] 
model_FF3F <- lm(ret_p_hl~ret_p[,mkt_e]+ret_p[,smb]+ret_p[,hml])
ret_p_FF3F[1,k+1] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,1]
ret_p_FF3F[2,k+1] <- coeftest(model_FF3F,vcov=NeweyWest(model_FF3F))[1,3]
ret_p_FF3F

