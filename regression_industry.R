#################################
#### Control for Industry
#### Input the Daily Stock Data 
now()
da_ind <- NULL 
for (i in 2007:2010) {   
  for (j in 1:12) {
    path <- paste0("D:/daily_all/",i,"/",i,"_",j,".csv")
    da_ind <- rbind(da_ind,read.csv(path,header=T))
  }
}
da_ind_1 <- da_ind[,c(3,9,7)]
colnames(da_ind_1) <- c("SecCode","TDate","ind_code")
save(da_ind_1,file="C:/Users/Ding/Desktop/da_ind_1.RData")
rm(list=ls())

# Note: the colnames of dataframe are different before and after 2011
now()
da_ind <- NULL
for (j in 1:12) {
  path <- paste0("D:/daily_all/2011/2011_",j,".csv")
  da_ind <- rbind(da_ind,read.csv(path,header=T))
}
da_ind <- rbind(da_ind,read.csv("D:/daily_all/2011/2011_8_2.csv",header=T))
da_ind <- rbind(da_ind,read.csv("D:/daily_all/2011/2011_12_2.csv",header=T))
da_ind_2 <- da_ind[,c(3,9,7)]
colnames(da_ind_2) <- c("SecCode","TDate","ind_code")
save(da_ind_2,file="C:/Users/Ding/Desktop/da_ind_2.RData")
rm(list=ls())

now()
da_ind <- NULL
for (i in 2012:2013) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_ind <- rbind(da_ind,read.csv(path,header=T))
    }
  }
}
da_ind_3 <- da_ind[,c(3,9,7)]
colnames(da_ind_3) <- c("SecCode","TDate","ind_code")
save(da_ind_3,file="C:/Users/Ding/Desktop/da_ind_3.RData")
rm(list=ls())

now()
da_ind <- NULL
for (i in 2014:2015) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_ind <- rbind(da_ind,read.csv(path,header=T))
    }
  }
}
da_ind_4 <- da_ind[,c(3,9,7)]
colnames(da_ind_4) <- c("SecCode","TDate","ind_code")
save(da_ind_4,file="C:/Users/Ding/Desktop/da_ind_4.RData")
rm(list=ls())

now()
da_ind <- NULL
for (i in 2016:2017) {   
  for (j in 1:12) {
    for (k in 1:2) {
      path <- paste0("D:/daily_all/",i,"/",i,"_",j,"_",k,".csv")
      da_ind <- rbind(da_ind,read.csv(path,header=T))
    }
  }
}
da_ind_5 <- da_ind[,c(3,9,7)]
colnames(da_ind_5) <- c("SecCode","TDate","ind_code")
save(da_ind_5,file="C:/Users/Ding/Desktop/da_ind_5.RData")
rm(list=ls())
now()

#################################
#### Compute the indentum Factor
load("F:/我的论文/第三篇/RData/da_ind_1.RData")
load("F:/我的论文/第三篇/RData/da_ind_2.RData")
load("F:/我的论文/第三篇/RData/da_ind_3.RData")
load("F:/我的论文/第三篇/RData/da_ind_4.RData")
load("F:/我的论文/第三篇/RData/da_ind_5.RData")

da_ind <- as.data.table(rbind(da_ind_1,da_ind_2,da_ind_3,da_ind_4,da_ind_5))
da_ind <- da_ind[ind_code!="",]
#class(da_ind$ind_code) # "factor"
da_ind[,ind_code:=as.character(ind_code)]
#which(is.na(da_ind),arr.ind=T)
da_ind <- na.omit(da_ind)

#sort(unique(da_ind$ind_code)) 
# "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S"
#length(sort(unique(da_ind$ind_code))) # 19
for (i in 1:length(sort(unique(da_ind$ind_code)))) {
  da_ind[ind_code==sort(unique(ind_code))[i],ind:=i]
}
#class(da_ind$ind) # "integer"
da_ind[,TDate:=ymd(TDate)]
da_ind[,`:=`(y=lubridate::year(TDate),m=lubridate::month(da_ind$TDate))]
da_ind[,ym:=ymd(paste0(y,"-",m,"-01"))]

da_ind <- da_ind[,.(ind=mean(ind)),by=.(SecCode,ym)]
#sort(unique(da_ind$ind))
da_ind <- da_ind[ind %in% 1:19,]
#save(da_ind,file="C:/Users/Ding/Desktop/da_ind.RData")

################################# 
#### Firm-Level Cross-Sectional Regression  
#### Compute the Median Data Frame
load("F:/我的论文/第三篇/RData/da_all_m.RData")
load("F:/我的论文/第三篇/RData/da_realized_m.RData")
load("F:/我的论文/第三篇/RData/da_ind.RData")
load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")

# Note: In most cases, a company's industry code doesn't change month by month.
# So it's OK to use this month's industry code (as we do here).
# A similar result will be obtained from the next month's industry code.
da_all_m <- merge(da_all_m,da_realized_m,by=c("ym","SecCode"))
da_all_m <- merge(da_all_m,da_ind,by=c("ym","SecCode"))

da_m_ind_1 <- da_all_m 
da_m_ind_1[illiq!=0,`:=`(size=log(size),BM=log(BM),illiq=log(illiq),
                         rvol=log(rvol))]
da_m_ind_1 <- da_m_ind_1[order(ym,SecCode),]
#save(da_m_ind_1,file="C:/Users/Ding/Desktop/da_m_ind_1.RData")

da_m_ind_2 <- merge(da_m_ind_1,da_beta_5y) # +be
da_m_ind_2 <- da_m_ind_2[order(ym,SecCode),]
#save(da_m_ind_2,file="C:/Users/Ding/Desktop/da_m_ind_2.RData")

da_m_ind_3 <- merge(da_m_ind_2,da_mom_m) # +mom
da_m_ind_3 <- da_m_ind_3[order(ym,SecCode),]
#save(da_m_ind_3,file="C:/Users/Ding/Desktop/da_m_ind_3.RData")

#### The Regression Part 
load("F:/我的论文/第三篇/RData/da_m_ind_1.RData")
load("F:/我的论文/第三篇/RData/da_m_ind_2.RData")
load("F:/我的论文/第三篇/RData/da_m_ind_3.RData")

lm_fit_nw <- function(nv) { # nw denotes Newey-West HAC t statistic
  # nv denotes numbers of variables
  lm_sta <- matrix(NA,nrow=nv+1,ncol=2) # sta denotes statistics
  for (i in 1:(nv+1)) {
    model_nw <- lm(lm_sta_raw[i,] ~ 1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(mean(as.numeric(unlist(summary(model)$adj.r.squared))))
}

####
da <- da_m_ind_1
nv <- 1
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~srjv|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind_1
nv <- 1
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rvol|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind_1
nv <- 1
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~rsk|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind_1
nv <- 2
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~srjv+rvol|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind_1
nv <- 2
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~srjv+rsk|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind_1
nv <- 3
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~srjv+rvol+rsk|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind_2
nv <- 4
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~srjv+be+size+BM|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind_3
nv <- 5
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~srjv+be+size+BM+mom|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind_3
nv <- 6
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~srjv+be+size+BM+mom+illiq|ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(summary(model)$coefficients[,,i][,1])
  }
}
lm_fit_nw(nv)
now()

da <- da_m_ind_3
nv <- 8
now()
ym_index <- sort(unique(da$ym))
lm_sta_raw <- matrix(NA,nrow=nv+1,ncol=length(sort(unique(da$ym))))
for (j in 1:length(ym_index)) {
  da_sub <- da[ym==ym_index[j],]
  selected_industry <- da_sub[,.N,by=ind][N>=20,ind]
  da_sub <- da_sub[ind %in% selected_industry,]
  model <- lmList(ret_e~srjv+rvol+rsk+be+size+BM+mom+illiq
                  |ind,da_sub)
  for (i in 1:(nv+1)) {
    lm_sta_raw[i,j] <- mean(as.matrix(summary(model)$coefficients[,,i])[,1])
  }
}
lm_fit_nw(nv)
now()


