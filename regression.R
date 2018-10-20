################################# 
#### Firm-Level Cross-Sectional Regression  
load("F:/我的论文/第三篇/RData/da_all_m.RData")
load("F:/我的论文/第三篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")

#skew(da_realized_m[,rvol]) #
#skew(da_all_m[,size]) #
#skew(da_all_m[,BM]) # 
#skew(da_all_m[,illiq]) # 

#da_m_lm_1[illiq==0,] # Empty data.table (0 rows)
# So it's OK of we delete illiq!=0 in the below.

da_m_lm_1 <- merge(da_all_m,da_realized_m,by=c("ym","SecCode")) 
da_m_lm_1[illiq!=0,`:=`(size=log(size),BM=log(BM),illiq=log(illiq),
                        rvol=log(rvol))]
da_m_lm_1 <- da_m_lm_1[order(ym,SecCode),]
#save(da_m_lm_1,file="C:/Users/Ding/Desktop/da_m_lm_1.RData")

da_m_lm_2 <- merge(da_m_lm_1,da_beta_5y,by=c("ym","SecCode")) # +be
da_m_lm_2 <- da_m_lm_2[order(ym,SecCode),]
#save(da_m_lm_2,file="C:/Users/Ding/Desktop/da_m_lm_2.RData")

da_m_lm_3 <- merge(da_m_lm_2,da_mom_m,by=c("ym","SecCode")) # +mom
da_m_lm_3 <- da_m_lm_3[order(ym,SecCode),]
#save(da_m_lm_3,file="C:/Users/Ding/Desktop/da_m_lm_3.RData")

################################# 
load("F:/我的论文/第三篇/RData/da_m_lm_1.RData")
load("F:/我的论文/第三篇/RData/da_m_lm_2.RData")
load("F:/我的论文/第三篇/RData/da_m_lm_3.RData")

lm_fit_nw <- function(model,nv) { # nw denotes Newey-West HAC t statistic
  # nv denotes numbers of variables
  lm_sta <- matrix(NA,nrow=nv+1,ncol=2) # sta denotes statistics
  for (i in 1:(nv+1)) {
    model_nw <- lm(summary(model)$coefficients[,,i][,1] ~ 1)
    lm_sta[i,1] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,1]
    lm_sta[i,2] <- coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]
  }
  print(lm_sta)
  print(mean(as.numeric(unlist(summary(model)$adj.r.squared))))
}

#### The Regression Part
lm_1 <- lmList(ret ~ srjv | ym , data=da_m_lm_1)
lm_fit_nw(lm_1,1) 
# The intercept is automatically included.
# That's why the number of variables here is 1, not 2
lm_2 <- lmList(ret ~ rvol | ym , data=da_m_lm_1)
lm_fit_nw(lm_2,1) 
lm_3 <- lmList(ret ~ rsk | ym , data=da_m_lm_1)
lm_fit_nw(lm_3,1) 
lm_4 <- lmList(ret ~ srjv+rvol | ym , data=da_m_lm_1)
lm_fit_nw(lm_4,2) 
lm_5 <- lmList(ret ~ srjv+rsk | ym , data=da_m_lm_1)
lm_fit_nw(lm_5,2) 
lm_6 <- lmList(ret ~ srjv+rvol+rsk | ym , data=da_m_lm_1)
lm_fit_nw(lm_6,3) 
lm_7 <- lmList(ret_e ~ srjv+be+size+BM | ym , data=da_m_lm_2)
lm_fit_nw(lm_7,4)
lm_8 <- lmList(ret_e ~ srjv+be+size+BM+mom | ym , data=da_m_lm_3)
lm_fit_nw(lm_8,5)
lm_9 <- lmList(ret_e ~ srjv+be+size+BM+mom+illiq | ym , data=da_m_lm_3)
lm_fit_nw(lm_9,6)
lm_10 <- lmList(ret_e ~ srjv+rvol+rsk+be+size+BM+mom+illiq | ym , data=da_m_lm_3)
lm_fit_nw(lm_10,8)

#### Only Consider Realized Variables
lm_1 <- lmList(ret ~ rvol | ym , data=da_m_lm_1)
lm_fit_nw(lm_1,1) 
lm_2 <- lmList(ret ~ rsk | ym , data=da_m_lm_1)
lm_fit_nw(lm_2,1) 
lm_3 <- lmList(ret ~ rkt | ym , data=da_m_lm_1)
lm_fit_nw(lm_3,1) 
lm_4 <- lmList(ret ~ srjv+rvol | ym , data=da_m_lm_1)
lm_fit_nw(lm_4,2) 
lm_5 <- lmList(ret ~ srjv+rsk | ym , data=da_m_lm_1)
lm_fit_nw(lm_5,2)
lm_6 <- lmList(ret ~ srjv+rkt | ym , data=da_m_lm_1)
lm_fit_nw(lm_6,2)
lm_7 <- lmList(ret ~ srjv+rvol+rsk+rkt | ym , data=da_m_lm_1)
lm_fit_nw(lm_7,4) 

