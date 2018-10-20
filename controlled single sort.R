#################################
#### Single Sort with Control
#### The Regression Part
load("F:/我的论文/第三篇/RData/da_m_lm_1.RData")
load("F:/我的论文/第三篇/RData/da_m_lm_2.RData")
load("F:/我的论文/第三篇/RData/da_m_lm_3.RData")
load("F:/我的论文/第五篇/RData/FF3F_A_nm.RData")

da_m_control_1 <- copy(da_m_lm_1)  
da_m_control_2 <- copy(da_m_lm_2) # +be
da_m_control_3 <- copy(da_m_lm_3) # +mom

da_m_control_1[,srjv_rvol:=summary(lm(srjv ~ rvol))$residuals,by=ym]
da_m_control_1[,srjv_rvol:=summary(lm(srjv ~ rsk))$residuals,by=ym]
da_m_control_1[,srjv_size:=summary(lm(srjv ~ size))$residuals,by=ym]
da_m_control_1[,srjv_BM:=summary(lm(srjv ~ BM))$residuals,by=ym]
da_m_control_1[,srjv_ret_tm:=summary(lm(srjv ~ ret_tm))$residuals,by=ym]
da_m_control_1[,srjv_illiq:=summary(lm(srjv ~ illiq))$residuals,by=ym]

da_m_control_2[,srjv_be:=summary(lm(srjv ~ be))$residuals,by=ym]

da_m_control_3[,srjv_mom:=summary(lm(srjv ~ mom))$residuals,by=ym]

#### Not Adjusted by FF3F
da_m <- da_m_control_1
#da_m <- da_m_control_2
#da_m <- da_m_control_3

ym_index <- sort(unique(da_m$ym))
k <- 5
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- matrix(NA,nrow=length(ym_index),ncol=k) # vari_level denotes variable level
for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
  
  da_sub <- da_sub[order(srjv_rvol),] # da_m_control_1
  #da_sub <- da_sub[order(srjv_rsk),]
  #da_sub <- da_sub[order(srjv_size),]
  #da_sub <- da_sub[order(srjv_BM),]
  #da_sub <- da_sub[order(srjv_ret_tm),]
  #da_sub <- da_sub[order(srjv_illiq),]
  
  #da_sub <- da_sub[order(srjv_be),] # da_m_control_2
  
  #da_sub <- da_sub[order(srjv_mom),] # da_m_control_3
  
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
    # This part we do not need to take risk-free rate into consideration 
    #ret_p[i,j] <- da_sub[group_n==j,weighted.mean(ret_e,size)]
  }
}

ret_p_m <- colMeans(ret_p,na.rm=T) # m denotes mean # full sample 
c(ret_p_m,hl=as.numeric(ret_p_m[k]-ret_p_m[1]))

## Newey-West t statistic
ret_p_hl <- ret_p[,k]-ret_p[,1] # ret_p_hl denotes high minus low portfolio returns
model_nw <- lm(ret_p_hl ~ 1) # nw denotes Newey-West
coeftest(model_nw,vcov = NeweyWest(model_nw))[1,3]

#### Adjusted by FF3F
FF3F_nm <- copy(FF3F_A_nm)
ret_p <- as.data.table(na.omit(ret_p))
ret_p$ym <- sort(unique(da_m$ym))
ret_p <- merge(ret_p,FF3F_nm,by="ym") 
## Newey-West t statistic
ret_p_FF3F <- matrix(NA,nrow=2,ncol=k+1) 
for (i in 1:k) { # the first column is the week
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
