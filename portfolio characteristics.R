#################################
#### Portfolio Characteristics
load("F:/我的论文/第三篇/RData/da_all_m.RData")
load("F:/我的论文/第三篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")
load("F:/我的论文/第五篇/RData/da_beta_5y.RData")

da_m <- merge(da_all_m,da_realized_m,by=c("ym","SecCode")) # dataset 1
#da_m <- merge(da_all_m,da_mom_m,by=c("ym","SecCode")) # dataset 2
#da_m <- merge(da_all_m,da_beta_5y,by=c("ym","SecCode")) # dataset 3

ym_index <- sort(unique(da_m$ym))
k <- 5
y <- 2  # number of porfolio characteristics variables
# One is max_ret or min_ret, the other is one of portfolio characteristics.
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- array(NA,c(length(ym_index),k,y)) # vari_level denotes variable level
for (i in 1:length(ym_index)) {
  da_sub <- da_m[ym==ym_index[i],]
  da_sub <- da_sub[order(srjv),]
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
    vari_level[i,j,1] <- da_sub[group_n==j,mean(srjv)]
    
    vari_level[i,j,2] <- da_sub[group_n==j,mean(rvol)] # dataset 1
    #vari_level[i,j,2] <- da_sub[group_n==j,mean(rsk)] # dataset 1
    #vari_level[i,j,2] <- da_sub[group_n==j,mean(be)] # dataset 3
    #vari_level[i,j,2] <- da_sub[group_n==j,mean(size)] # dataset 1
    #vari_level[i,j,2] <- da_sub[group_n==j,mean(BM)] # dataset 1
    #vari_level[i,j,2] <- da_sub[group_n==j,mean(mom)] # dataset 2
    #vari_level[i,j,2] <- da_sub[group_n==j,mean(ret_tm)] # dataset 1
    #vari_level[i,j,2] <- da_sub[group_n==j,mean(illiq)] # dataset 1
  }
}
vari_level_m <- matrix(NA,nrow=k,ncol=y) # m denotes mean 
for (j in 1:k) {
  for (p in 1:y) {
    vari_level_m[j,p] <- mean(vari_level[,j,p],na.rm=T)
  }
}
colnames(vari_level_m) <- c("srjv","other variable")
vari_level_m
