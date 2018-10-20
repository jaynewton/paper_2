#################################
#### Transform Daily Data into monthly Data 
load("F:/我的论文/第三篇/RData/da_all.RData")

now()
da_all_m <- da_all[,.(max_ret_1=max(ret_d),min_ret_1=min(ret_d),
                      max_ret_3=mean(sort(ret_d,decreasing=T)[1:3]),
                      min_ret_3=mean(sort(ret_d)[1:3]),
                      max_ret_5=mean(sort(ret_d,decreasing=T)[1:5]),
                      min_ret_5=mean(sort(ret_d)[1:5])),
                   keyby=.(ym,SecCode)]
now() 
nrow(da_all_m)

da_max_m <- copy(da_all_m)
da_max_nm <- copy(da_all_m)
da_max_nm[,ym:=ym-months(1)]
da_max_nm <- da_max_nm[ym>=ymd("2007-1-1"),]

save(da_max_m,file="C:/Users/Ding/Desktop/da_max_m.RData")
save(da_max_nm,file="C:/Users/Ding/Desktop/da_max_nm.RData")

#################################
#### Single Sort
#### Not Adjusted by FF3F
load("F:/我的论文/第三篇/RData/da_all_m.RData")
load("F:/我的论文/第三篇/RData/da_realized_m.RData")
load("F:/我的论文/第三篇/RData/da_max_m.RData")

da_m <- merge(da_all_m,da_realized_m,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_max_m,keyby=c("ym","SecCode"))

ym_index <- sort(unique(da_m$ym))
k <- 5
ret_p <- matrix(NA,nrow=length(ym_index),ncol=k) # p denotes portfolio
colnames(ret_p) <- paste0("p",1:k)
vari_level <- matrix(NA,nrow=length(ym_index),ncol=k) # vari_level denotes variable level
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
    ret_p[i,j] <- da_sub[group_n==j,mean(max_ret_1)]
    #ret_p[i,j] <- da_sub[group_n==j,mean(max_ret_3)]
    #ret_p[i,j] <- da_sub[group_n==j,mean(max_ret_5)]
    #ret_p[i,j] <- da_sub[group_n==j,mean(min_ret_1)]
    #ret_p[i,j] <- da_sub[group_n==j,mean(min_ret_3)]
    #ret_p[i,j] <- da_sub[group_n==j,mean(min_ret_5)]
    vari_level[i,j] <- da_sub[group_n==j,mean(srjv)]
  }
}

#### the part below calculate the performance of the whole sample period 
ret_p_m <- colMeans(ret_p,na.rm=T) # m denotes mean # full sample 
vari_level_m <- colMeans(vari_level,na.rm=T)
output <- cbind(vari_level_m,ret_p_m) 
output

#################################
#### Persistence of Extreme value

#### Note: Relevant research see paper 5 code: Persistence.

