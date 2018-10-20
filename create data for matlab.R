#################################
#### Create Data for Matlab

#### ret_p
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
    vari_level[i,j] <- da_sub[group_n==j,mean(srjv)]
  }
}

FF3F_nm <- copy(FF3F_A_nm)
ret_p <- as.data.table(na.omit(ret_p))
ret_p[,ym:=sort(unique(da_m$ym))]
ret_p <- merge(ret_p,FF3F_nm,by="ym")

write.table(ret_p,file="C:/Users/Ding/Desktop/ret_p.txt",row.names = F,col.names = F)

#################################
#### Create Data for Matlab

#### ret_p_vw 
# vw denotes value-weighted
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
    ret_p[i,j] <- da_sub[group_n==j,weighted.mean(ret_e,size)]
    vari_level[i,j] <- da_sub[group_n==j,weighted.mean(srjv,size)]
  }
}

FF3F_nm <- copy(FF3F_A_nm)
ret_p_vw <- as.data.table(na.omit(ret_p))
ret_p_vw[,ym:=sort(unique(da_m$ym))]
ret_p_vw <- merge(ret_p_vw,FF3F_nm,by="ym")

write.table(ret_p_vw,file="C:/Users/Ding/Desktop/ret_p_vw.txt",row.names = F,col.names = F)



