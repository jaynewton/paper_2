#################################
#### Descriptive Statistics
load("F:/我的论文/第三篇/RData/da_all_m.RData")
load("F:/我的论文/第三篇/RData/da_realized_m.RData")
load("F:/我的论文/第五篇/RData/da_beta_5y.RData")
load("F:/我的论文/第五篇/RData/da_mom_m.RData")

#### Realized Variables Only
da_m <- merge(da_all_m,da_realized_m,keyby=c("ym","SecCode"))

cor(da_m[,.(srjv,rvol,rsk)])
#print(cor(da_m[,.(srjv,rvol,rsk)]),digits=3)
#round(cor(da_m[,.(srjv,rvol,rsk)]),digits=3)
#round(cor(da_m[,.(srjv,rvol,rsk)]),digits=3) # double quotation marks appears

ds_1 <- describe(da_m[,.(srjv,rvol,rsk)],type=1)[,c("mean","median","sd","skew","kurtosis","min","max")]
ds_1[,"kurtosis"] <- ds_1[,"kurtosis"]+3
ds_1

#### All Variables
da_m <- NULL
da_m <- merge(da_all_m,da_realized_m,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_beta_5y,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_mom_m,keyby=c("ym","SecCode"))

cor(da_m[,.(srjv,rvol,rsk,be,size,BM,mom,illiq,ret_tm)])

ds_2 <- describe(da_m[,.(srjv,rvol,rsk,be,size,BM,mom,illiq,ret_tm)])[,c("mean","median","sd","skew","kurtosis","min","max")]
ds_2[,"kurtosis"] <- ds_2[,"kurtosis"]+3
#ds_2
format(ds_2,digits=3)

#### All Variables except ret_tm
da_m <- NULL
da_m <- merge(da_all_m,da_realized_m,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_beta_5y,keyby=c("ym","SecCode"))
da_m <- merge(da_m,da_mom_m,keyby=c("ym","SecCode"))

cor(da_m[,.(srjv,rvol,rsk,be,size,BM,mom,illiq)])

ds_3 <- describe(da_m[,.(srjv,rvol,rsk,be,size,BM,mom,illiq)])[,c("mean","median","sd","skew","kurtosis","min","max")]
ds_3[,"kurtosis"] <- ds_3[,"kurtosis"]+3
#ds_3
format(ds_3,digits=3)
