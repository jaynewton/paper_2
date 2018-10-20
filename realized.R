#################################
#### Input the High Frequency Stock Data
READ_HF <- function(year_n,quarter_n,k) { 
  # READ_HF means read high frequency data
  # k is the number of txt files of that quarter
  da_raw <- NULL
  for (i in 0:k) {
    path <- paste0("D:/高频数据/",year_n,quarter_n,"/",year_n,quarter_n,".part",i,".txt")
    da_raw <- rbind(da_raw,read.table(path,header=T,sep="\t"))
  }
  da_raw <<- da_raw
}

#READ_HF(2007,1,38) # example for test

#### Transform 5-minute High-Frequency Data into Daily Data
HF_M <- function(year_n,quarter_n,k){ 
  # HF_M denotes the function is written to change high-frequency data into daily data,
  # year_n denotes the year number, quarter_n denotes the quarter number
  READ_HF(year_n,quarter_n,k)
  da_raw <- as.data.table(da_raw)
  da_raw[,TDate:=ymd(TDate)]
  da_raw <- da_raw[order(SecCode,TDate,MinTime),]
  da_raw[,ret:=c(NA,diff(log(EndPrc))),by=.(SecCode,TDate)]
  da_raw[,`:=`(y=lubridate::year(TDate), 
               m=lubridate::month(TDate))]
  da_raw[,ym:=ymd(paste0(y,"-",m,"-01"))]
  da_raw <- na.omit(da_raw)
  da_realized_m <- da_raw[,.(rs_n=sum((ret*ifelse(ret<0,1,0))^2),
                             rs_p=sum((ret*ifelse(ret>0,1,0))^2),
                             rvol=sqrt(sum(ret^2)),rsk_nu=sqrt(.N)*sum(ret^3),
                             rkt_nu=sqrt(.N)*sum(ret^4)),by=.(SecCode,ym)]
  #which(is.na(da_realized_m), arr.ind = T)
  da_realized_m <- na.omit(da_realized_m)
  da_realized_m <- da_realized_m[rvol!=0,]
  da_realized_m[,`:=`(srjv=(rs_p-rs_n)/rvol^2,
                      rsk=rsk_nu/rvol^3,
                      rkt=rkt_nu/rvol^4)]
  da_realized_m <- da_realized_m[,.(ym,SecCode,srjv,rvol,rsk,rkt)]
  print(year_n)
  print(quarter_n)
  print(now())
  save(da_realized_m,file=paste0("C:/Users/Ding/Desktop/hf_d/hf_d_",year_n,"_",quarter_n,".RData"))
  # "a" in the above line means alternative.
}

#HF_M(2007,1,38) # An example for test

now()
year_range <- 2007:2017
parameter_matrix <- matrix(NA,nrow=length(year_range)*4,ncol=3)
parameter_matrix[,1] <- rep(year_range,each = 4) # year
parameter_matrix[,2] <- rep(1:4,times=length(year_range)) # quarter
parameter_matrix[,3] <- c(38,40,44,42,  # 2007 # number of files
                          43,44,48,49,  # 2008
                          45,47,52,49,  # 2009
                          50,53,58,59,  # 2010
                          59,63,71,67,  # 2011
                          66,69,78,75,  # 2012
                          69,70,78,74,  # 2013
                          71,73,78,74,  # 2014
                          70,76,77,76,  # 2015
                          76,78,86,87,  # 2016
                          89,90,100,94) # 2017

for (i in 1:nrow(parameter_matrix)) {
  HF_M(parameter_matrix[i,1],parameter_matrix[i,2],parameter_matrix[i,3])
}

#################################
#### Transform Daily Data into monthly Data (For High-Frequency Data Part)
year_range <- 2007:2017
da_realized_intermediate <- data.table()
for (i in year_range) {   
  for (j in 1:4){
    path <- paste0("F:/我的论文/第三篇/RData/hf_d/hf_d_",i,"_",j,".RData")
    load(path)
    da_realized_intermediate <- rbind(da_realized_intermediate,da_realized_m)
  }
}
da_realized_m <- da_realized_intermediate # a denotes alternative.
da_realized_m <- da_realized_m[(SecCode>=600000 & SecCode<900000) | SecCode<200000,]
# only include A stock

#save(da_realized_m,file="C:/Users/Ding/Desktop/da_realized_m.RData")
