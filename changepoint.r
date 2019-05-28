# Generate changepoint2.csv file, using moving t-test (stars)

library(changepoint)
library(dplyr)
library(zoo)
library(bcp)
library(cpm)

source("stars.r")

## Wide regime, mortality on, count dead (normal)
FD_wide=read.csv("E:/nm_scardata/e3_range_wide/FD_timeseries_TRUE.csv")

## narrow regime, mortality on, count dead (normal)
FD_narrow=read.csv("E:/nm_scardata/e3_range_narrow/FD_timeseries_TRUE.csv")


#Upper 95% CI
u_95=function(x){
  quantile(x,.95,na.rm=TRUE)
}

l_95=function(x){
  quantile(x,.05,na.rm=TRUE)
}

### Create a time-series, from a variable, of mean, sd
comb_ts=function(data,var,splityear){
  n_data=subset(data,select=c("ID","year",var),SPLIT_YEAR==splityear)
  names(n_data)=c("ID","year","var")
  as.data.frame(n_data)
}




cd_ts=function(data1,inpf,tit,sy){
  ids=unique(data1$ID)
  for(i in seq_along(ids)){
    this_d = subset(data1,ID==ids[i])
    #plot(this_d$var ~ this_d$year)
    input = this_d$var
    names(input)=1:length(input)
    cd = stars(input,L=29)
    theseq = which.min(cd$starsResult[,3])
    #if(cd$changeDetected==TRUE){
    tdf=data.frame(file=inpf,sampler=tit,splityear=sy,changedetection=theseq)
    #}else{
    #  tdf=data.frame(file=inpf,sampler=tit,splityear=sy,changedetection=-Inf)
    #}
    write.table(tdf,file="changepoint.csv",append=TRUE,row.names=FALSE,col.names=FALSE,sep=",",eol="\n")
  }
}



## Do a given regime for FD
do_FD_chart=function(data,file,splityear,typeofd){
  base_ts=comb_ts(data,"FD_RealSeries",splityear)
  
  
  # Targeted 
  this_ts=comb_ts(data,"FD_T_10",splityear)
  cd_ts(this_ts,typeofd,"FD_T_10",splityear)
  
  this_ts=comb_ts(data,"FD_T_25",splityear)
  cd_ts(this_ts,typeofd,"FD_T_25",splityear)
  
  this_ts=comb_ts(data,"FD_T_50",splityear)
  cd_ts(this_ts,typeofd,"FD_T_50",splityear)
  
  this_ts=comb_ts(data,"FD_T_100",splityear)
  cd_ts(this_ts,typeofd,"FD_T_100",splityear)
  
  this_ts=comb_ts(data,"FD_T_250",splityear)
  cd_ts(this_ts,typeofd,"FD_T_250",splityear)
  
  this_ts=comb_ts(data,"FD_T_500",splityear)
  cd_ts(this_ts,typeofd,"FD_T_500",splityear)
  
  this_ts=comb_ts(data,"FD_CensusSeries",splityear)
  cd_ts(this_ts,typeofd,"FD_CensusSeries",splityear)
  
  # Random 
  this_ts=comb_ts(data,"FD_R_10",splityear)
  cd_ts(this_ts,typeofd,"FD_R_10",splityear)
  
  this_ts=comb_ts(data,"FD_R_25",splityear)
  cd_ts(this_ts,typeofd,"FD_R_25",splityear)
  
  this_ts=comb_ts(data,"FD_R_50",splityear)
  cd_ts(this_ts,typeofd,"FD_R_50",splityear)
  
  this_ts=comb_ts(data,"FD_R_100",splityear)
  cd_ts(this_ts,typeofd,"FD_R_100",splityear)
  
  this_ts=comb_ts(data,"FD_R_250",splityear)
  cd_ts(this_ts,typeofd,"FD_R_250",splityear)
  
  this_ts=comb_ts(data,"FD_R_500",splityear)
  cd_ts(this_ts,typeofd,"FD_R_500",splityear)
  
}

split_list=unique(FD_wide$SPLIT_YEAR)  
template_frame= data.frame(file="Test",sampler="Test",splityear=0,changedetection=0)
write.csv(template_frame,file="changepoint.csv",row.names=FALSE)




for(j in seq_along(split_list)){
  
  this_split = split_list[j]
  print(this_split)
  # Mortality on, include dead
  do_FD_chart(FD_wide,file=paste0("figures/exp3b_FD_wide_",this_split,".pdf"),this_split,"Wide")
  do_FD_chart(FD_narrow,file=paste0("figures/exp3b_FD_narrow_",this_split,".pdf"),this_split,"Narrow")
}

