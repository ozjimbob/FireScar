## Change point 2D Histograms
library(tidyverse)

cdata=read_csv("changepoint.csv")


plot_fn <- function(wide,fn){
  sampler_list=c("FD_T_10","FD_T_25","FD_T_50","FD_T_100","FD_T_250","FD_T_500","FD_CensusSeries")
  year_list = unique(wide$splityear)
  
  pdf(paste0("",fn),width=35,height=20)
  par(mfrow=c(length(sampler_list)+1,length(year_list)+1))
  par(mar=c(2.5,1,2,2))
  plot.new()
  plot.window(c(0,1),c(0,1))
  for(j in year_list){
    plot.new()
    plot.window(c(0,1),c(0,1))
    text(0.5,0.5,700-j,cex=3)
  }
  mini_sampler_list=c("10","25","50","100","250","500","Census")
  idx=1
  for(j in sampler_list){
    plot.new()
    plot.window(c(0,1),c(0,1))
    text(0.5,0.5,mini_sampler_list[idx],cex=3,srt=90)
    idx=idx+1
    for(i in year_list){
      this_frame = filter(wide,splityear == i & sampler == j)
      cd = this_frame$changedetection-i
      co=cd[is.finite(cd)]
      ii=sum(abs(co)<25)

      if(ii>20){
        tcol="red"
      }else{
        tcol="grey50"
      }
      hist(cd,main="",xlab="",xlim=c(-200,200),ylim=c(0,35),breaks=seq(-612,612,25),col=tcol,xaxt="n",yaxt="n")
      axis(1,at=c(-200,0,200),labels=c(-200,0,200),cex.axis=2)
      axis(2,at=c(0,35),labels=c(0,35),cex.axis=2)
      abline(v=-25,lty=2)
      abline(v=25,lty=2)
    }
  }
  dev.off()
}


# Narrow
narrow <- cdata %>% filter(file=="Narrow" & !grepl("_R_",sampler)) %>% group_by(sampler,splityear) 
plot_fn(narrow,"Fig4.pdf")

wide <- cdata %>% filter(file=="Wide" & !grepl("_R_",sampler)) %>% group_by(sampler,splityear) 
plot_fn(wide,"Fig5.pdf")


