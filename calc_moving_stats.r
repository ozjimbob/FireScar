
## Calculate timeseries stats, moving across timeseries to detect changepoint in experiment 2
## Launch model

library(doParallel)
library(foreach)
library(iterators)
library(dplyr)
library(zoo)

source("summary_functions.R")


registerDoParallel(cores=4)



calc_stats=function(wd,run,count_dead=TRUE){
  print(run)
  data_frame=read.csv(paste0(wd,run,"/sim_list.csv"))
  
  FD_frame=data.frame(ID=0,year=0,SPLIT_YEAR = 0,FD_RealSeries=0,FD_CensusSeries=0,FD_T_500=0,FD_T_250=0,FD_T_100=0,FD_T_50=0,FD_T_25=0,FD_T_10=0,FD_R_500=0,FD_R_250=0,FD_R_100=0,FD_R_50=0,FD_R_25=0,FD_R_10=0)
  AB_frame=data.frame(ID=0,year=0,SPLIT_YEAR = 0,AB_RealSeries=0,AB_CensusSeries=0,AB_T_500=0,AB_T_250=0,AB_T_100=0,AB_T_50=0,AB_T_25=0,AB_T_10=0,AB_R_500=0,AB_R_250=0,AB_R_100=0,AB_R_50=0,AB_R_25=0,AB_R_10=0)
  
  for(idx in 1:dim(data_frame)[1]){
    print(paste0("__",idx))
    print(idx)
    
    aa=load(paste0(wd,run,"/fire_record_",idx,".Rdata"),env=environment())
    this_tree=read.csv(paste0(wd,run,"/tree_frame_",idx,".csv"))
    
    if(!count_dead){
      this_tree=subset(this_tree,STATUS==0)
    }
    
    ## Real count of fires per decade.
    
    field_size=dim(this_fire)[1] * dim(this_fire)[2]
    fc=apply(this_fire,MARGIN=3,FUN=sum)
    fc=fc>0
    g=rollapply(fc,10,sum)
    
    ###
    FD_RealSeries=g
    FD_CensusSeries=rollapply(f_tree_tsyear(this_tree,this_fire)$scars>0,10,sum)
    ###
    
    
    # Shuffle this_tree so the ordered trees have a random subset within them, or else all the dead ones will be at the bottom
    this_tree$shuffler=sample(dim(this_tree)[1])
    
    ## Calculate a summary of the number of scars per tree, for filtering
    this_tree=group_by(this_tree,ID)
    
    scar_count=summarise(this_tree,count=length(YBP),MFI=f_tree_MFIhelper(YBP),shuffler=first(shuffler))
    scar_count=arrange(scar_count,desc(count),shuffler)
    scar_count=as.data.frame(scar_count)
    scar_count=subset(scar_count,is.finite(scar_count$MFI))
    
    this_tree=as.data.frame(this_tree)
    
    ## Targeted subsets
    FD_T_500 = rollapply(f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:500,]$ID),this_fire)$scars>0,10,sum)
    FD_T_250 = rollapply(f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:250,]$ID),this_fire)$scars>0,10,sum)
    FD_T_100 = rollapply(f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:100,]$ID),this_fire)$scars>0,10,sum)
    FD_T_50 = rollapply(f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:50,]$ID),this_fire)$scars>0,10,sum)
    FD_T_25 = rollapply(f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:25,]$ID),this_fire)$scars>0,10,sum)
    FD_T_10 = rollapply(f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:10,]$ID),this_fire)$scars>0,10,sum)
    
    ## Random subsets
    FD_R_500 = rollapply(f_tree_tsyear(this_tree[sample(dim(this_tree)[1],500),],this_fire)$scars>0,10,sum)
    FD_R_250 = rollapply(f_tree_tsyear(this_tree[sample(dim(this_tree)[1],250),],this_fire)$scars>0,10,sum)
    FD_R_100 = rollapply(f_tree_tsyear(this_tree[sample(dim(this_tree)[1],100),],this_fire)$scars>0,10,sum)
    FD_R_50 = rollapply(f_tree_tsyear(this_tree[sample(dim(this_tree)[1],50),],this_fire)$scars>0,10,sum)
    FD_R_25 = rollapply(f_tree_tsyear(this_tree[sample(dim(this_tree)[1],25),],this_fire)$scars>0,10,sum)
    FD_R_10 = rollapply(f_tree_tsyear(this_tree[sample(dim(this_tree)[1],10),],this_fire)$scars>0,10,sum)
    
    ################### Now area burnt
    
    field_size=dim(this_fire)[1] * dim(this_fire)[2]
    fc=apply(this_fire,MARGIN=3,FUN=sum)
    g=fc/field_size
    
    ###
    AB_RealSeries=g
    AB_CensusSeries=f_tree_tsyear(this_tree,this_fire)$perc
    ###
    
    AB_T_500 = f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:500,]$ID),this_fire)$perc
    AB_T_250 = f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:250,]$ID),this_fire)$perc
    AB_T_100 = f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:100,]$ID),this_fire)$perc
    AB_T_50 = f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:50,]$ID),this_fire)$perc
    AB_T_25 = f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:25,]$ID),this_fire)$perc
    AB_T_10 = f_tree_tsyear(subset(this_tree,this_tree$ID %in% scar_count[1:10,]$ID),this_fire)$perc
    
    ## Random subsets
    AB_R_500 = f_tree_tsyear(this_tree[sample(dim(this_tree)[1],500),],this_fire)$perc
    AB_R_250 = f_tree_tsyear(this_tree[sample(dim(this_tree)[1],250),],this_fire)$perc
    AB_R_100 = f_tree_tsyear(this_tree[sample(dim(this_tree)[1],100),],this_fire)$perc
    AB_R_50 = f_tree_tsyear(this_tree[sample(dim(this_tree)[1],50),],this_fire)$perc
    AB_R_25 = f_tree_tsyear(this_tree[sample(dim(this_tree)[1],25),],this_fire)$perc
    AB_R_10 = f_tree_tsyear(this_tree[sample(dim(this_tree)[1],10),],this_fire)$perc
    
    ## bind it
    
    t_FD=data.frame(ID=rep(idx,length(FD_RealSeries)),year=seq_along(FD_RealSeries),SPLIT_YEAR=rep(data_frame$SPLIT_YEAR[idx],length(FD_RealSeries)),FD_RealSeries,FD_CensusSeries,FD_T_500,FD_T_250,FD_T_100,FD_T_50,FD_T_25,FD_T_10,FD_R_500,FD_R_250,FD_R_100,FD_R_50,FD_R_25,FD_R_10)
    t_AB=data.frame(ID=rep(idx,length(AB_RealSeries)),year=seq_along(AB_RealSeries),SPLIT_YEAR=rep(data_frame$SPLIT_YEAR[idx],length(AB_RealSeries)),AB_RealSeries,AB_CensusSeries,AB_T_500,AB_T_250,AB_T_100,AB_T_50,AB_T_25,AB_T_10,AB_R_500,AB_R_250,AB_R_100,AB_R_50,AB_R_25,AB_R_10)
    
    FD_frame=rbind(FD_frame,t_FD)
    AB_frame=rbind(AB_frame,t_AB)
  }
  
  FD_frame=subset(FD_frame,ID>0)
  AB_frame=subset(AB_frame,ID>0)
  
  write.csv(FD_frame,paste0(wd,run,"/FD_timeseries_",count_dead,".csv"))
  write.csv(AB_frame,paste0(wd,run,"/AB_timeseries_",count_dead,".csv"))
}




# Do the narrow/wide 
td_vec=c("e2_range_narrow","e2_range_wide")
foreach(i=iter(td_vec),.packages=c("dplyr","zoo")) %dopar% calc_stats(working_dir,i)

#for(i in td_vec){
#  print(i)
#  calc_stats(working_dir,i)
#}

