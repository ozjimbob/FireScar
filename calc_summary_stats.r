# Calculates summary stats (eg. MFI 10%, MFI 25%) on the simulation output
# for fixed regime simulations. Outputs a new file (sim_list_stats1.csv) in each simulation output directory
# with statistics calculated for each simulation run


library(dplyr)

source("summary_functions.r")

calc_stats=function(wd,run){
  print(run)
  data_frame=read.csv(paste0(wd,run,"/sim_list.csv"))
  
  data_frame$GridRealMFI=0
  data_frame$GridRealNFR=0
  data_frame$NFR_census=0
  data_frame$NFR_500=0
  data_frame$NFR_250=0
  data_frame$NFR_100=0
  data_frame$NFR_50=0
  data_frame$NFR_25=0
  data_frame$NFR_10=0
  data_frame$NFR_500_T=0
  data_frame$NFR_250_T=0
  data_frame$NFR_100_T=0
  data_frame$NFR_50_T=0
  data_frame$NFR_25_T=0
  data_frame$NFR_10_T=0
  
  data_frame$MFI_census=0
  data_frame$MFI_500=0
  data_frame$MFI_250=0
  data_frame$MFI_100=0
  data_frame$MFI_50=0
  data_frame$MFI_25=0
  data_frame$MFI_10=0
  data_frame$MFI_500_T=0
  data_frame$MFI_250_T=0
  data_frame$MFI_100_T=0
  data_frame$MFI_50_T=0
  data_frame$MFI_25_T=0
  data_frame$MFI_10_T=0
  
  data_frame$MFI_grid_10=0
  data_frame$MFI_census_10=0
  
  data_frame$MFI_500_10=0
  data_frame$MFI_250_10=0
  data_frame$MFI_100_10=0
  data_frame$MFI_50_10=0
  data_frame$MFI_25_10=0
  data_frame$MFI_10_10=0
  
  data_frame$MFI_500_T_10=0
  data_frame$MFI_250_T_10=0
  data_frame$MFI_100_T_10=0
  data_frame$MFI_50_T_10=0
  data_frame$MFI_25_T_10=0
  data_frame$MFI_10_T_10=0
  
  data_frame$MFI_grid_25=0
  data_frame$MFI_census_25=0
  
  data_frame$MFI_500_25=0
  data_frame$MFI_250_25=0
  data_frame$MFI_100_25=0
  data_frame$MFI_50_25=0
  data_frame$MFI_25_25=0
  data_frame$MFI_10_25=0
  
  data_frame$MFI_500_T_25=0
  data_frame$MFI_250_T_25=0
  data_frame$MFI_100_T_25=0
  data_frame$MFI_50_T_25=0
  data_frame$MFI_25_T_25=0
  data_frame$MFI_10_T_25=0
  
  data_frame$MFI_grid_2o=0
  data_frame$MFI_census_2o=0
  
  data_frame$MFI_500_2o=0
  data_frame$MFI_250_2o=0
  data_frame$MFI_100_2o=0
  data_frame$MFI_50_2o=0
  data_frame$MFI_25_2o=0
  data_frame$MFI_10_2o=0
  
  data_frame$MFI_500_T_2o=0
  data_frame$MFI_250_T_2o=0
  data_frame$MFI_100_T_2o=0
  data_frame$MFI_50_T_2o=0
  data_frame$MFI_25_T_2o=0
  data_frame$MFI_10_T_2o=0 
  
  
  count_dead=TRUE
  for(idx in 1:dim(data_frame)[1]){
    print(idx)
    
    aa=load(paste0(wd,run,"/fire_record_",idx,".Rdata"),env=environment())
    
    this_tree=read.csv(paste0(wd,run,"/tree_frame_",idx,".csv"))
    
    if(!count_dead){
      this_tree=subset(this_tree,STATUS==0)
    }
    
    ## Real Grid Statistics
    
    data_frame$GridRealMFI[idx]=f_RealMFI(this_fire,0)
    data_frame$GridRealNFR[idx]=f_RealNFR(this_fire)
    data_frame$MFI_grid_10[idx]=f_RealMFI(this_fire,10)
    data_frame$MFI_grid_25[idx]=f_RealMFI(this_fire,25)
    data_frame$MFI_grid_2o[idx]=f_RealMFIf(this_fire,2)
    
    # Shuffle this_tree so the ordered trees have a random subset within them, or else all the dead ones will be at the bottom
    this_tree$shuffler=sample(dim(this_tree)[1])
    
    ## Calculate a summary of the number of scars per tree, for filtering
    this_tree=group_by(this_tree,ID)
    
    scar_count=summarise(this_tree,count=length(YBP),MFI=f_tree_MFIhelper(YBP),shuffler=first(shuffler))
    scar_count=arrange(scar_count,desc(count),shuffler)
    
    ## NFR for random samples
    this_tree=as.data.frame(this_tree)
    data_frame$NFR_census[idx]=f_tree_NFR(this_tree,this_fire)
    data_frame$NFR_500[idx]=f_tree_NFR(this_tree[sample(dim(this_tree)[1],500),],this_fire)
    data_frame$NFR_250[idx]=f_tree_NFR(this_tree[sample(dim(this_tree)[1],250),],this_fire)
    data_frame$NFR_100[idx]=f_tree_NFR(this_tree[sample(dim(this_tree)[1],100),],this_fire)
    data_frame$NFR_50[idx]=f_tree_NFR(this_tree[sample(dim(this_tree)[1],50),],this_fire)
    data_frame$NFR_25[idx]=f_tree_NFR(this_tree[sample(dim(this_tree)[1],25),],this_fire)
    data_frame$NFR_10[idx]=f_tree_NFR(this_tree[sample(dim(this_tree)[1],10),],this_fire)
    
    data_frame$NFR_500_T[idx]=f_tree_NFR(subset(this_tree,this_tree$ID %in% scar_count[1:500,]$ID),this_fire)
    data_frame$NFR_250_T[idx]=f_tree_NFR(subset(this_tree,this_tree$ID %in% scar_count[1:250,]$ID),this_fire)
    data_frame$NFR_100_T[idx]=f_tree_NFR(subset(this_tree,this_tree$ID %in% scar_count[1:100,]$ID),this_fire)
    data_frame$NFR_50_T[idx]=f_tree_NFR(subset(this_tree,this_tree$ID %in% scar_count[1:50,]$ID),this_fire)
    data_frame$NFR_25_T[idx]=f_tree_NFR(subset(this_tree,this_tree$ID %in% scar_count[1:25,]$ID),this_fire)
    data_frame$NFR_10_T[idx]=f_tree_NFR(subset(this_tree,this_tree$ID %in% scar_count[1:10,]$ID),this_fire)
    
    scar_count=as.data.frame(scar_count)
    scar_count=subset(scar_count,is.finite(scar_count$MFI))
    
    all_tree_scaryears=f_tree_percscar(this_tree,0,this_fire)
    
    # MFI for all trees with no synchronicity required
    data_frame$MFI_census[idx]=mean(diff(all_tree_scaryears$year))
    data_frame$MFI_500[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],500),],0,this_fire)$year))
    data_frame$MFI_250[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],250),],0,this_fire)$year))
    data_frame$MFI_100[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],100),],0,this_fire)$year))
    data_frame$MFI_50[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],50),],0,this_fire)$year))
    data_frame$MFI_25[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],25),],0,this_fire)$year))
    data_frame$MFI_10[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],10),],0,this_fire)$year))
    
    data_frame$MFI_500_T[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:500,]$ID),0,this_fire)$year))
    data_frame$MFI_250_T[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:250,]$ID),0,this_fire)$year))
    data_frame$MFI_100_T[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:100,]$ID),0,this_fire)$year))
    data_frame$MFI_50_T[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:50,]$ID),0,this_fire)$year))
    data_frame$MFI_25_T[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:25,]$ID),0,this_fire)$year))
    data_frame$MFI_10_T[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:10,]$ID),0,this_fire)$year))
    
    # MFI for all trees with 10% synchronicity required
    
    data_frame$MFI_census_10[idx]=mean(diff(f_tree_percscar(this_tree,10,this_fire)$year))
    data_frame$MFI_500_10[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],500),],10,this_fire)$year))
    data_frame$MFI_250_10[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],250),],10,this_fire)$year))
    data_frame$MFI_100_10[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],100),],10,this_fire)$year))
    data_frame$MFI_50_10[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],50),],10,this_fire)$year))
    data_frame$MFI_25_10[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],25),],10,this_fire)$year))
    data_frame$MFI_10_10[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],10),],10,this_fire)$year))
    
    data_frame$MFI_500_T_10[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:500,]$ID),10,this_fire)$year))
    data_frame$MFI_250_T_10[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:250,]$ID),10,this_fire)$year))
    data_frame$MFI_100_T_10[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:100,]$ID),10,this_fire)$year))
    data_frame$MFI_50_T_10[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:50,]$ID),10,this_fire)$year))
    data_frame$MFI_25_T_10[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:25,]$ID),10,this_fire)$year))
    data_frame$MFI_10_T_10[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:10,]$ID),10,this_fire)$year))
    
    
    # MFI for all trees with 25% synchronicity required
    data_frame$MFI_census_25[idx]=mean(diff(f_tree_percscar(this_tree,25,this_fire)$year))
    data_frame$MFI_500_25[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],500),],25,this_fire)$year))
    data_frame$MFI_250_25[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],250),],25,this_fire)$year))
    data_frame$MFI_100_25[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],100),],25,this_fire)$year))
    data_frame$MFI_50_25[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],50),],25,this_fire)$year))
    data_frame$MFI_25_25[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],25),],25,this_fire)$year))
    data_frame$MFI_10_25[idx]=mean(diff(f_tree_percscar(this_tree[sample(dim(this_tree)[1],10),],25,this_fire)$year))
    
    data_frame$MFI_500_T_25[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:500,]$ID),25,this_fire)$year))
    data_frame$MFI_250_T_25[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:250,]$ID),25,this_fire)$year))
    data_frame$MFI_100_T_25[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:100,]$ID),25,this_fire)$year))
    data_frame$MFI_50_T_25[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:50,]$ID),25,this_fire)$year))
    data_frame$MFI_25_T_25[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:25,]$ID),25,this_fire)$year))
    data_frame$MFI_10_T_25[idx]=mean(diff(f_tree_percscar(subset(this_tree,this_tree$ID %in% scar_count[1:10,]$ID),25,this_fire)$year))
    
    ### MFI for trees with more than 2 trees with scars in each year
    
    # MFI for all trees with 10% synchronicity required
    
    data_frame$MFI_census_2o[idx]=mean(diff(f_tree_percscarf(this_tree,10,this_fire)$year))
    data_frame$MFI_500_2o[idx]=mean(diff(f_tree_percscarf(this_tree[sample(dim(this_tree)[1],500),],2,this_fire)$year))
    data_frame$MFI_250_2o[idx]=mean(diff(f_tree_percscarf(this_tree[sample(dim(this_tree)[1],250),],2,this_fire)$year))
    data_frame$MFI_100_2o[idx]=mean(diff(f_tree_percscarf(this_tree[sample(dim(this_tree)[1],100),],2,this_fire)$year))
    data_frame$MFI_50_2o[idx]=mean(diff(f_tree_percscarf(this_tree[sample(dim(this_tree)[1],50),],2,this_fire)$year))
    data_frame$MFI_25_2o[idx]=mean(diff(f_tree_percscarf(this_tree[sample(dim(this_tree)[1],25),],2,this_fire)$year))
    data_frame$MFI_10_2o[idx]=mean(diff(f_tree_percscarf(this_tree[sample(dim(this_tree)[1],10),],2,this_fire)$year))
    
    data_frame$MFI_500_T_2o[idx]=mean(diff(f_tree_percscarf(subset(this_tree,this_tree$ID %in% scar_count[1:500,]$ID),2,this_fire)$year))
    data_frame$MFI_250_T_2o[idx]=mean(diff(f_tree_percscarf(subset(this_tree,this_tree$ID %in% scar_count[1:250,]$ID),2,this_fire)$year))
    data_frame$MFI_100_T_2o[idx]=mean(diff(f_tree_percscarf(subset(this_tree,this_tree$ID %in% scar_count[1:100,]$ID),2,this_fire)$year))
    data_frame$MFI_50_T_2o[idx]=mean(diff(f_tree_percscarf(subset(this_tree,this_tree$ID %in% scar_count[1:50,]$ID),2,this_fire)$year))
    data_frame$MFI_25_T_2o[idx]=mean(diff(f_tree_percscarf(subset(this_tree,this_tree$ID %in% scar_count[1:25,]$ID),2,this_fire)$year))
    data_frame$MFI_10_T_2o[idx]=mean(diff(f_tree_percscarf(subset(this_tree,this_tree$ID %in% scar_count[1:10,]$ID),2,this_fire)$year))
  }
  
  
  write.csv(data_frame,paste0(wd,run,"/sim_list_stats1.csv"))
}


working_dir="E:/nm_scardata/"

library(doParallel)
library(foreach)
library(iterators)

registerDoParallel(cores=12)

td_vec=c("latin_sens","e1_l1","e1_l2","e1_l3","e1_l4")

foreach(i=iter(td_vec),.packages="dplyr") %dopar% calc_stats(working_dir,i)



