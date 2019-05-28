### Summary Functions
# Contains functions for calculating various dendro fire indices based on defined samples of the simulation output

library(compiler)

# Calculate Grid Real Mean Fire Interval  
f_RealMFI=function(x,z){
  fsz=dim(x)[1] * dim(x)[2]
  fc=apply(x,MARGIN=3,FUN=sum)
  fc=fc> (fsz * (z/100))
  bc=sum(fc)/length(fc)
  1/bc
}

f_RealMFI=cmpfun(f_RealMFI)

# With a fixed tree threshold
f_RealMFIf=function(x,z){
  fsz=dim(x)[1] * dim(x)[2]
  fc=apply(x,MARGIN=3,FUN=sum)
  fc=fc>=z
  bc=sum(fc)/length(fc)
  1/bc
}

f_RealMFIf=cmpfun(f_RealMFIf)

# Calculate Grid Real Rotation
f_RealNFR=function(x){
  cells=dim(x)[1] * dim(x)[2]
  years=dim(x)[3]
  smat=sum(x)
  NFR=(smat/(cells*years)) * 100
}

f_RealNFR=cmpfun(f_RealNFR)

# For a 
f_tree_MFIhelper=function(x){
  dx=diff(x)
  mean(dx)
}

f_tree_MFIhelper=cmpfun(f_tree_MFIhelper)


#### DendroNFR Calculation
#### Have a frame with column for the present year, number of trees recording, number of scars

f_tree_NFR=function(inp_frame,this_fire){
  NFR_Frame=data.frame(year=seq(1:dim(this_fire)[3]),recording=0,scars=0)
  for(jdx in seq_along(NFR_Frame$year)){
    number_recording=length(subset(inp_frame,inp_frame$YBP<=jdx)$YBP)
    number_scars=length(subset(inp_frame,inp_frame$YBP==jdx)$YBP)
    NFR_Frame$recording[jdx]=number_recording
    NFR_Frame$scars[jdx]=number_scars
  }
  
  ## Filter years with <5% recording trees
  NFR_Frame=subset(NFR_Frame,NFR_Frame$recording>5)
  
  length_of_record=dim(NFR_Frame)[1]
  
  NFR_Frame$perc=(NFR_Frame$scars / NFR_Frame$recording)*100
  sperc=sum(NFR_Frame$perc)
  out_NFR=sperc/length_of_record
  out_NFR
}
f_tree_NFR=cmpfun(f_tree_NFR)

# Return a frame of trees with a given percentage of scarring per year
f_tree_percscar=function(inp_frame,tperc,this_fire){
  NFR_Frame=data.frame(year=seq(1:dim(this_fire)[3]),recording=0,scars=0)
  for(jdx in seq_along(NFR_Frame$year)){
    number_recording=length(subset(inp_frame,inp_frame$YBP<=jdx)$YBP)
    number_scars=length(subset(inp_frame,inp_frame$YBP==jdx)$YBP)
    NFR_Frame$recording[jdx]=number_recording
    NFR_Frame$scars[jdx]=number_scars
  }
  NFR_Frame=subset(NFR_Frame,NFR_Frame$recording>5)
  NFR_Frame$perc=(NFR_Frame$scars / NFR_Frame$recording)*100
  NFR_Frame=subset(NFR_Frame,NFR_Frame$perc > tperc)
  NFR_Frame
}

f_tree_percscar=cmpfun(f_tree_percscar)



# Return a frame of trees with a given percentage of scarring per year
f_tree_percscarf=function(inp_frame,tperc,this_fire){
  NFR_Frame=data.frame(year=seq(1:dim(this_fire)[3]),recording=0,scars=0)
  for(jdx in seq_along(NFR_Frame$year)){
    number_recording=length(subset(inp_frame,inp_frame$YBP<=jdx)$YBP)
    number_scars=length(subset(inp_frame,inp_frame$YBP==jdx)$YBP)
    NFR_Frame$recording[jdx]=number_recording
    NFR_Frame$scars[jdx]=number_scars
  }
  NFR_Frame=subset(NFR_Frame,NFR_Frame$recording>5)
  NFR_Frame=subset(NFR_Frame,NFR_Frame$scars >= tperc)
  NFR_Frame$perc=(NFR_Frame$scars / NFR_Frame$recording)*100
  NFR_Frame=subset(NFR_Frame,NFR_Frame$perc > 0)
  NFR_Frame
}

f_tree_percscarf=cmpfun(f_tree_percscarf)


### Time-series of fire years
# Return a frame of trees with a given percentage of scarring per year
f_tree_tsyear=function(inp_frame,this_fire){
  NFR_Frame=data.frame(year=seq(1:dim(this_fire)[3]),recording=0,scars=0)
  for(jdx in seq_along(NFR_Frame$year)){
    number_recording=length(subset(inp_frame,inp_frame$YBP<=jdx)$YBP)
    number_scars=length(subset(inp_frame,inp_frame$YBP==jdx)$YBP)
    NFR_Frame$recording[jdx]=number_recording
    NFR_Frame$scars[jdx]=number_scars
  }
  NFR_Frame$perc=NFR_Frame$scars / NFR_Frame$recording
  NFR_Frame
}

f_tree_tsyear=cmpfun(f_tree_tsyear)