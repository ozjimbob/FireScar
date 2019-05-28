## firescar with mortality

library(compiler)


## Function, compatible with parallell backend, to launch simulation with defined parameters and save results.
launch_sim=function(idx){
  print(idx)
  dim(sim_def_frame)
  this_sim=sim_def_frame[idx,]
  
  output=scar_sim(this_sim$SIM_ID,this_sim$FIELD_SIZE,this_sim$SIM_LENGTH,this_sim$FIRE_FREQ,this_sim$FIRE_PROB,this_sim$MORTALITY_ON,this_sim$F_MORTALITY_ON,
                  this_sim$MORT_b1,this_sim$MORT_b2,this_sim$MORT_p1,this_sim$MORT_p2,this_sim$MORT_p3,
                  this_sim$MORT_F_b1, this_sim$MORT_F_b2,this_sim$MORT_F_b3, this_sim$MORT_F_p1,this_sim$MORT_F_p2,this_sim$MORT_F_p3, this_sim$MORT_F_p4)
  write.csv(output$tree_frame,paste0(this_sim$output_dir,"/tree_frame_",idx,".csv"))
  this_fire=output$fire_record
  save(this_fire,file=paste0(this_sim$output_dir,"/fire_record_",idx,".Rdata"))
  this_veg_matrix=output$veg_matrix
  save(this_veg_matrix,file=paste0(this_sim$output_dir,"/veg_matrix_",idx,".Rdata"))
}


scar_sim=function(SIM_ID,FIELD_SIZE,SIM_LENGTH,FIRE_FREQ,FIRE_PROB,MORTALITY_ON,F_MORTALITY_ON,
                  MORT_b1,MORT_b2,MORT_p1,MORT_p2, MORT_p3,
                  MORT_F_b1,MORT_F_b2,MORT_F_b3,MORT_F_p1,MORT_F_p2,MORT_F_p3,MORT_F_p4){
  veg_matrix=matrix(0,nrow=FIELD_SIZE,ncol=FIELD_SIZE) # Stores current age of trees
  dead_veg_matrix=matrix(0,nrow=FIELD_SIZE,ncol=FIELD_SIZE) # Stores age of trees at death for dead records
  year_of_death_matrix=matrix(0,nrow=FIELD_SIZE,ncol=FIELD_SIZE) # Stores year dead record trees died
  fire_matrix=matrix(5,nrow=FIELD_SIZE,ncol=FIELD_SIZE) # Stores time since last fire - initialized to 5
  
  storage_matrix=rep(0,FIELD_SIZE*FIELD_SIZE*SIM_LENGTH) # Stores history of scars
  dim(storage_matrix)=c(FIELD_SIZE,FIELD_SIZE,SIM_LENGTH) 
  
  fire_boundary_matrix=rep(0,FIELD_SIZE*FIELD_SIZE*SIM_LENGTH) # Stores history of actual fire areas
  dim(fire_boundary_matrix)=c(FIELD_SIZE,FIELD_SIZE,SIM_LENGTH) 
  
  dead_tree_matrix=rep(0,FIELD_SIZE*FIELD_SIZE*SIM_LENGTH) # Dead trees are moved here until another fire comes to destory snag
  dim(dead_tree_matrix)=c(FIELD_SIZE,FIELD_SIZE,SIM_LENGTH) 
  
  to_die_block=rep(0,FIELD_SIZE*FIELD_SIZE*SIM_LENGTH)
  dim(to_die_block)=c(FIELD_SIZE,FIELD_SIZE,SIM_LENGTH) 
  
  demo_hist=matrix(0,ncol=100,nrow=SIM_LENGTH)
  
  # Constant for number of cells
  COUNT_TREES=length(veg_matrix) 
  
  # Quadratic helper function
  #  quad=function(a,b,c,x){a*x^2+b*x+c}
  #  qq=function(x){2.50e-07*(x^2) -1.25e-04*x + 2.00e-02}
  # Distribution function for tree age
  # Returns a vector of length x with tree ages from a gamma distribution
  TREE_DIST=function(x){round(rgamma(x,6,.021))+1} 
  
  # Probability function - give a vector containing tree ages
  # It will return whether a burn scar was recorded
  # It's currently a logistic curve with a steep transition around age=5
  # So trees younger than 5 years don't record scar (eg. killed)
  #BURN_SCAR_TSF=function(x){runif(x,0,1)<plogis(x,5,.3)}
  BURN_SCAR_TSF=function(x){runif(x,0,1)<plogis(x,20.7,9.5)*0.75}
  
  # Function of tree mortality at a given age, independent of fire
  
  if(MORTALITY_ON){
    #MORTALITY = function(x){runif(x,0,1)<plogis(x,350,35)*.01+0.01} #Logistic curve
    MORTALITY=function(x){
      x[x<MORT_b1]=MORT_p1
      x[x>= MORT_b1 & x <MORT_b2]=MORT_p2
      x[x>=MORT_b2]=MORT_p3
      runif(x,0,1)<x
    }
  }else{
    MORTALITY = function(x){x<0} # Fast way to return a false matrix/vector irregardless of dimension
  }
  
  # Function of tree mortality at a given age, due to fire
  #MORTALITY_F = function(x){runif(x,0,1)<(1-plogis(x,30,10))}
  #MORTALITY_F = function(x){runif(x,0,1)<(1-plogis(x,30,10)*.8-0.15)}
  if(F_MORTALITY_ON){
    #MORTALITY_F = function(x){runif(x,0,1)<(1-plogis(x,30,10)*.8-0.199)}
    MORTALITY_F=function(x){
      x[x<MORT_F_b1]=MORT_F_p1
      x[x>= MORT_F_b1 & x <MORT_F_b2]=MORT_F_p2
      x[x>= MORT_F_b2 & x <MORT_F_b3]=MORT_F_p3
      x[x>=MORT_F_b3]=MORT_F_p4
      runif(x,0,1)<x
    }
  }else{
    MORTALITY_F = function(x){x<0} # Fast way to return a false matrix/vector irregardless of dimension
  }
  # Probability of a tree without a scar forming an initial scar and starting recording
  INITIAL_SCAR_P = 0.08
  
  # Initialize the veg matrix with gamma distribution of tree age
  veg_matrix[1:COUNT_TREES]=TREE_DIST(COUNT_TREES)
  
  # utility function to determine if any drill of the 3D matrix has a scar
  is_one=function(x){sum(x)>1}
  
  # Function to generate a rectangular burn at random location
  # It then feeds this burn to the burn scar function
  # Based on the age of trees in that burn area it returns
  # A matrix showing which trees a scar was recorded on
  # The fire matrix (time since fire) is updated and set to zero where
  # the burn occured
  burn_veg=function(){
    x=veg_matrix                            # Complete matrix of vegetation age
    y=fire_matrix                           # Complete matrix of time since fire
    
    prob_burn_field=matrix(runif(COUNT_TREES,0,1)<(FIRE_PROB/100),nrow=FIELD_SIZE,ncol=FIELD_SIZE) #Fire pixels as logic to allow use as cell selector
    
    scar_map=x                              # Create a copy of the matrix to store the scar map  
    fire_map=x                              # Create a copy of the matrix to store the total fire  
    scar_map[1:COUNT_TREES]=FALSE           # Set all values of the scar map to false
    fire_map[1:COUNT_TREES]=0               # Set all values of the fire map to false
    
    this_fire=y[prob_burn_field]        # this_fire contains the matrix subset burnt by the fire of time since burnt
    this_scar=BURN_SCAR_TSF(this_fire)      # For each cell in this, test if they are actually burnt, store in this_scar
    this_any_scar=any_scar[prob_burn_field]  # Grab subset of any_scar to combine with new scar formation
    
    this_scar=this_scar * this_any_scar          # only allow trees that are "1" in any_scar to form a scar, set rest to zero
    
    scar_map[prob_burn_field]=this_scar # Put this back in the scar map
    fire_map[prob_burn_field]=1         # Save a mask of the complete fire
    y[prob_burn_field]=0                # For area within the fire boundary, set time since fire to 0
    fire_matrix<<-y                         # Write this updated fire matrix to global variable
    
    ### Do vegetation mortality due to fire
    rec_veg=x                               # Make a copy of the vegetation age
    this_veg=rec_veg[prob_burn_field]   # Grab a subset where the fire burnt
    trees_to_die=MORTALITY_F(this_veg)      # Make a logical subset matrix to show which trees were killed by fire
    rec_veg[1:COUNT_TREES]=FALSE                # Blank out the overall veg age copy matrix
    rec_veg[prob_burn_field] = trees_to_die   # Put in just the trees that have just been killed, so their record can be wiped
    
    list(scar_map=scar_map,fire_map=fire_map,to_die=rec_veg)
  }
  
  
  
  # Run simulation for given number of years
  for(idx in 1:SIM_LENGTH){
    
    any_scar=apply(storage_matrix,MARGIN=c(1,2),FUN=is_one)  # Create a matrix showing if tree has ANY scar already
    new_scar_probability=matrix(sample(c(0,1),COUNT_TREES,replace=TRUE,prob=c(1-INITIAL_SCAR_P,INITIAL_SCAR_P)),nrow=FIELD_SIZE,ncol=FIELD_SIZE) # Probability of recording tree activating
    any_scar = matrix(as.numeric(any_scar | new_scar_probability),nrow=FIELD_SIZE,ncol=FIELD_SIZE)  # Combine this probability with existing scars to produce matrix of trees capable of being scarred
    fire_matrix=fire_matrix+1 # Age the time since fire by 1 year
    veg_matrix=veg_matrix+1 # Age the vegetation by 1 year
    
    # If it's a "fire year", burn an area
    if(runif(1,0,1) < FIRE_FREQ/100){
      
      old_tsf=fire_matrix
      burn_it = burn_veg()             # Burn function 
      to_die_fire = burn_it$to_die     # Read out the trees to die in the fire
      total_fire_map=burn_it$fire_map            
      
      # Erase the records from the dead_tree_matrix that were just burnt
      inverse_fire_map=!total_fire_map
      inverse_fire_store=rep(inverse_fire_map,SIM_LENGTH)
      dim(inverse_fire_store)=c(FIELD_SIZE,FIELD_SIZE,SIM_LENGTH)  # Make it 3D
      
      # Keep tree with probability base on time since fire
      inverse_fire_keepsome = inverse_fire_map | BURN_SCAR_TSF(old_tsf)
      inverse_fire_keepsome_store=rep(inverse_fire_keepsome,SIM_LENGTH)
      dim(inverse_fire_keepsome_store)=c(FIELD_SIZE,FIELD_SIZE,SIM_LENGTH) 
      
      dead_tree_matrix=dead_tree_matrix*inverse_fire_keepsome_store  # Wipe fire scare area in dead tree store
      dead_veg_matrix = dead_veg_matrix*inverse_fire_keepsome  #Set tree age to zero in dead veg matrix
      year_of_death_matrix = year_of_death_matrix*inverse_fire_keepsome # Set year burnt to zero in year died matrix
      
      storage_matrix[,,idx] = burn_it$scar_map # Store the burn matrix in a 3D matrix - do this now so we get the scar coincident with death
      
      fire_boundary_matrix[,,idx] = total_fire_map # Store the actual fire matrix in 3D matrix
      
    } else{
      to_die_fire = veg_matrix              # If there's no fire, we still need a 
      to_die_fire[1:COUNT_TREES] = FALSE    # blank died-in-fire matrix
    }
    # Natural mortality
    to_die_naturally = MORTALITY(veg_matrix)        # Natural mortality matrix based on age probability
    #fire_matrix[to_die_naturally]=0                 # If they're dead, there's also no "time since fire"??
    to_die_total = to_die_fire | to_die_naturally    # Trees killed by fire OR natrually
    
    
    # the slow way! Copy dead tree records into the dead tree store
    for(x in 1:FIELD_SIZE){
      for(y in 1:FIELD_SIZE){
        if(to_die_total[x,y]==1){
          dead_tree_matrix[x,y,]=storage_matrix[x,y,]
          dead_veg_matrix[x,y]=veg_matrix[x,y]
          year_of_death_matrix[x,y]=idx
        }
      }
    }
    
    veg_matrix[to_die_total]=0                  # Kill them (again, reset to zero)
    
    to_die_total= 1-to_die_total                # Make it multipliable eg. x0 = dead, x1 = alive
    to_die_block=rep(to_die_total,SIM_LENGTH)    # Repeat that dead tree multiplier throughout storage matrix
    dim(to_die_block)=c(FIELD_SIZE,FIELD_SIZE,SIM_LENGTH)  # Make it 3D
    storage_matrix=storage_matrix*to_die_block        # Update storage matrix with new dead tree holes
    #demo_hist[idx,]=hist(veg_matrix,breaks=seq(0,SIM_LENGTH,SIM_LENGTH/100),plot=FALSE)$counts # Save a row in the demographic histogram
  }
  
  
  
  # Plot of how many burn scars were recorded
  scar_count=apply(storage_matrix,MARGIN=c(1,2),FUN=sum)
  #image(scar_count)
  
  dead_scar_count=apply(dead_tree_matrix,MARGIN=c(1,2),FUN=sum)
  #image(dead_scar_count)
  
  total_count=apply(fire_boundary_matrix,MARGIN=c(1,2),FUN=sum)
  #image(total_count)
  
  # Put all the still living - note, need a different method if mortality is turned off
  
  tree_frame=data.frame(ID=0,X=0,Y=0,YBP=0,STATUS=0,START_YEAR=0,END_YEAR=0)
  ID=0
  
  for(x in 1:FIELD_SIZE){
    
    for(y in 1:FIELD_SIZE){
      if(scar_count[x,y]>=1){
        ID=ID+1
        this_age=veg_matrix[x,y]
        if(this_age > SIM_LENGTH){
          this_age=SIM_LENGTH       # Just get rid of records past length of simulation
        }
        this_scarvec=storage_matrix[x,y,((SIM_LENGTH-this_age)+1):(SIM_LENGTH)]
        born=SIM_LENGTH - this_age
        this_age_vec=(born+1):SIM_LENGTH
        this_frame=data.frame(ID=rep(ID,this_age),X=rep(x,this_age),Y=rep(y,this_age),SCAR=this_scarvec,YBP=this_age_vec,STATUS=rep(0,this_age),START_YEAR=rep(born,this_age),END_YEAR=rep(SIM_LENGTH,this_age))
        this_frame=subset(this_frame,this_frame$SCAR==1,select=c("ID","X","Y","YBP","STATUS","START_YEAR","END_YEAR"))
        tree_frame=rbind(tree_frame,this_frame)
      }
    }
  }
  
  tree_frame=subset(tree_frame,ID!=0)
  if(MORTALITY_ON){
    for(x in 1:FIELD_SIZE){
      
      for(y in 1:FIELD_SIZE){
        if(dead_scar_count[x,y]>=1){
          ID=ID+1
          this_age=dead_veg_matrix[x,y]
          this_year_died=year_of_death_matrix[x,y]
          born=this_year_died - this_age
          if(born < 1){
            born=0
            
          }
          this_scarvec=dead_tree_matrix[x,y,((born)+1):(this_year_died)]
          this_age_vec=(born+1):this_year_died
          vec_length=length(this_age_vec)
          this_frame=data.frame(ID=rep(ID,vec_length),X=rep(x,vec_length),Y=rep(y,vec_length),SCAR=this_scarvec,YBP=this_age_vec,STATUS=rep(1,vec_length),START_YEAR=rep(born,vec_length),END_YEAR=rep(this_year_died,vec_length))
          this_frame=subset(this_frame,this_frame$SCAR==1,select=c("ID","X","Y","YBP","STATUS","START_YEAR","END_YEAR"))
          tree_frame=rbind(tree_frame,this_frame)
        }
      }
    }
  }
  
  # Add some extra useful columns to the frame
  tree_frame$AGE=tree_frame$END_YEAR-tree_frame$START_YEAR
  
  tree_frame$SIM_ID = SIM_ID
  tree_frame$MORT_b1 = MORT_b1
  tree_frame$MORT_b2 = MORT_b2
  tree_frame$MORT_p1 = MORT_p1
  tree_frame$MORT_p2 = MORT_p2
  tree_frame$MORT_p3 = MORT_p2
  tree_frame$MORT_F_b1 = MORT_F_b1
  tree_frame$MORT_F_b2 = MORT_F_b2
  tree_frame$MORT_F_b3 = MORT_F_b3
  tree_frame$MORT_F_p1 = MORT_F_p1
  tree_frame$MORT_F_p2 = MORT_F_p2
  tree_frame$MORT_F_p3 = MORT_F_p3
  tree_frame$MORT_F_p4 = MORT_F_p4
  tree_frame$FIELD_SIZE=FIELD_SIZE                     
  tree_frame$SIM_LENGTH=SIM_LENGTH     
  tree_frame$FIRE_FREQ=FIRE_FREQ 
  tree_frame$FIRE_PROB=FIRE_PROB                      
  tree_frame$MORTALITY_ON=MORTALITY_ON                   
  tree_frame$F_MORTALITY_ON=F_MORTALITY_ON                  
  
  
  # Return all the output defining this model
  list(tree_frame=tree_frame,
       fire_record=fire_boundary_matrix,
       veg_matrix=veg_matrix)
}



plot_dendro=function(tree_frame){
  
  sim_end=max(tree_frame$END_YEAR)
  
  tree_frame$START_YEAR = -(sim_end-tree_frame$START_YEAR)
  tree_frame$END_YEAR = -(sim_end-tree_frame$END_YEAR)
  tree_frame$YBP = -(sim_end-tree_frame$YBP)
  
  number_of_trees=length(unique(tree_frame$ID))
  plot.new()
  plot.window(xlim=c(-sim_end,0),ylim=c(0,number_of_trees))
  for(idx in 1:number_of_trees){
    this_tree=subset(tree_frame,ID==idx)
    first_scar=min(this_tree$YBP)
    if(this_tree$STATUS[1]==0){
      col_vec=c("grey","black")
    }else{
      col_vec=c("pink","red")
    }
    lines(c(this_tree$START_YEAR[1],first_scar),c(idx,idx),col=col_vec[1])
    lines(c(first_scar,this_tree$END_YEAR[1]),c(idx,idx),col=col_vec[2])
    points(this_tree$YBP,rep(idx,length(this_tree$YBP)),pch="|",col=col_vec[2],cex=0.5)
  }
  axis(1)
}

scar_sim=cmpfun(scar_sim)
plot_dendro=cmpfun(plot_dendro)


