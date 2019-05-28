## Launch model
library(lhs)
library(doParallel)
library(foreach)

source("cat_face_mortality_pfire.r")


registerDoParallel(cores=16)

############################################

### Run experiment 0 - latin square generation

############################################

FIELD_SIZE=200                    # Number of rows/columns
SIM_LENGTH=700                    # number of years to run
FIRE_FREQ=20                      #  percentage of years
FIRE_PROB=25                      # Size as %age of field
MORTALITY_ON=TRUE                     # Non-fire Mortality turned on?
F_MORTALITY_ON=TRUE                    # Fire-Morality turned on?

MORT_b1 = 20                      # First mortality age break
MORT_b2 = 400                     # Second mortality age break

MORT_p1 =.02
MORT_p2 =.005
MORT_p3 =.02

MORT_F_b1 = 10                      # First fire mortality age break
MORT_F_b2 = 30                     # Second fire mortality age break
MORT_F_b3 = 400                     # Second fire mortality age break

MORT_F_p1 = 0.7
MORT_F_p2 = 0.3
MORT_F_p3 = 0.05
MORT_F_p4 = 0.4

NUMBER_SIMS=100

## Latin hypercube scaler functions;
gen_l=function(v){
  min_v=v-(v*.25)
  max_v=v+(v*.25)
  range=max_v-min_v
  function(x){min_v+(x*range)}
}

lhq=randomLHS(NUMBER_SIMS,7)
lhq[,1]=gen_l(MORT_p1)(lhq[,1])
lhq[,2]=gen_l(MORT_p2)(lhq[,2])
lhq[,3]=gen_l(MORT_p3)(lhq[,3])
lhq[,4]=gen_l(MORT_F_p1)(lhq[,4])
lhq[,5]=gen_l(MORT_F_p2)(lhq[,5])
lhq[,6]=gen_l(MORT_F_p3)(lhq[,6])
lhq[,7]=gen_l(MORT_F_p4)(lhq[,7])

output_dir="latin_sens"

dir.create(output_dir)

#########################################

# Create dataframe to define this sim
# This can vary between runs
#
#########################################


sim_def_frame=data.frame(SIM_ID=1:NUMBER_SIMS)
sim_def_frame$FIELD_SIZE=FIELD_SIZE
sim_def_frame$SIM_LENGTH=SIM_LENGTH
sim_def_frame$FIRE_FREQ=FIRE_FREQ
sim_def_frame$FIRE_PROB=FIRE_PROB
sim_def_frame$MORTALITY_ON=MORTALITY_ON
sim_def_frame$F_MORTALITY_ON=F_MORTALITY_ON
sim_def_frame$MORT_b1 = MORT_b1
sim_def_frame$MORT_b2 = MORT_b2
sim_def_frame$MORT_p1 = lhq[,1]
sim_def_frame$MORT_p2 = lhq[,2]
sim_def_frame$MORT_p3 = lhq[,3]
sim_def_frame$MORT_F_b1 = MORT_F_b1
sim_def_frame$MORT_F_b2 = MORT_F_b2
sim_def_frame$MORT_F_b3 = MORT_F_b3
sim_def_frame$MORT_F_p1 = lhq[,4]
sim_def_frame$MORT_F_p2 = lhq[,5]
sim_def_frame$MORT_F_p3 = lhq[,6]
sim_def_frame$MORT_F_p4 = lhq[,7]
sim_def_frame$output_dir = output_dir

write.csv(sim_def_frame,paste0(output_dir,"/sim_list.csv"))

foreach(i=1:NUMBER_SIMS) %dopar% launch_sim(i)


