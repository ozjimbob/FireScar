# Experiment 2
# Shift in fire regime from small-frequent to large-infrequent
# Shift occurs at different times in the past, and between widely divergent regimes and closer regimes

## Launch model
library(doParallel)
library(foreach)

source("cat_face_mortality_pfire_split.r")


registerDoParallel(cores=16)

######################################################
######################################################

# Experiment 2: Split regime, mortality on
############################################

FIELD_SIZE=200                    # Number of rows/columns
SIM_LENGTH=700                    # number of years to run
FIRE_FREQ_1=100                   #  percentage of years regime 1
FIRE_PROB_1=5                     # Area regime 1
FIRE_FREQ_2=5                     # Percentage of years regime 2
FIRE_PROB_2=100                   # Area regime 2
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

NUMBER_SIMS=400

SPLIT_YEAR=rep(c(150,200,250,300,350,400,450,500,550,600),each=40) # Sequence of split years

output_dir="e2_range_wide"
dir.create(output_dir)
#########################################

# Create dataframe to define this sim
# This can vary between runs
#
#########################################


sim_def_frame=data.frame(SIM_ID=1:NUMBER_SIMS)
sim_def_frame$FIELD_SIZE=FIELD_SIZE
sim_def_frame$SIM_LENGTH=SIM_LENGTH
sim_def_frame$FIRE_FREQ_1=FIRE_FREQ_1
sim_def_frame$FIRE_FREQ_2=FIRE_FREQ_2
sim_def_frame$FIRE_PROB_1=FIRE_PROB_1
sim_def_frame$FIRE_PROB_2=FIRE_PROB_2
sim_def_frame$MORTALITY_ON=MORTALITY_ON
sim_def_frame$F_MORTALITY_ON=F_MORTALITY_ON
sim_def_frame$MORT_b1 = MORT_b1
sim_def_frame$MORT_b2 = MORT_b2
sim_def_frame$MORT_p1 = MORT_p1
sim_def_frame$MORT_p2 = MORT_p2
sim_def_frame$MORT_p3 = MORT_p3
sim_def_frame$MORT_F_b1 = MORT_F_b1
sim_def_frame$MORT_F_b2 = MORT_F_b2
sim_def_frame$MORT_F_b3 = MORT_F_b3
sim_def_frame$MORT_F_p1 = MORT_F_p1
sim_def_frame$MORT_F_p2 = MORT_F_p2
sim_def_frame$MORT_F_p3 = MORT_F_p3
sim_def_frame$MORT_F_p4 = MORT_F_p4
sim_def_frame$SPLIT_YEAR=SPLIT_YEAR
sim_def_frame$output_dir = output_dir

write.csv(sim_def_frame,paste0(output_dir,"/sim_list.csv"))

foreach(i=1:NUMBER_SIMS) %dopar% launch_sim_split(i)


######################################################
######################################################

# Experiment 2: Split regime, mortality on narror
############################################

FIELD_SIZE=200                    # Number of rows/columns
SIM_LENGTH=700                    # number of years to run
FIRE_FREQ_1=20                      #  percentage of years
FIRE_PROB_1=25
FIRE_FREQ_2=10
FIRE_PROB_2=50
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

NUMBER_SIMS=400

SPLIT_YEAR=rep(c(150,200,250,300,350,400,450,500,550,600),each=40)

output_dir="e2_range_narrow"
dir.create(output_dir)
#########################################

# Create dataframe to define this sim
# This can vary between runs
#
#########################################


sim_def_frame=data.frame(SIM_ID=1:NUMBER_SIMS)
sim_def_frame$FIELD_SIZE=FIELD_SIZE
sim_def_frame$SIM_LENGTH=SIM_LENGTH
sim_def_frame$FIRE_FREQ_1=FIRE_FREQ_1
sim_def_frame$FIRE_FREQ_2=FIRE_FREQ_2
sim_def_frame$FIRE_PROB_1=FIRE_PROB_1
sim_def_frame$FIRE_PROB_2=FIRE_PROB_2
sim_def_frame$MORTALITY_ON=MORTALITY_ON
sim_def_frame$F_MORTALITY_ON=F_MORTALITY_ON
sim_def_frame$MORT_b1 = MORT_b1
sim_def_frame$MORT_b2 = MORT_b2
sim_def_frame$MORT_p1 = MORT_p1
sim_def_frame$MORT_p2 = MORT_p2
sim_def_frame$MORT_p3 = MORT_p3
sim_def_frame$MORT_F_b1 = MORT_F_b1
sim_def_frame$MORT_F_b2 = MORT_F_b2
sim_def_frame$MORT_F_b3 = MORT_F_b3
sim_def_frame$MORT_F_p1 = MORT_F_p1
sim_def_frame$MORT_F_p2 = MORT_F_p2
sim_def_frame$MORT_F_p3 = MORT_F_p3
sim_def_frame$MORT_F_p4 = MORT_F_p4
sim_def_frame$SPLIT_YEAR=SPLIT_YEAR
sim_def_frame$output_dir = output_dir

write.csv(sim_def_frame,paste0(output_dir,"/sim_list.csv"))

foreach(i=1:NUMBER_SIMS) %dopar% launch_sim_split(i)


