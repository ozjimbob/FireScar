# Experiment 1
# Four fixed regimes, from small/frequent through to large/infrequent
# No shift in regime

## Launch model
library(lhs)
library(doParallel)
library(foreach)

source("cat_face_mortality_pfire.r")

registerDoParallel(cores=16)


######################################################
######################################################

# Experiment 1: fire interval 1 
############################################

FIELD_SIZE=200                    # Number of rows/columns
SIM_LENGTH=700                    # number of years to run
FIRE_FREQ=100                      #  percentage of years
FIRE_PROB=5                      # Size as %age of field
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

output_dir="e1_l1"
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
sim_def_frame$output_dir = output_dir

write.csv(sim_def_frame,paste0(output_dir,"/sim_list.csv"))

foreach(i=1:NUMBER_SIMS) %dopar% launch_sim(i)

######################################################
######################################################

# Experiment 1: fire interval 2 
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

output_dir="e1_l2"
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
sim_def_frame$output_dir = output_dir

write.csv(sim_def_frame,paste0(output_dir,"/sim_list.csv"))

foreach(i=1:NUMBER_SIMS) %dopar% launch_sim(i)

######################################################
######################################################

# Experiment 1: fire interval 3 
############################################

FIELD_SIZE=200                    # Number of rows/columns
SIM_LENGTH=700                    # number of years to run
FIRE_FREQ=10                      #  percentage of years
FIRE_PROB=50                      # Size as %age of field
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

output_dir="e1_l3"
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
sim_def_frame$output_dir = output_dir

write.csv(sim_def_frame,paste0(output_dir,"/sim_list.csv"))

foreach(i=1:NUMBER_SIMS) %dopar% launch_sim(i)

######################################################
######################################################

# Experiment 1: fire interval 4 
############################################

FIELD_SIZE=200                    # Number of rows/columns
SIM_LENGTH=700                    # number of years to run
FIRE_FREQ=5                      #  percentage of years
FIRE_PROB=100                      # Size as %age of field
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

output_dir="e1_l4"
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
sim_def_frame$output_dir = output_dir

write.csv(sim_def_frame,paste0(output_dir,"/sim_list.csv"))

foreach(i=1:NUMBER_SIMS) %dopar% launch_sim(i)

####################################################
####################################################
##
##
##
##

