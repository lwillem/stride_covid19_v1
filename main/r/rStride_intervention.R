#!/usr/bin/env Rscript
############################################################################ #
#  This file is part of the Stride software. 
#  It is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by 
#  the Free Software Foundation, either version 3 of the License, or any 
#  later version.
#  The software is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  You should have received a copy of the GNU General Public License,
#  along with the software. If not, see <http://www.gnu.org/licenses/>.
#  see http://www.gnu.org/licenses/.
#
#
#  Copyright 2020, Willem L, Kuylen E & Broeckhove J
############################################################################ #
#
# Call this script from the main project folder (containing bin, config, lib, ...)
# to get all relative data links right. 
#
# E.g.: path/to/stride $ ./bin/rStride_explore.R 
#
############################################################################ #

# Clear work environment
rm(list=ls())

# Load rStride
source('./bin/rstride/rStride.R')

# Load default parameter configurations
source('./bin/rStride_intervention_baseline.R')

# set directory postfix (optional)
dir_postfix <- '_int'

################################## #
## DESIGN OF EXPERIMENTS        ####
################################## #

# add default parameters and values to combine in a full-factorial grid
exp_param_list <- get_exp_param_default()

# change parameters and values to combine in a full-factorial grid
exp_param_list$population_file  <- 'pop_belgium600k_c500_teachers_censushh.csv'
exp_param_list$num_seeds        <- 10


# reduce exp design
exp_param_list$cnt_reduction_other_exit     <- min(exp_param_list$cnt_reduction_other_exit)
exp_param_list$cnt_reduction_workplace_exit <- min(exp_param_list$cnt_reduction_workplace_exit)

exp_param_list$num_days <- 30
  
# # CTS
# exp_param_list$detection_probability <- 0.5
# exp_param_list$start_date <- '2020-05-3'
# exp_param_list$num_days   <- 20
#exp_param_list$event_log_level <-  "All"


################################################ #
## GENERATE DESIGN OF EXPERIMENT GRID         ####
################################################ #

# add sequence with all rng seeds
exp_param_list$rng_seed = seq(exp_param_list$num_seeds)

# generate grid
exp_design <- expand.grid(exp_param_list,
                          stringsAsFactors = F)

# add a unique seed for each run
set.seed(125)
exp_design$rng_seed <- sample(nrow(exp_design))
dim(exp_design)

# check period
range(as.Date(exp_design$start_date), as.Date(exp_design$start_date)+ exp_design$num_days)


################################## #
## RUN rSTRIDE                  ####
################################## #
project_dir <- run_rStride(exp_design               = exp_design,
                           dir_postfix              = dir_postfix,
                           ignore_stdout = FALSE,
                           remove_run_output = FALSE,
                           get_transmission_rdata = TRUE)


############################# #
## INPUT-OUTPUT BEHAVIOR   ####
############################# #
inspect_summary(project_dir)


############################# #
## SURVEY PARTICIPANT DATA ####
############################# #
inspect_participant_data(project_dir)


############################# #
## INCIDENCE DATA          ####
############################# #
inspect_incidence_data(project_dir)


############################# #
## PREVALENCE              ####
############################# #
inspect_prevalence_data(project_dir)


############################# #
## TRANSMISSION            ####
############################# #
inspect_transmission_dynamics(project_dir)
 

############################# #
## CONTACT TRACING         ####
############################# #
inspect_tracing_data(project_dir)



 
