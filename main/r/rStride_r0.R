#!/usr/bin/env Rscript
#############################################################################
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
#  Copyright 2020, Willem L
#############################################################################
#
# Call this script from the main project folder (containing bin, config, lib, ...)
# to get all relative data links right. 
#
# E.g.: path/to/stride $ ./bin/rStride_r0.R 
#
#############################################################################

# Clear work environment
rm(list=ls())

# Load rStride
source('./bin/rstride/rStride.R')

##################################
## DESIGN OF EXPERIMENTS        ##
##################################

# uncomment the following line to inspect the config xml tags
#names(xmlToList('./config/run_default.xml'))

# set directory postfix (optional)
dir_postfix <- '_r0'

# set the number of realisations per configuration set
num_seeds  <- 10

# add parameters and values to combine in a full-factorial grid
exp_design <- expand.grid(r0                            = sort(c(seq(1,6,length=15))),
                          num_days                      = c(20),
                          rng_seed                      = seq(num_seeds),
                          start_date                    = c('2020-02-01','2020-02-02','2020-02-03','2020-02-04','2020-02-05','2020-02-06','2020-02-07'),
                          num_infected_seeds            = 20,
                          seeding_age_min               = 1,
                          seeding_age_max               = 99,
                          disease_config_file           = "disease_covid19_age.xml",
                          population_file               = "pop_belgium600k_c500_teachers_censushh.csv",
                          age_contact_matrix_file       = "contact_matrix_flanders_conditional_teachers.xml",
                          holidays_file                 = "holidays_none.csv",
                          stringsAsFactors = F)

# add a unique seed for each run
set.seed(num_seeds)
exp_design$rng_seed <- sample(nrow(exp_design))
dim(exp_design)

##################################
## RUN rSTRIDE                  ##
##################################
project_dir <- run_rStride(exp_design = exp_design,
                           dir_postfix = dir_postfix,
                           ignore_stdout = T,
                           remove_run_output = T,
                           get_transmission_rdata = T)


############################# #
## INPUT-OUTPUT BEHAVIOR   ####
############################# #
# inspect_summary(project_dir)
inspect_participant_data(project_dir)
# inspect_incidence_data(project_dir)
# inspect_prevalence_data(project_dir)
inspect_transmission_dynamics(project_dir)


##################################
## REPRODUCTION NUMBER          ##
##################################
analyse_transmission_data_for_r0(project_dir)


################################### #
## HOSPITAL ADMISSIONS BY AGE    ####
#####################################

# covid-19 specific!!
analyse_transmission_data_for_hospital_admissions(project_dir)
  

## TO SELECT A MEAN INFECTIOUS PERIOD
if(0==1){
  
  output_names <- dir("/Users/lwillem/Documents/university/research/stride/results_covid19_revision/20200916_r0_timeInfectious",full.names = T)
  output_names <- output_names[!grepl('_child',output_names)]
  d <- 6
  foreach(d = 4:6,
          .combine = 'rbind') %do% {
            
            filename <- output_names[grepl(paste0('r0_d',d),output_names)]
            
            return(cbind(d,aggregate_transmission_dynamics(filename)))
            
          } -> r0_all
  

  # create data.frame  
  r0_all <- data.frame(r0_all)
  
  # get directory name and open pdf stream
  dir_name <- unique(dirname(output_names))
  pdf(file=smd_file_path(dir_name,'stride_doubling_generation.pdf'),6,4)
  
  plot(y = r0_all$doubling_time,
       x = r0_all$gen_interval,
        col = r0_all$d-2,
       pch=20,
       xlim=c(3,8),
       ylim = c(1,10),
       xlab='Generation interval (days)',
       ylab='Doubling time (days)')
  

  abline(h=3.1,col=1,lty=2)
  polygon(x=c(1:10,rev(1:10)),
          y = c(rep(2.4,10),rep(4.4,10)),
          col=alpha(1,0.1),border = NA)
  
  abline(v=5.2,col=1,lty=2)
  polygon(y=c(1:10,rev(1:10)),
          x = c(rep(3.78,10),rep(6.78,10)),
          col=alpha(1,0.1),border = NA)
  
  # p_sel <- r0_all$sec_cases < 4 &  r0_all$sec_cases > 2.5
  # points(y = r0_all$doubling_time[p_sel],
  #      x = r0_all$gen_interval[p_sel],
  #      col = r0_all$V1[p_sel],
  #      pch=19)
  # 
  legend('topright',
         paste(4:6,'days'),
         title="Mean infectious period",
         col=4:6-2,
         cex=0.8,
         pch=20,
         bg='white') 
  
  # close pdf stream
  dev.off()
  
  }



