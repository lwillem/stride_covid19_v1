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
# Baseline settings for rStride intervention scenarios
#
############################################################################ #

################################## #
## DESIGN OF EXPERIMENTS        ####
################################## #

# get default parameter values to combine in a full-factorial grid
get_exp_param_default <- function(bool_child_param = FALSE){
   out <- list(r0                            = seq(3.4,3.4,0.1),
                num_days                      = 196,
                num_seeds                     = 10,
                num_participants_survey       = 300,
                num_infected_seeds            = 510,
                disease_config_file           = "disease_covid19_age.xml",
                population_file               = c("pop_belgium11M_c500_teachers_censushh.csv"),
                age_contact_matrix_file       = "contact_matrix_flanders_conditional_teachers.xml",
                start_date                    = c('2020-02-17'),
                holidays_file                 = 'calendar_belgium_2020_covid19_exit_school_adjusted.json',
                school_system_adjusted        = 1,
                telework_probability          = 0,
                cnt_reduction_workplace       = 0.8,
                cnt_reduction_other           = 0.85,
                compliance_delay_workplace    = 6,
                compliance_delay_other        = 6,
                num_daily_imported_cases      = 0,
                cnt_reduction_workplace_exit  = seq(0.6,0.8,0.1),
                cnt_reduction_other_exit      = c(0.7,0.80,0.85),
                cnt_reduction_school_exit     = 0.5,
                cnt_reduction_intergeneration = 0.9,
                cnt_reduction_intergeneration_cutoff = 65,
                cnt_intensity_householdCluster = 0,
                detection_probability          = 0,
                tracing_efficiency_household   = 0.9, 
                tracing_efficiency_other       = 0.5,
                case_finding_capacity          = 10000, # no limit at this stage
                delay_isolation_index          = 1,
                delay_contact_tracing          = 3,
                test_false_negative            = 0.1,
                cnt_other_exit_delay           = 21,
               
               # log level
               event_log_level                 = "Transmissions",
                
                # factor for parameter estimation and fitting
                hosp_probability_factor        = 1
          )
   
   # change parameters if childrens infectiousness is 1/2 compared to adults
   if(bool_child_param){ #best fit on 2020-06-07
     out$disease_config_file <- "disease_covid19_child.xml"
     out$cnt_reduction_workplace <- 0.85
     out$cnt_reduction_other     <- 0.87
     out$hosp_probability_factor <- 0.80
   }
   
   # return parameters
   return(out)
}

