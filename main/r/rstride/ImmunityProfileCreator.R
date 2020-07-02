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
#  Copyright 2019, Willem L, Kuylen E & Broeckhove J
#############################################################################
#
# BASED ON DATA FROM:
#
# Hens N, Abrams S, Santermans E, Theeten H, Goeyvaerts N, Lernout T, Leuridan E, 
# Van Kerckhove K, Goossens H, Van Damme P, Beutels P. Assessing the risk of 
# measles resurgence in a highly vaccinated population: Belgium anno 2013. Euro 
# Surveill. 2015;20(1):pii=20998.  
#
############################################

## TO RUN THIS SCRIPT
if(0==1){
  # clear workspace
  rm(list=ls())
  
  # load library
  library(XML)
  
  # run
  get_immunity_profile()
}

############################################
## LOAD AND RESHAPE DATA                  ##
############################################

get_immunity_profile <- function()
{
  
  # load data
  all_data <- read.table('./data/susceptibility_measles_belgium_2013.csv',sep=',',header=F,skip=1)  
  num_age_data <- nrow(all_data)
  dim(all_data)
  
  # calculate the average susceptibility per age
  susceptiblilty_profile <- rowSums(all_data) / ncol(all_data)
  
  # newborns have 3 months maternally immunity => 1/4 protected, 3/4 not protected 
  susceptiblilty_profile[1] <- 3/4
  
  # extend to 110 years of age by repeating the last one (from 0 to 110 year = 111 categories)
  max_age <- 110
  num_age <- max_age+1
  susceptiblilty_profile <- c(susceptiblilty_profile,rep(susceptiblilty_profile[num_age_data],num_age-num_age_data))
  
  # get immunity =  1 - suscetibility
  immunity_profile <- 1-susceptiblilty_profile
  
  # explore
  plot(susceptiblilty_profile,ylim=0:1,type='l',lwd=7,ylab='susceptibility',xlab='age')
  plot(immunity_profile,ylim=0:1,type='l',lwd=7,ylab='immunity',xlab='age')
  
  ############################################
  ## SAVE AS XML  	 	                      ##
  ############################################
  
  # add age group as column names
  names(immunity_profile) <- paste0('age',0:max_age)
  
  # add info on data source and manipulation
  immunity_data <- unlist(list(data_source = 'susceptibility_measles_belgium_2013.csv',
                               data_manipulation = "average by age",
                               round(immunity_profile,digits=4)))
  
  # save as xml
  .rstride$save_config_xml(immunity_data,'immunity','immunity_measles_belgium')
  
  
  ############################################
  ## FUTURE IMMUNITY PROFILES               ##
  ############################################
  
  age2index <- function(x) return(x+1)
  
  imm_ages <- 0:max_age
  plot(imm_ages,susceptiblilty_profile,ylim=0:1,type='p',lwd=7,ylab='susceptibility',xlab='age')
  abline(v=12)
  abline(v=20)
  abline(v=29)
  
  time_horizon <- 10
  pred_year <- 2013+(0:time_horizon)
  
  i_year <- 0
  for(i_year in 0:time_horizon){
    
    # assemple susceptibility profile
    pred_susceptiblilty_profile <- c( susceptiblilty_profile[age2index(0)],               # age 0
                                      rep(susceptiblilty_profile[age2index(1)],i_year),   # age 1 (extended)
                                      susceptiblilty_profile[-age2index(0)])              # other ages
    
    # select and store
    pred_susceptiblilty_profile <- pred_susceptiblilty_profile[1:num_age]
    
    # get immunity =  1 - suscetibility
    immunity_profile <- 1-pred_susceptiblilty_profile
    
    # add ages
    names(immunity_profile) <- paste0('age',0:max_age)
    
    # add info on data source and manipulation
    immunity_data <- unlist(list(data_source = 'susceptibility_measles_belgium_2013.csv',
                                 data_manipulation = "average by age",
                                 prediction_year  = pred_year[i_year+1],
                                 round(immunity_profile,digits=4)))
    
    
    .rstride$save_config_xml(immunity_data,'immunity',paste0('immunity_measles_belgium',pred_year[i_year+1]))
  } # end for-loop
  
} # end function
