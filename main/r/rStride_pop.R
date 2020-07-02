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
#  Copyright 2020, Willem L, Kuylen E & Broeckhove J
#############################################################################
#
# Call this script from the main project folder (containing bin, config, lib, ...)
# to get all relative data links right. 
#
# E.g.: path/to/stride $ ./bin/rStride_pop.R 
#
#############################################################################

# Clear work environment
rm(list=ls())

# Load rStride
source('./bin/rstride/rStride.R')

##################################
## DESIGN OF EXPERIMENTS        ##
##################################

# set filename
# pop_file_name <- 'pop_belgium600k_c500_teachers_censushh.csv'
pop_file_name <- 'pop_belgium3000k_c500_teachers_censushh.csv'

# set maximum age difference between household seniors.
max_age_diff <- 3

# max number of households in one cluster?
household_cluster_size <- 1

# run function to extend population data
extend_population_data(pop_file_name,max_age_diff,household_cluster_size)


# end.



