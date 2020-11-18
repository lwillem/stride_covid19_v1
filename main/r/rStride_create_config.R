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
# E.g.: path/to/stride $ ./bin/rStride_explore.R 
#
#############################################################################

# Clear work environment
rm(list=ls())

# Load rStride
source('./bin/rstride/rStride.R')

args = commandArgs(trailingOnly=TRUE)
dir = args[1]
csv_fn = args[2]
exp_id = args[3]

run_tag <- exp_id
default_config <- create_default_config('./config/run_default.xml', run_tag)

exp_design <- read.csv(smd_file_path(dir, csv_fn), stringsAsFactors=FALSE)
exp_dir <- paste0(dir,"/",exp_id,"/") 
xml_fn <- smd_file_path(exp_dir,"config.xml")
exp_row <- match(TRUE, exp_design$id == exp_id)
config_exp <- create_config_exp(default_config, exp_dir, exp_design, exp_row)
save_config_xml(config_exp,xml_fn)
