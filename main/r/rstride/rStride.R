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
# R controller for the Stride model
#
############################################################################ #

# # load simid.rtools package (and install if not yet installed)
if(!'simid.rtools' %in% installed.packages()[,1]){
  require(devtools,quietly = T)
  devtools::install_github("lwillem/simid_rtools",force=F,quiet=T)
  #devtools::uninstall(simid.rtools)
}
library('simid.rtools',quietly = T)

# LOAD R PACKAGES
# 
# XML         to parse and write XML files
# doParallel  to use parallel foreach
# ggplot2     to plot contact matrices
# gridExtra   to plot contact matrices
# mgcv        to sample uncertaint parameters from distributions
# data.table  to use convenience functions for range subsets (e.g. "between")
# openxlsx    to read excel files (reference data on incidence)
# scales      to plot ensembles with transparant colors
# tidyr       to easily replace na's by 0 (replace_na)
# data.table  to process (large) transmission data sets, much better performance!! 
smd_load_packages(c('XML','doParallel','ggplot2','gridExtra','mgcv','data.table','openxlsx','tidyr','data.table'))

# load general help functions
source('./bin/rstride/Misc.R')

# load rStride files (excluding this file and SymptomaticProfileCreator.R)
rStride_files <- dir('./bin/rstride',recursive = T,pattern = '\\.R',full.names = T)
rStride_files <- rStride_files[rStride_files != "./bin/rstride/rStride.R"]
rStride_files <- rStride_files[rStride_files != "./bin/rstride/SymptomaticProfileCreator.R"]
sapply(rStride_files,source)
rStride_functions <- ls()

#' Main function to run rStride for a given design of experiment
#' 
#' @param exp_design                vector with experimental design with parameter names as column names
#' @param dir_postfix               add postfix to output directory name (optional)
#' @param ignore_stdout             hide standard STRIDE terminal output   
#' @param remove_run_output         remove all run-specific output
#' @param parse_log_data            parse log files and aggregate into RData files
#' @param get_csv_output            store aggregated log data also in csv format
#' @param store_transmission_rdata  parse and store transmission data as RData file
#' @param use_date_prefix           add date tag as prefix to output directory and file names
#' @param num_parallel_workers      number of parallel workers (NA = use default settings)
run_rStride <- function(exp_design               = exp_design, 
                        dir_postfix              = '',
                        ignore_stdout            = TRUE, 
                        parse_log_data           = TRUE,
                        get_csv_output           = FALSE,
                        remove_run_output        = TRUE,
                        get_transmission_rdata   = FALSE, 
                        get_burden_rdata         = FALSE,
                        use_date_prefix          = TRUE,
                        num_parallel_workers     = NA)
{
  
  # debug
  if(0==1){
    attach(list(ignore_stdout            = FALSE, 
                parse_log_data           = TRUE,
                get_csv_output           = FALSE,
                remove_run_output        = TRUE,
                get_transmission_rdata   = FALSE,
                get_burden_rdata         = FALSE,
                use_date_prefix          = TRUE,
                num_parallel_workers     = NA))
  }
  
  # command line message
  smd_print('STARTING rSTRIDE CONTROLLER')

  ################################## #
  ## CHECK DESIGN OF EXPERIMENT   ####
  ################################## #
  if(.rstride$data_files_exist(exp_design) == FALSE ||
     .rstride$log_levels_exist(exp_design) == FALSE ||
     .rstride$valid_r0_values(exp_design)  == FALSE ||
     .rstride$valid_immunity_profiles(exp_design)  == FALSE ||
     .rstride$valid_seed_infected(exp_design) == FALSE){
    
    .rstride$cli_abort()
    return(.rstride$no_return_value())
  }
  
  ################################## #
  ## GENERAL OPTIONS              ####
  ################################## #
  stride_bin              <- './bin/stride'
  config_opt              <- '-c'
  config_default_filename <- './config/run_default.xml'
  output_dir              <- 'sim_output'
  
  ################################## #
  ## RUN TAG AND DIRECTORY        ####
  ################################## #

  # create run tag using the current time if use_date_prefix == TRUE
  run_tag <- ifelse(use_date_prefix,format(Sys.time(), format="%Y%m%d_%H%M%S"),'')
  
  # add dir_postfix
  run_tag <- paste0(run_tag,dir_postfix)
  
  # create project directory
  project_dir <- smd_file_path(output_dir,run_tag)

  # command line message
  smd_print('WORKING DIR',getwd())
  smd_print('PROJECT DIR',project_dir)
  
  ################################## #
  ## GENERAL CONFIG MODIFICATIONS ####
  ################################## #
  config_default                  <- xmlToList(config_default_filename)
  config_default$num_threads      <- 1
  config_default$vaccine_profile  <- 'None'
  config_default$vaccine_rate     <- 0
  config_default$immunity_profile <- 'None'
  config_default$immunity_rate    <- 0
  config_default$output_summary   <- 'true'
  config_default$run_tag          <- run_tag
  config_default$num_cea_samples  <- 1e4
  config_default$track_index_case              <- 'false'
  config_default$event_log_level               <- 'Transmissions'
  
  # variable for hospital probability estimation
  config_default$hosp_probability_factor <- 1
  
  ################################## #
  ## PARALLEL SETUP               ####
  ################################## #
  
  # choose timeout based on platform
  cluster_timeout <- ifelse(.rstride$is_ua_cluster(),36000,1000)
  
  # start parallel workers
  smd_start_cluster(timeout = cluster_timeout, num_proc = num_parallel_workers)
  
  ################################## #
  ## RUN                          ####
  ################################## #
  
  # command line message
  smd_print('READY TO RUN',nrow(exp_design),'EXPERIMENT(S)')
  
  # add an "experiment id" to the design of experiment matrix
  exp_design$exp_id <- 1:nrow(exp_design) 
  
  # create temporary directory to store experiment results
  project_dir_exp <- smd_file_path(project_dir,'exp_all')
  
  time_stamp_loop = Sys.time()
  i_exp=1
  # run all experiments (in parallel)
  par_out <- foreach(i_exp=1:nrow(exp_design),
                     .combine='rbind',
                     .packages=c('XML','simid.rtools','data.table'),
                     .export = c('.rstride','par_nodes_info',
                                 'add_hospital_admission_time',
                                 'get_prevalence_data',
                                 rStride_functions),
                     .verbose=FALSE) %dopar%
                     {  
                      
                       # print progress (only slave1)
                       smd_print_progress(i_exp,nrow(exp_design),time_stamp_loop,par_nodes_info)

                       # create experiment tag
                       exp_tag <- .rstride$create_exp_tag(i_exp)

                       # copy default param
                       config_exp <-   config_default
                       
                       # add design parameters
                       for(i_param in 1:ncol(exp_design)){
                         config_exp[names(exp_design)[i_param]] <- exp_design[i_exp,i_param]
                       }  
                       
                       # set process RNG seed
                       set.seed(config_exp$rng_seed + 16022018)
                       
                       # update experiment output prefix
                       config_exp$output_prefix <- smd_file_path(project_dir,exp_tag,.verbose=FALSE)
                       
                       # create xml file
                       config_exp_filename <- .rstride$save_config_xml(config_exp,'run',config_exp$output_prefix)

                       # run stride (using the C++ Controller)
                       system(paste(stride_bin,config_opt,paste0('../',config_exp_filename)),ignore.stdout=ignore_stdout)

                       # load output summary
                       summary_filename <- file.path(config_exp$output_prefix,'summary.csv')
                       run_summary      <- read.table(summary_filename,header=T,sep=',')
                       
                       # merge output summary with input param
                       # note: do not use "merge" to prevent issues with decimal numbers
                       config_df   <- as.data.frame(config_exp)
                       config_df   <- config_df[,!names(config_df) %in% names(run_summary)]
                       run_summary <- cbind(run_summary,config_df)
                       
                       # if we do not want to parse log data, return run summary
                       if(!parse_log_data){
                           return(run_summary)
                       }

                       ## PARSE LOGFILE
                       # create rstride_out list
                       rstride_out <- list()
                       
                       # parse event_log (if present)
                       event_log_filename <- smd_file_path(config_exp$output_prefix,'event_log.txt')
                       if(file.exists(event_log_filename)){
                         rstride_out <- .rstride$parse_event_logfile(event_log_filename,i_exp)
                       
                         # account for non-symptomatic cases
                         flag <- rstride_out$data_transmission$start_symptoms == rstride_out$data_transmission$end_symptoms
                         rstride_out$data_transmission[flag,start_symptoms := NA]
                         rstride_out$data_transmission[flag,end_symptoms := NA]
              
                         # add estimated hospital admission
                         rstride_out$data_transmission <- add_hospital_admission_time(rstride_out$data_transmission,config_exp)
                         
                         # get incidence data
                         rstride_out$data_transmission[,infection_date  := as.Date(config_exp$start_date,'%Y-%m-%d') + sim_day]
                         rstride_out$data_incidence <- get_transmission_statistics(rstride_out$data_transmission)
                         
                         # store disease burden and hospital admission data (for additional analysis)
                         if(get_burden_rdata){
                            rstride_out$data_burden <- data.frame(day_infection           = rstride_out$data_transmission$sim_day,
                                                                  part_age                = rstride_out$data_transmission$part_age,
                                                                  start_infectiousness    = rstride_out$data_transmission$start_infectiousness,
                                                                  end_infectiousness      = rstride_out$data_transmission$end_infectiousness,
                                                                  start_symptoms          = rstride_out$data_transmission$start_symptoms,
                                                                  end_symptoms            = rstride_out$data_transmission$end_symptoms,
                                                                  hospital_admission      = rstride_out$data_transmission$hospital_admission_start,
                                                                  infector_age            = rstride_out$data_transmission$infector_age,
                                                                  infector_is_symptomatic = rstride_out$data_transmission$infector_is_symptomatic,
                                                                  date_infection          = as.Date(config_exp$start_date,'%Y-%m-%d') + rstride_out$data_transmission$sim_day,
                                                                  exp_id                  = rstride_out$data_transmission$exp_id)
                         }
                         
                         # if transmission data should not be stored, replace item by NA
                         if(!get_transmission_rdata){
                           rstride_out$data_transmission <- NA
                         }
                         
                       } else { # end if logfile does not existse
                          smd_print("LOGFILE NOT FOUND!!",WARNING = T)
                       }
                       
                       # convert 'prevalence' files (if present) 
                       rstride_out$data_prevalence_infected    <- get_prevalence_data(config_exp,'infected.csv')
                       rstride_out$data_prevalence_exposed     <- get_prevalence_data(config_exp,'exposed.csv')
                       rstride_out$data_prevalence_infectious  <- get_prevalence_data(config_exp,'infectious.csv')
                       rstride_out$data_prevalence_symptomatic <- get_prevalence_data(config_exp,'symptomatic.csv')
                       rstride_out$data_prevalence_total       <- get_prevalence_data(config_exp,'cases.csv')
                       
                       
                       # save list with all results
                       saveRDS(rstride_out,file=smd_file_path(project_dir_exp,paste0(exp_tag,'_parsed.rds')))
                       
                       # remove experiment output and config
                       if(remove_run_output){
                         unlink(config_exp$output_prefix,recursive=TRUE)
                         unlink(config_exp_filename,recursive = TRUE)
                       }
                       
                       # Finally: return experiment output summary
                       return(run_summary)
                     }
  
  # print final statement
  smd_print('COMPLETE:',nrow(exp_design),'/',nrow(exp_design))
  
  
  # save overal summary
  write.table(par_out,file=file.path(project_dir,paste0(run_tag,'_summary.csv')),sep=',',row.names=F)
  
  ################################## #
  ## AGGREGATE OUTPUT ----
  ################################## #
  # if log data is parsed => aggregate
  if(parse_log_data){
    .rstride$aggregate_compressed_output(project_dir,get_csv_output)
  }
  
    # remove project output
  if(remove_run_output){
    unlink(project_dir_exp,recursive = T)
  }
  
  
  ################################## #
  ## TERMINATE PARALLEL NODES     ####
  ################################## #
  smd_stop_cluster()
  
  # command line message
  smd_print('rSTRIDE CONTROLLER FINISHED')
  
  return(project_dir)
  
} # end run_rStride function

# help function to get the daily incidence
get_counts <- function(all_data,sim_day_max,output_col = "counts"){
  
  if(all(is.na(all_data))){
    return( rep(0,sim_day_max))
  }
  
  # get all bins (-0.5 to set the midpoint to 0,1,2,...)
  breaks <- (0:max(all_data+1,na.rm=T))-0.5
  
  # get statistics
  data_out <- unlist(hist(all_data,breaks,include.lowest = T,right=F,plot=F)[output_col])
  
  # limit to given number of days
  data_out <- data_out[1:sim_day_max]
  
  return(data_out)
}

#data_transmission <- rstride_out$data_transmission
add_hospital_admission_time <- function(data_transmission,config_exp){
  
  # set hospital age groups (age groups for hospital admission)
  hosp_age <- list(age1 = 0:18,   # 0:16
                   age2 = 19:59,  # 16:59
                   age3 = 60:79,  # 60:80
                   age4 = 80:110) # 80+
  
  # create columns for hospital admission start (by age)
  data_transmission[, hospital_admission_start      := as.numeric(NA)]
  data_transmission[, hospital_admission_start_age1 := as.numeric(NA)]
  data_transmission[, hospital_admission_start_age2 := as.numeric(NA)]
  data_transmission[, hospital_admission_start_age3 := as.numeric(NA)]
  data_transmission[, hospital_admission_start_age4 := as.numeric(NA)]
  
  # hospital probability
  # hospital_probability <- data.frame(age1 = 0.035,
  #                                    age2 = 0.0216,
  #                                    age3 = 0.0855,
  #                                    age4 = 0.423)
  hospital_probability <- data.frame(age1 = 0.049,
                                     age2 = 0.03024,
                                     age3 = 0.1197,
                                     age4 = 0.5922)
  
  # adjust probability (for fitting)
  hospital_probability <- hospital_probability * config_exp$hosp_probability_factor
  
  # set hospital delay for 3 age groups
  hosp_delay_mean <- data.frame(age1 = 3,
                                age2 = 7,
                                age3 = 7,
                                age4 = 6)
  
  # set (uniform) delay  distribution -1, 0, 1
  hosp_delay_variance <- -1:1
  
  # calculate time between symptom onset and hospital admission (future work)
  get_hospital_delay <- function(n){
    round(rtweibull(n, shape=1.112,scale=5.970, max =31))
  }

  # sample hospital admission dates
  i_hosp <- 1
  for(i_hosp in 1:length(hospital_probability)){
    flag_part      <- !is.na(data_transmission$start_symptoms) & data_transmission$part_age %in% hosp_age[[i_hosp]]
    flag_admission <- as.logical(rbinom(n = nrow(data_transmission),size = 1,prob = hospital_probability[[i_hosp]]))
    flag_hosp      <- flag_part & flag_admission
    hosp_start    <- as.numeric(data_transmission$start_symptoms[flag_hosp]) + hosp_delay_mean[[i_hosp]] + sample(hosp_delay_variance,sum(flag_hosp),replace = T)
    data_transmission[flag_hosp, hospital_admission_start := hosp_start]
    # data_transmission$hospital_admission_start[flag_hosp]        <- data_transmission$start_symptoms[flag_hosp] +
    #   hosp_delay_mean[[i_hosp]] + sample(hosp_delay_variance,sum(flag_hosp),replace = T)
    # data_transmission$hospital_admission_start[flag_hosp]        <- data_transmission$start_symptoms[flag_hosp] + get_hospital_delay(sum(flag_hosp))
    
    # save age-specific results  
    data_transmission[flag_hosp ,paste0('hospital_admission_start_age',i_hosp) := hosp_start]

  }
 
  # info from Christel on 2048 patients, hospitalised between Feb 29 and March 30
  # 57/2048 born before 2004 (time to hospitalization: median=1, mean=3, range= 0-13)
  # 658/2048 born between 1960-2004 (median=7, mean=7, range= 0-23)
  # 770/2048 born between 1940-1960 (median=6, mean=6, range= 0-29)
  # 416 born before 1940/2048 (median=3, mean=4, range= 0-29)
  # 147 missing age
  ref <- c(57,658,770,416) / (2048-147)
  
  estim <- c(sum(!is.na(data_transmission$hospital_admission_start_age1)),
              sum(!is.na(data_transmission$hospital_admission_start_age2)),
              sum(!is.na(data_transmission$hospital_admission_start_age3)),
              sum(!is.na(data_transmission$hospital_admission_start_age4)))
  
  rbind(estim / sum(estim),
        ref)
  
  smd_print('AVG. HOSPITAL RATE (age specific)',round(mean(approx(c(0,19,60,80),hospital_probability,0:100,method='constant',rule = 2)$y),digits=3))
  smd_print('AVG. HOSPITAL RATE (cases)',round(sum(!is.na(data_transmission$hospital_admission_start)) / sum(!is.na(data_transmission$start_symptoms)),digits=3))

  # return
  return(data_transmission) 
}


# load prevelence data and add colunm names
get_prevalence_data <- function(config_exp,file_name){
  prevalence_filename <- smd_file_path(config_exp$output_prefix,file_name)
  if(file.exists(prevalence_filename)){
    data_prevalence        <- read.table(prevalence_filename,sep=',')
    names(data_prevalence) <- paste0('day',seq(length(data_prevalence))-1)
    data_prevalence$exp_id <- config_exp$exp_id
   return(data_prevalence)
  } else {
    return(NA)
  }

}


