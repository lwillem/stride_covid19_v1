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
#  Copyright 2019, Willem L, Kuylen E & Broeckhove J
############################################################################ #
# 
# Helpfunction(s) to parse the log file(s)
#
############################################################################ #

################################# #
## PARSE LOGFILE               ####
################################# #

"DEVELOPMENT CODE"
if(0==1){
  
  #f_exp_dir <- file.path(output_dir,output_exp_dirs[i_exp])
  f_exp_dir <- file.path(project_dir,'exp0001')
  event_logfile <- file.path(f_exp_dir,'event_log.txt')
  
  
}

.rstride$parse_event_logfile <- function(event_logfile,exp_id)
{
  
  # terminal message
  cat("PARSING LOGFILE:",event_logfile,fill=TRUE)
  
  # load log file and fill empty columns
  data_log  <- fread(event_logfile, sep=' ',fill=T)

  # experiment output directory
  exp_dir <- dirname(event_logfile)
  
  # initialise output variables
  rstride_out <- list()
  
  # Parse log file using the following tags tags: 
  # - PART    participant info
  # - PRIM    seed infection
  # - TRAN    transmission event
  # - CONT    contact event
  # - VACC    additional immunization
  # - TRACE   contact tracing
  # 
  # note:
  # - drop the first column with the log tag
  

  ###################### #
  ## PARTICIPANT DATA ####
  ###################### #
  if(data_log[V1 == "[PART]",.N] > 0)
    
  {
    header_part         <- c('local_id', 'part_age', 'household_id', 'school_id', 'college_id','workplace_id', 'household_cluster_id',
                             'is_susceptible','is_infected','is_infectious','is_recovered','is_immune',
                             'start_infectiousness','start_symptomatic','end_infectiousness','end_symptomatic',
                             'household_size','school_size','college_size','workplace_size','primarycommunity_size','secundarycommunity_size','is_teleworking')
    data_part <- data_log[V1=="[PART]",-1]
    names(data_part)    <- header_part
    
    # # set 'true' and 'false' in the R-format
    # # data_part[data_part=="true"] <- 1
    # # data_part[data_part=="false"] <- 0
    # data_part <- data_part[,replace(.SD, .SD == 'true', 1)]
    # data_part <- data_part[,replace(.SD, .SD == 'false', 0)]
    # summary(data_part)
    
    # make sure, all values (except the booleans) are stored as integers
    #col_non_numeric <- which(grepl('is_',header_part))
    # data_part[,-col_non_numeric] <- data.frame(apply(data_part[,-col_non_numeric], 2, as.double))
    #data_part[header_part == 'true',]
    
    col_boolean <- header_part[grepl('is_',header_part)]
    data_part[, (col_boolean) := replace(.SD, .SD == 'true', 1), .SDcols = col_boolean]
    data_part[, (col_boolean) := replace(.SD, .SD == 'false', 0), .SDcols = col_boolean]
    data_part[,..col_boolean,]
    
    # data_part[,is_infected := is_infected == 1]
    
    # add exp_id
    data_part$exp_id <- exp_id
    
    # save
    # save(data_part,file=file.path(exp_dir,'data_participants.RData'))
    rstride_out$data_participants = data_part
  } else {
    rstride_out$data_participants = NA
  }
  
  
  ####################### #
  ## TRANSMISSION DATA ####
  ####################### #
  #if(any(c("[PRIM]","[TRAN]") %in% data_log[,1]))
  if(data_log[V1 %in% c("[PRIM]","[TRAN]"),.N]>0)
  {
    header_transm       <- c('local_id', 'infector_id','part_age',
                             'infector_age','cnt_location','sim_day','id_index_case',
                             'start_infectiousness','end_infectiousness','start_symptoms','end_symptoms',
                             'infector_is_symptomatic')
    sel_cols    <- paste0('V',seq_len(length(header_transm))+1)
    data_transm <- data_log[V1 %in% c("[PRIM]","[TRAN]"),..sel_cols]
    names(data_transm)  <- header_transm
    data_transm
    
    # # make sure, all values are stored as integers
    # if(any(apply(data_transm, 2, typeof) != 'integer')){
    #   location_col <- names(data_transm) %in% c('cnt_location','infector_is_symptomatic')
    #   if(nrow(data_transm)>1){
    #     data_transm[,!location_col] <- data.frame(apply(data_transm[,!location_col], 2, as.double))
    #   } else {
    #     data_transm[,!location_col] <- c(apply(data_transm[,!location_col], 2, as.double))
    #   }
    # }
    
    # set 'true' and 'false' in the R-format
    data_transm[,infector_is_symptomatic:=infector_is_symptomatic == 'true']
    
    
    # set local_id and cnt_location from the seed infected cases to NA (instead as -1)
    # data_transm[data_transm == -1]   <- NA
    # data_transm$cnt_location[data_transm$cnt_location == '<NA>'] <- NA
    data_transm <- data_transm[,replace(.SD, .SD == -1, NA)]
    #data_transm <- data_transm[,replace(.SD, .SD == '<NA>', NA)]
    
    # set 'true' and 'false' in the R-format
    #data_transm$infector_is_symptomatic <- as.logical(data_transm$infector_is_symptomatic)
    # data_transm[,infector_is_symptomatic:=1,]
    # data_transm$infector_is_symptomatic_b
    # data_transm <- data_transm[,replace(.SD, .SD == 'true', 1)]
    # data_transm <- data_transm[,replace(.SD, .SD == 'false', 0)]
    
    # add exp_id
    data_transm$exp_id <- exp_id
    
    # save
    # save(data_transm,file=file.path(exp_dir,'data_transmission.RData'))
    rstride_out$data_transmission = data_transm
  } else {
    rstride_out$data_transmission = NA
  }
  
  ###################### #
  ## CONTACT DATA     ####
  ###################### # 
  # if(any(data_log[,1] == "[CONT]"))
  if(data_log[V1 %in% "[CONT]",.N]>0)
  {
    header_cnt          <- c('local_id', 'part_age', 'cnt_age', 'cnt_home', 'cnt_school', 
                             'cnt_college','cnt_work', 'cnt_prim_comm', 'cnt_sec_comm', 'cnt_hh_cluster', 
                             'sim_day', 'cnt_prob', 'trm_prob','part_sympt','cnt_sympt')
    sel_cols    <- paste0('V',seq_len(length(header_cnt))+1)
    data_cnt <- data_log[V1 %in% "[CONT]",..sel_cols]
    names(data_cnt)  <- header_cnt
    
    # convert text into boolean
    # data_cnt$part_sympt <- as.numeric(data_cnt$part_sympt == 'true')
    # data_cnt$cnt_sympt  <- as.numeric(data_cnt$cnt_sympt == 'true')
    col_boolean <- header_cnt[grepl('_sympt',header_cnt)]
    data_cnt[, (col_boolean) := replace(.SD, .SD == 'true', 1), .SDcols = col_boolean]
    data_cnt[, (col_boolean) := replace(.SD, .SD == 'false', 0), .SDcols = col_boolean]
    
    # make sure, all values are stored as integers
    # data_cnt <- data.frame(apply(data_cnt,  2, as.double))
    # dim(data_cnt)
    
    # add exp_id
    data_cnt$exp_id <- exp_id
    
    # save
    # save(data_cnt,file=file.path(exp_dir,'data_contacts.RData'))
    rstride_out$data_contacts = data_cnt
  } else {
    rstride_out$data_contacts = NA
  }
  
  ###################### #
  ## VACCINATION DATA ####
  ###################### # 
  # if(any(data_log[,1] == "[VACC]"))
  if(data_log[V1 %in% "[VACC]",.N]>0)  
  {
    header_vac          <- c('local_id', 'part_age', 'pool_type', 'pool_id', 'pool_has_infant', 'sim_day')
    sel_cols    <- paste0('V',seq_len(length(header_vac))+1)
    data_cnt    <- data_log[V1 %in% "[VACC]",..sel_cols]
    names(data_vacc)    <- header_vac
    data_vacc[1,]
    
    # # make sure, all values are stored as integers
    # pool_type_col <- names(data_vacc) %in% c('pool_type','pool_has_infant')
    # data_vacc[,!pool_type_col] <- data.frame(apply(data_vacc[,!pool_type_col], 2, as.integer))
    # dim(data_vacc)
    
    # add exp_id
    data_vacc$exp_id <- exp_id
    
    # save
    # save(data_vacc,file=file.path(exp_dir,'data_vaccination.RData'))
    rstride_out$data_vaccination = data_vacc
  } else {
    rstride_out$data_vaccination = NA
  }
  
  ########################################## #
  ## CONTACT TRACING DATA ####
  ########################################## # 
  # if(any(data_log[,1] == "[TRACE]"))
  if(data_log[V1 %in% c("[TRACE]"),.N]>0)
  {
    header_trace           <- c('local_id', 'part_age', 'is_infected', 'is_symptomatic','pool_type', 
                                'case_id','case_age','sim_day','num_unique_contacts','num_contacts_tested')
    sel_cols             <- paste0('V',seq_len(length(header_trace))+1)
    data_tracing         <- data_log[V1 %in% "[TRACE]",..sel_cols]
    names(data_tracing)  <- header_trace
    data_tracing[1,]
    
    # # convert text into boolean
    # data_tracing$is_infected <- as.numeric(data_tracing$is_infected == 'true')
    # data_tracing$is_symptomatic <- as.numeric(data_tracing$is_symptomatic == 'true')
    col_boolean <- header_trace[grepl('is_',header_trace)]
    data_tracing[, (col_boolean) := replace(.SD, .SD == 'true', 1), .SDcols = col_boolean]
    data_tracing[, (col_boolean) := replace(.SD, .SD == 'false', 0), .SDcols = col_boolean]
     
    # # make sure, all values are stored as integers
    # pool_type_col <- names(data_tracing) %in% c('pool_type')
    # data_tracing[,!pool_type_col] <- data.frame(apply(data_tracing[,!pool_type_col], 2, as.integer))
    # dim(data_tracing)
    
    # add exp_id
    data_tracing$exp_id <- exp_id
    
    # save
    rstride_out$data_tracing = data_tracing
  } else {
    rstride_out$data_tracing = NA
  }
  
  # terminal message
  cat("LOG PARSING COMPLETE",fill=TRUE)
  
  # return
  return(rstride_out)
}
