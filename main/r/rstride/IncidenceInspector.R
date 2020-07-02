############################################################################# #
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
############################################################################# #
#
# MODEL INCIDENCE EXPLORATION
#
############################################################################# #


#' @param project_dir   name of the project folder
#' @param num_selection the number of experiments with minimal LS score to select and present
inspect_incidence_data <- function(project_dir, num_selection = 4, bool_add_param=TRUE)
{
  # command line message
  smd_print('INSPECT INCIDENCE DATA...')
  
  #debug
  if(!exists('num_selection')) {num_selection = 4}
  if(!exists('bool_add_param')) {bool_add_param = TRUE}
  
  # load project summary
  project_summary    <- .rstride$load_project_summary(project_dir)
  
  # reduce population filenames
  project_summary$population_file <- gsub('_belgium','',project_summary$population_file)
  project_summary$population_file <- gsub('_c500_teachers_censushh','',project_summary$population_file)
  project_summary$population_file <- gsub('.csv','',project_summary$population_file)

  # retrieve all variable model parameters
  input_opt_design   <- .rstride$get_variable_model_param(project_summary)
  
  # get all transmission output
  data_incidence_all <- .rstride$load_aggregated_output(project_dir,'data_incidence')
  
  if(length(data_incidence_all) == 1 && is.na(data_incidence_all)){
    smd_print('NO INCIDENCE DATA AVAILABLE.')
    return(.rstride$no_return_value())
  }
  
  
  
  # add config_id 
  # get variable names of input_opt_design (fix if only one column)
  if(ncol(input_opt_design) == 1) {
    project_summary$config_id  <- project_summary[,colnames(input_opt_design)]
    input_opt_design           <- data.frame(input_opt_design,config_id = c(input_opt_design))
  } else{
    project_summary$config_id  <- apply(project_summary[,names(input_opt_design)],1,paste, collapse='_')
    input_opt_design$config_id <- apply(input_opt_design,1,paste, collapse='_')
  }
  
  # to generate contact tracing id
  flag_opt_input_tracing <- !(grepl('cnt_reduction',names(input_opt_design)) | grepl('config_id',names(input_opt_design)))
  colnames_tracing           <- names(input_opt_design)[flag_opt_input_tracing]
  if(length(colnames_tracing) == 1) {
    project_summary$tracing_id  <- project_summary[,colnames_tracing]
    input_opt_design            <- data.frame(input_opt_design, 
                                              tracing_id = unlist(input_opt_design[colnames_tracing]))
  } else{
    project_summary$tracing_id  <- apply(project_summary[,colnames_tracing],1,paste, collapse='_')
    input_opt_design$tracing_id <- apply(input_opt_design[,colnames_tracing],1,paste, collapse='_')
  }
  
  # add contact id
  flag_opt_input_tracing <- (grepl('cnt_reduction',names(input_opt_design)) | grepl('exit',names(input_opt_design)))
  if(any(flag_opt_input_tracing)){
    colnames_contact           <- names(input_opt_design)[flag_opt_input_tracing]
    if(length(colnames_contact) == 1) {
      project_summary$contact_id  <- project_summary[,colnames_contact]
      input_opt_design            <- data.frame(input_opt_design, 
                                                contact_id = unlist(input_opt_design[colnames_contact]))
    } else{
      project_summary$contact_id  <- apply((1-project_summary[,colnames_contact])*100,1,paste, collapse=',')
      input_opt_design$contact_id <- apply((1-input_opt_design[,colnames_contact])*100,1,paste, collapse=',')
    }
  } else{
    project_summary$contact_id  <- project_summary$config_id
    input_opt_design$contact_id <- input_opt_design$config_id
  }
  
  
  # add config_id and tracing_id to incidence data
  data_incidence_all         <- merge(data_incidence_all,project_summary[,c('exp_id','config_id','tracing_id','contact_id')] )

  ## REFERENCE DATA COVID-19: new hospitalisation
  file_name <- './data/covid19.csv'
  burden_of_disaese  <- read.table(file_name,sep=',',header=T,stringsAsFactors = F)
  hosp_cases_num     <- burden_of_disaese$NewPatientsNotReferredHospital
  hosp_cases_cum     <- cumsum(hosp_cases_num)
  hosp_cases_date    <- as.Date(burden_of_disaese$DateCase)
  
  # aggregate into one data.frame
  hosp_adm_data <- data.frame(date    = hosp_cases_date,
                              num_adm = hosp_cases_num,
                              cum_adm = hosp_cases_cum)
  
  # remove reference data if simulation period is shorter
  flag_compare  <- hosp_adm_data$date %in% data_incidence_all$sim_date
  hosp_adm_data <- hosp_adm_data[flag_compare,]
  
  # create columns for hospital cases and least square score
  data_incidence_all$cumulative_hospital_cases <- NA
  data_incidence_all$ls_score_hosp              <- NA
  
  # create matrix to reformat hospital data
  hosp_data_ls     <- matrix(NA, ncol = 1, nrow = nrow(project_summary))
  hosp_data_tag    <- hosp_data_ls
  
  ## PREVALENCE DATA STOCHASTIC MODEL
  prevalence_ref <- readRDS('./data/prevalence_stochastic_model_20200604.rds')
  head(prevalence_ref)
  pop_size_be <- 11e6  #TODO: use universal variable
  
  i_exp <- 1  
  # loop over each experiment
  for(i_exp in unique(data_incidence_all$exp_id)){
    
    # select one run
    flag_exp       <- data_incidence_all$exp_id == i_exp
    
    # calculate cumulative cases
    data_incidence_all$cumulative_infections[flag_exp]        <- .rstride$cumsum_na(data_incidence_all$new_infections[flag_exp])
    data_incidence_all$cumulative_infectious_cases[flag_exp]  <- .rstride$cumsum_na(data_incidence_all$new_infectious_cases[flag_exp])
    data_incidence_all$cumulative_symptomatic_cases[flag_exp] <- .rstride$cumsum_na(data_incidence_all$new_symptomatic_cases[flag_exp])
    data_incidence_all$cumulative_hospital_cases[flag_exp]    <- .rstride$cumsum_na(data_incidence_all$new_hospital_admissions[flag_exp])
    
    # age specific hospital admissions
    data_incidence_all$cumulative_hospital_cases_age1[flag_exp]    <- .rstride$cumsum_na(data_incidence_all$new_hospital_admissions_age1[flag_exp])
    data_incidence_all$cumulative_hospital_cases_age2[flag_exp]    <- .rstride$cumsum_na(data_incidence_all$new_hospital_admissions_age2[flag_exp])
    data_incidence_all$cumulative_hospital_cases_age3[flag_exp]    <- .rstride$cumsum_na(data_incidence_all$new_hospital_admissions_age3[flag_exp])
    data_incidence_all$cumulative_hospital_cases_age4[flag_exp]    <- .rstride$cumsum_na(data_incidence_all$new_hospital_admissions_age4[flag_exp])
    
    # Sum of Squares: score
    flag_hosp_data       <- flag_exp & data_incidence_all$sim_date %in% hosp_adm_data$date
    flag_hosp_ref        <- hosp_adm_data$date %in% data_incidence_all$sim_date[flag_exp]
    ls_score_hosp        <- sum(sqrt((data_incidence_all$new_hospital_admissions[flag_hosp_data] - hosp_adm_data$num_adm[flag_hosp_ref])^2),na.rm = T)
    hosp_data_ls[i_exp,] <- sum(ls_score_hosp)
    hosp_data_tag[i_exp,]<- unique(data_incidence_all$config_id[flag_exp])
    
  }
  
  # get mean ls per config id
  ls_summary_config        <- aggregate(hosp_data_ls,list(hosp_data_tag) , mean, na.rm=T)
  names(ls_summary_config) <- c('config_tag','ls_score_hosp')
  
  # save incidence data and scores
  run_tag           <- unique(project_summary$run_tag)
  file_name_path    <- file.path(project_dir,paste0(run_tag,'_incidence_processed.RData'))
  save(data_incidence_all,ls_summary_config,file=file_name_path)
  
  # select the 'num_selection' best parameter sets
  num_selection  <- min(num_selection,length(ls_summary_config$ls_score_hosp))
  ls_order       <- order(ls_summary_config$ls_score_hosp,decreasing = F)
  config_tag_sel <- ls_summary_config[ls_order[1:num_selection],]
  
  # select incidence data
  flag_plot               <- data_incidence_all$config_id %in% config_tag_sel$config_tag
  data_incidence_ensemble <- data_incidence_all[flag_plot,]
  
  
  ## ENSEMBLE  ####
  .rstride$create_pdf(project_dir,'incidence_ensemble',width = 6, height = 7)
  par(mfrow=c(4,1))
  
  # plot temporal patterns (ensemble)
  plot_incidence_data(data_incidence_ensemble,project_summary,
                      hosp_adm_data,input_opt_design,prevalence_ref,
                      bool_add_param)
  
  ## ENSEMBLE (SINGLE RUNS) ####
  opt_config_id <- config_tag_sel$config_tag
  i_config <- opt_config_id[1]
  for(i_config in opt_config_id){
    
    # select subset
    data_incidence_sel <- data_incidence_all[data_incidence_all$config_id == i_config,]
    
    # plot
    plot_incidence_data(data_incidence_sel,project_summary,
                        hosp_adm_data,input_opt_design,prevalence_ref,
                        bool_add_param)
  }
  
  # close pdf
  dev.off()
  
  
  ## ALL PLOTS ####
  .rstride$create_pdf(project_dir,'incidence_inspection',width = 6, height = 7)
  par(mfrow=c(4,1))
  
  opt_config_id <- unique(data_incidence_all$config_id)
  i_config <- opt_config_id[1]
  for(i_config in opt_config_id){
    
    # select subset
    data_incidence_sel <- data_incidence_all[data_incidence_all$config_id == i_config,]
    
    # plot
    plot_incidence_data(data_incidence_sel,project_summary,
                        hosp_adm_data,input_opt_design,prevalence_ref,
                        bool_add_param)
  }
  
  # close pdf
  dev.off()
  #--------------------------#
  
  
  ## ALL PLOTS (zoom) ####
  .rstride$create_pdf(project_dir,'incidence_inspection_zoom',width = 6, height = 7)
  par(mfrow=c(4,1))
  
  # select subset
  flag_dates <- data_incidence_sel$sim_date %in% (as.Date("2020-05-20"):as.Date("2020-06-20"))
  data_incidence_sel <- data_incidence_all[flag_dates,]
  # data_incidence_sel <- data_incidence_all[data_incidence_sel$sim_date > median(data_incidence_sel$sim_date,na.rm=TRUE),]
  
  # plot
  plot_incidence_data(data_incidence_sel,project_summary,
                      hosp_adm_data,input_opt_design,prevalence_ref,
                      bool_add_param)
  
  head(data_incidence_all)
  opt_config_id <- unique(data_incidence_all$config_id)
  i_config <- opt_config_id[1]
  for(i_config in opt_config_id){
    
    # select subset
    data_incidence_sel <- data_incidence_all[data_incidence_all$config_id == i_config,]
    data_incidence_sel <- data_incidence_sel[data_incidence_sel$sim_date > median(data_incidence_sel$sim_date,na.rm=TRUE),]
    
    # plot
    plot_incidence_data(data_incidence_sel,project_summary,
                        hosp_adm_data,input_opt_design,prevalence_ref,
                        bool_add_param)
  }
  
  # close pdf
  dev.off()
  #--------------------------#
  
  ## R0     ####
  ## PER R0: plot temporal patterns
  opt_r0 <- unique(input_opt_design$r0)
  if(length(opt_r0)>0){
    .rstride$create_pdf(project_dir,'incidence_R0',width = 6, height = 7)
    par(mfrow=c(4,1))
    
    
    i_r0 <- opt_r0[13]
    for(i_r0 in opt_r0){
      
      # select config_id
      opt_config_id <- unique(input_opt_design$config_id[input_opt_design$r0 ==  i_r0])

      # select subset
      data_incidence_sel <- data_incidence_all[data_incidence_all$config_id %in% opt_config_id,]
      dim(data_incidence_sel)
      
      # check selection
      if(nrow(data_incidence_sel)>0){
        # plot
        plot_incidence_data(data_incidence_sel,project_summary,
                            hosp_adm_data,input_opt_design,prevalence_ref,
                            bool_add_param)
      }
    }
    
    # close pdf
    dev.off()
  }
  
  ## PER contact tracing level ####
    opt_tracing <- unique(input_opt_design$tracing_id)
    if(length(opt_tracing)>0){
      .rstride$create_pdf(project_dir,'incidence_contact_trancing',width = 6, height = 7)
      par(mfrow=c(4,1))
      
      
      i_detection <- opt_tracing[1]
      for(i_tracing in opt_tracing){
        
        # select config_id
        opt_config_id <- unique(input_opt_design$config_id[input_opt_design$tracing_id ==  i_tracing])
        
        # select subset
        data_incidence_sel <- data_incidence_all[data_incidence_all$config_id %in% opt_config_id,]
        dim(data_incidence_sel)
        
        # check selection
        if(nrow(data_incidence_sel)>0){
          # plot
          plot_incidence_data(data_incidence_sel,project_summary,
                              hosp_adm_data,input_opt_design,prevalence_ref,
                              bool_add_param)
        }
      }
      
      # close pdf
      dev.off()
    }
  
  ## ALL TOGETHER (PDF) ####
  .rstride$create_pdf(project_dir,'incidence_all',width = 6, height = 2.5)
  par(mar=c(3,5,1,3))
  
  # plot
  plot_incidence_data(data_incidence_all,project_summary,
                      hosp_adm_data,input_opt_design,prevalence_ref,
                      bool_add_param,bool_only_hospital_adm = TRUE) 

  # close pdf
  dev.off()
  
  ## ALL TOGETHER (JPEG) ####
  .rstride$create_jpg(project_dir,'incidence_all',width = 6, height = 2.5)
  par(mar=c(3,5,1,5))
  
  # plot
  plot_incidence_data(data_incidence_all,project_summary,
                      hosp_adm_data,input_opt_design,prevalence_ref,
                      bool_add_param,bool_only_hospital_adm = TRUE) 
  
  # close pdf
  dev.off()
  
  ## ALL TOGETHER: NO PARAM ####
  .rstride$create_pdf(project_dir,'incidence_no_param',width = 6, height = 2.5)
  par(mar=c(3,5,1,3))

  # plot
  plot_incidence_data(data_incidence_all,project_summary,
                      hosp_adm_data,input_opt_design,prevalence_ref,
                      bool_add_param = FALSE,
                      bool_only_hospital_adm = FALSE) 
  
  # close pdf
  dev.off()
  
  
   #--------------------------#
  # parameters
  
  if(nrow(input_opt_design)>1){
    flag_param   <- input_opt_design$config_id %in% config_tag_sel$config_tag
    print(input_opt_design[flag_param,-ncol(input_opt_design)])
    
    # flag_param <- project_summary$config_id %in% config_tag_sel$config_tag
    # project_summary[flag_param,]
  }
  
  
   # command line message
  smd_print('INSPECTION OF INCIDENCE DATA COMPLETE')
  
} # end function

plot_incidence_data <- function(data_incidence_sel,project_summary,
                                hosp_adm_data,input_opt_design,prevalence_ref,
                                bool_add_param,
                                bool_add_axis4 = TRUE,
                                bool_only_hospital_adm = FALSE,
                                bool_seroprev_limited = FALSE){

  # change figure margins
  if(!bool_only_hospital_adm){
    par(mar=c(3,5,1,5))
  }
  
  # set Belgian population
  pop_size_be <- 11e6
  
  # set color definitions and other layout definitions
  pcolor <- data.frame(E = "black",  # exposed (or total infections)
                       I = "darkgoldenrod3",  # infectious
                       S = "red",  # symptomatic
                       H = 'blue',  # hospitalized
                       D = 'black',
                       alpha = 0.1,
                       lwd = 3,
                       pch = 20 , # if points are used
                       stringsAsFactors = F)  # data
  
  # change transparancey for low number of rng-runs
  if(max(table(project_summary$config_id)) < 10){
    pcolor$alpha <- 0.2
  }
  
  ## FIX FOR PLOTTING: Set all values for the last sim_day to NA
  data_incidence_sel[data_incidence_sel$sim_date %in% max(data_incidence_sel$sim_date,na.rm=T),] <- NA
  
  
  # set y-lim
  y_lim <- range(0,max(hosp_adm_data$num_adm)*1.2,max(data_incidence_sel$new_hospital_admissions,na.rm=T),na.rm=T)

  ## HOSPITAL ADMISSIONS ####
  plot(data_incidence_sel$sim_date,
       data_incidence_sel$new_hospital_admissions,
       type='l',
       col=alpha(pcolor$H,pcolor$alpha),
       ylab='Hospital admissions',
       xlab='',
       ylim = y_lim,
       yaxt='n',
       xaxt='n')
  add_x_axis(data_incidence_sel$sim_date)
  add_y_axis(y_lim)
  points(hosp_adm_data$date,hosp_adm_data$num_adm,col=pcolor$D,pch=pcolor$pch)
  add_breakpoints()
  add_legend_hosp(pcolor)
  
  # add config tag
  adm_mean_final <- aggregate(new_hospital_admissions ~ sim_date + config_id + contact_id, data=data_incidence_sel,mean)
  adm_mean_final <- adm_mean_final[adm_mean_final$sim_date == max(adm_mean_final$sim_date),]
  # text(adm_mean_final$sim_date,adm_mean_final$new_hospital_admissions,adm_mean_final$config_id,
  #      pos=4,xpd=TRUE,cex=0.4)
  if(bool_add_axis4){
    if(bool_only_hospital_adm){
      axis(4,adm_mean_final$new_hospital_admissions,adm_mean_final$contact_id,las=2,cex.axis=0.6) # add contact details
    } else{
      axis(4,adm_mean_final$new_hospital_admissions,adm_mean_final$config_id,las=2,cex.axis=0.4) # add full config id
    }  
  }
  
  
  
  if(bool_only_hospital_adm){ return() } # stop
  
  ## CUMULATIVE: HOSPITAL ####
  y_lim <- range(0,max(hosp_adm_data$cum_adm)*3.5,na.rm=T)
  plot(data_incidence_sel$sim_date,
       data_incidence_sel$cumulative_hospital_cases,
       type='l',
       col=alpha(pcolor$H,pcolor$alpha),
       ylab='Total admissions',
       xlab='',
       yaxt='n',
       xaxt='n',
       ylim= range(y_lim))
  add_x_axis(data_incidence_sel$sim_date)
  add_y_axis(y_lim)
  add_y_axis_pop(y_lim,pop_size_be)
  points(hosp_adm_data$date,hosp_adm_data$cum_adm,col=pcolor$D,pch=pcolor$pch)
  add_breakpoints()
  add_legend_hosp(pcolor)
  
  lines(data_incidence_sel$sim_date,
       data_incidence_sel$cumulative_hospital_cases_age1,
       col=5)
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cumulative_hospital_cases_age2,
        col=6)
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cumulative_hospital_cases_age3,
        col=7)
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cumulative_hospital_cases_age4,
        col=8)
  
  legend('left',
         rev(c('0-18',
           '19-59',
           '60-79',
           '+80')),
         col=c(8:5),
         lwd=2,
         title='Age group',
         cex=0.5
  )
  
  ## INCIDENCE: ALL ####
  y_lim <- range(0,data_incidence_sel$new_infections,na.rm = T)
  plot(data_incidence_sel$sim_date,
       data_incidence_sel$new_infections,
       type='l',
       col=alpha(pcolor$E,pcolor$alpha),
       ylab='New cases',
       xlab='',
       ylim = y_lim,
       yaxt='n',
       xaxt='n')
  add_x_axis(data_incidence_sel$sim_date)
  add_y_axis(y_lim)
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$new_infectious_cases,
        col=alpha(pcolor$I,pcolor$alpha))
  
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$new_symptomatic_cases,
        col=alpha(pcolor$S,pcolor$alpha))
  
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$new_hospital_admissions,
        col=alpha(pcolor$H,pcolor$alpha))
  # points(hosp_adm_data$date,hosp_adm_data$num_adm,col=pcolor$D,pch=pcolor$pch)
  add_breakpoints()
  add_legend_incidence(pcolor)
  
  ## CUMULATIVE: ALL STATES ####
  y_lim <- pretty(data_incidence_sel$cumulative_infections)
  plot(data_incidence_sel$sim_date,
       data_incidence_sel$cumulative_infections,
       type='l',
       col=alpha(pcolor$E,pcolor$alpha),
       ylab='Total cases',
       xlab='',
       yaxt='n',
       xaxt='n')
  add_x_axis(data_incidence_sel$sim_date)
  add_y_axis(y_lim)
  add_y_axis_pop(y_lim,pop_size_be)
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cumulative_infectious_cases,
        col=alpha(pcolor$I,pcolor$alpha))
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cumulative_symptomatic_cases,
        col=alpha(pcolor$S,pcolor$alpha))
  lines(data_incidence_sel$sim_date,
        data_incidence_sel$cumulative_hospital_cases,
        col=alpha(pcolor$H,pcolor$alpha))
  #points(hosp_adm_data$date,hosp_adm_data$cum_adm,col=pcolor$D,pch=pcolor$pch)
  
  add_breakpoints()
  if(bool_add_param) {
    add_legend_runinfo(project_summary,input_opt_design,
                       unique(data_incidence_sel$config_id))
  } else {
    add_legend_incidence(pcolor,legend_pos = 'topleft',bool_seroprevalence=TRUE)
  }
  

  ## add reference
  prevalence_selection <- as.Date(c('2020-03-15','2020-04-1','2020-04-15','2020-05-01'))
  if(bool_seroprev_limited){
    prevalence_selection <- as.Date(c('2020-03-30','2020-04-20'))
  }
  flag_prevalence <- prevalence_ref$date %in% prevalence_selection
  #points(prevalence_selection,prevalence_ref$mean[flag_prevalence]*pop_size_be,pch=8)
  arrows(prevalence_selection,prevalence_ref$min[flag_prevalence]*pop_size_be,
         prevalence_selection,prevalence_ref$max[flag_prevalence]*pop_size_be,
         angle=90,length=0.05)
  arrows(prevalence_selection,prevalence_ref$max[flag_prevalence]*pop_size_be,
         prevalence_selection,prevalence_ref$min[flag_prevalence]*pop_size_be,
         angle=90,length=0.05)
  

} # end function to plot figure

# define the vertical breaks on the plots
add_breakpoints <- function(bool_text=TRUE){
  
  # add start intervention
  add_vertical_line("2020-03-14",bool_text)
  
  # # add today
  # add_vertical_line(Sys.Date())
  
  # add scenario date (exit wave 1)
  add_vertical_line("2020-05-04",bool_text)
  
  # add scenario date (exit wave 2)
  add_vertical_line("2020-05-18",bool_text)
  
  # add scenario date (summer holiday)
  add_vertical_line("2020-07-01",bool_text)
}

# add vertical line on given date + label on x-axis
add_vertical_line <- function(date_string,bool_text){
  
  plot_limits <- par("usr")
  
  v_date <- as.Date(date_string)
  abline(v=v_date,lty=2)
  #axis(1,v_date,format(v_date,'%d/%m'),
       #cex.axis=0.5,padj=-3,tck=-0.005)
  if(bool_text)
  {
    text(x = v_date,
       y = mean(plot_limits[3:4]),
       format(v_date,'%d/%m'),
       srt=90, pos=3, offset = +1.5,cex=0.6)
  }
}
  

# define the legend for hospital(-only) plots
add_legend_hosp <- function(pcolor){
  legend('topleft',
         c('Reported',
           'Simulation'),
         col=c(pcolor$D,pcolor$H),
         pch=c(16,NA),
         lwd=c(NA,2),
         bg='white',
         cex = 0.6,
         ncol=1)
}

# define the legend with all categories
add_legend_incidence <- function(pcolor,legend_pos = 'topright',bool_seroprevalence=FALSE){
  
  legend_text <- c('Exposed (latent)',
                   'Infectious',
                   'Symptomatic',
                   'Hospitalized')
  legend_col <- unlist(pcolor)
  legend_pch <- rep(NA,length(legend_text))
  legend_lwd <- rep(2,length(legend_text))
  
  if(bool_seroprevalence){
    legend_text <- c(legend_text,'Seroprevalence')
    legend_col  <- c(legend_col,1)
    legend_pch  <- c(legend_pch,124)
    legend_lwd  <- c(legend_lwd,NA)
  }
  
  legend(legend_pos,
         legend_text,
         col=legend_col,
         pch=legend_pch,
         lwd=legend_lwd,
         cex=0.6,
         bg='white')
}

add_legend_runinfo <- function(project_summary,input_opt_design,
                               i_config){
  
  # remove NA from i_config
  i_config <- i_config[!is.na(i_config)]
  
  # subset project summary
  project_summary_sel <- project_summary[project_summary$config_id %in% i_config,]
  
  # get run info: population
  run_info <- c(paste0('pop_size = ',unique(project_summary_sel$population_size)/1e3,'k'))

  # add other exp_design parameters (excl. ids)
  names_exp_param <- colnames(input_opt_design)
  flag_col        <- names_exp_param %in% c('config_id','contact_id','tracing_id')
  names_exp_param <- names_exp_param[!flag_col]
  if(length(names_exp_param)>0){
    for(i_name in names_exp_param){
      run_info <- c(run_info,
                    paste0(i_name, ' = ',paste(unique(project_summary_sel[,i_name]),collapse=', '))
                    )
    }
    
  }
  
  # present info in legend
  legend('topleft',
         run_info,
         cex=0.6,
         bg='white')
}


plot_distancing <- function(project_summary){
  
  opt_distancing <- unique(data.frame(compliance_delay = project_summary$compliance_delay,
                                 cnt_reduction_work = project_summary$cnt_reduction_work,
                                 cnt_reduction_other = project_summary$cnt_reduction_other))


  date_start <- as.Date('2020-03-14')
  
  i_distancing <- 1    
  
  opt_distancing[i_distancing,]
  num_days <- opt_distancing$compliance_delay[i_distancing]
  date_all <- date_start + (1:num_days)-1
  compliance_factor <- 1:num_days / num_days
  
  plot(date_all,compliance_factor*opt_distancing$cnt_reduction_work[i_distancing],
       type='l',
       col=4,
       ylim=0:1)
  lines(date_all,compliance_factor*opt_distancing$cnt_reduction_other[i_distancing])
        
    
  
}

add_x_axis <- function(sim_dates){
  x_ticks <- pretty(sim_dates,5)
  axis(1,x_ticks, format(x_ticks,'%e %b'),cex.axis=0.9)
  abline(v=pretty(sim_dates,7),lty=3,col='lightgray')
  #grid(nx=NA,ny=NULL,lty=3,col='lightgray')
}

add_y_axis <- function(y_lim){
  
  y_ticks <- pretty(y_lim,5)
  y_labels <- y_ticks # default
  if(max(y_ticks)>1e4){ y_labels <- paste0(round(y_ticks/1e3),'k')}
  if(max(y_ticks)>1e6){ y_labels <- paste0(round(y_ticks/1e6,digits=2),'M')}
  
  axis(2,y_ticks, y_labels,cex.axis=0.9,las=2)
  abline(h=pretty(y_lim,5),lty=3,col='lightgray')
}

add_y_axis_pop <- function(y_lim,pop_size){
  
  # add axis
  axis(4,pretty(y_lim),paste0(round(pretty(y_lim)/pop_size*100,digits=1),'%'),las=2,cex.axis=0.9)
  mtext('Belgian population (%)',side = 4,line=3,cex=0.7)
}



## HELP FUNCTION TO REFORMAT THE PREVALENCE DATA FROM THE STOCHASTIC MODEL
reformat_prevalence_stochastic_model <- function(){
  
  ref_prevalence <- read.table('./data/prevalence_stochastic_model_20200604.csv',sep=';')
  dim(ref_prevalence)  

  date_steps <- seq(as.Date('2020-03-01'),as.Date('2020-06-01'),1/24)
  num_steps  <- length(date_steps)
  plot(date_steps[-1],colMeans(ref_prevalence[,2:num_steps]),col=2,
       ylim=range(ref_prevalence[,-1]),type='l',xaxt='n',
       xlab='',
       ylab='prevalence stochastic model')
  lines(date_steps[-1],apply(ref_prevalence[,2:num_steps],2,min),col=3)
  lines(date_steps[-1],apply(ref_prevalence[,2:num_steps],2,max),col=3)
  add_breakpoints()
  add_x_axis(date_steps)
  grid(nx=NA,ny=NULL)
  
  date_out <- seq(as.Date('2020-03-01'),as.Date('2020-06-01'),1)
  prevalence_out <- data.frame(mean = approx(date_steps[-num_steps],colMeans(ref_prevalence[,2:num_steps]),date_out)$y,
                               min = approx(date_steps[-num_steps],apply(ref_prevalence[,2:num_steps],2,min),date_out)$y,
                               max = approx(date_steps[-num_steps],apply(ref_prevalence[,2:num_steps],2,max),date_out)$y,
                              date = date_out)
  
  approx(date_steps[-num_steps],colMeans(ref_prevalence[,2:num_steps]),seq(as.Date('2020-04-01'),as.Date('2020-06-01'),15))

  head(prevalence_out)
  saveRDS(prevalence_out,file='./sim_output/prevalence_stochastic_model_20200604.rds')
  
  

}




