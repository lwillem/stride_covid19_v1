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
#  Copyright 2020, Willem L et al.
############################################################################# #
#
# CONTACT TRACING ENSEMBLE
#
############################################################################# #

rm(list=ls())

source('./bin/rstride/rStride.R')

# set directory name with results and get output files
# dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200518_results'
# dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200531_results_cts/'
#dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19/20200618_cts_optim/'
dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19_revision/20201102_1_main'
#dir_results <- '/Users/lwillem/Documents/university/research/stride/results_covid19_revision/20201102_3_robustness/n40'

file_name_incidence <- dir(dir_results,pattern='incidence.RData',recursive = T,full.names = T)

select_cts_results <- function(file_names){
        flag_files <- grepl('int1_',file_names) |
                grepl('int17',file_names) |
                grepl('cts1',file_names) |
                grepl('cts2',file_names) |
                grepl('cts3',file_names)
        return(file_names[flag_files])
}

file_name_incidence <- select_cts_results(file_name_incidence)
file_name_incidence
# file_name_incidence <- file_name_incidence[grepl('n40',file_name_incidence)]


# output tag
output_tag <- paste0(format(Sys.Date(),'%Y%m%d'),'_results')

## LOAD DATA ----
i_file <- 2
# load and aggregate results
foreach(i_file = 1:length(file_name_incidence),
.combine = rbind) %do% {
        
        # load results
        load(file_name_incidence[i_file])
        data_incidence_all <- data_all
        names(data_incidence_all)
        
        # parse scenario name
        scenario_name <- substr(basename(file_name_incidence[i_file]),16,100)
        scenario_name <- gsub('_data_incidence.RData','',scenario_name)
        scenario_name <- gsub('_int','scen',scenario_name)
  
        # fix single digit numbers
        if(grepl('scen._',scenario_name)){
                scenario_name <- gsub('scen','scen0',scenario_name)
        }
        
        scenario_name
        
        if(!'location_HouseholdCluster' %in% names(data_incidence_all)){
                data_incidence_all$location_HouseholdCluster <- 0
        }
        
        # remove location NA
        data_incidence_all$location_NA <- NULL
        
        # add scenario name
        data_incidence_all$scenario <- scenario_name
        # data_incidence_all$scenario_id <- as.numeric(substr(scenario_name,5,6))
        
        # add project_dir
        project_dir <- dirname(file_name_incidence[i_file])
        data_incidence_all$project_dir <- project_dir
        
        # config_id
        project_summary <- .rstride$load_project_summary(project_dir)
        input_opt       <- .rstride$get_variable_model_param(project_summary)
        
        project_summary$config_id <- .rstride$get_config_id(project_summary)
        
        # to generate contact tracing id
        flag_opt_input_tracing <- !(grepl('cnt_reduction',names(input_opt)) | grepl('config_id',names(input_opt)))
        colnames_tracing           <- names(input_opt)[flag_opt_input_tracing]
        if(length(colnames_tracing) == 1) {
                project_summary$tracing_id  <- project_summary[,colnames_tracing]
        } else{
                project_summary$tracing_id  <- apply(project_summary[,colnames_tracing],1,paste, collapse='_')
        }
        
        # add contact id
        flag_opt_input_tracing <- (grepl('cnt_reduction',names(project_summary)) & grepl('exit',names(project_summary)))
        if(any(flag_opt_input_tracing)){
                colnames_contact           <- names(project_summary)[flag_opt_input_tracing]
                project_summary$contact_id  <- apply((1-project_summary[,colnames_contact])*100,1,paste, collapse=',')
        } else{
                project_summary$contact_id <- NA
        }
        
        # add config_id and tracing_id to incidence data
        data_incidence_all         <- merge(data_incidence_all,project_summary[,c('exp_id','config_id','tracing_id','contact_id')] )
        
        
        # check
        smd_print(i_file,scenario_name)
        smd_print(dim(data_incidence_all))
        
        # return
        data_incidence_all
} -> data_incidence_scenario

# make copy and add month
data_incidence <- data_incidence_scenario
data_incidence$sim_month <- format(data_incidence$sim_date,'%B')
table(data_incidence$scenario)


## CONFIG FILE ----
file_name_summary <- dir(dir_results,pattern='summary.csv',recursive = T,full.names = T)
file_name_summary <- select_cts_results(file_name_summary)

i_file <- 3
# load and aggregate results
foreach(i_file = 1:length(file_name_summary),
        .combine = rbind) %do% {
          
          # load results
          project_summary <- .rstride$load_project_summary(project_dir = dirname(file_name_summary[i_file]))

          # parse scenario name
          scenario_name <- substr(basename(file_name_incidence[i_file]),16,100)
          scenario_name <- gsub('_data_incidence.RData','',scenario_name)
          scenario_name <- gsub('_int','scen',scenario_name)

          # fix single digit numbers
          if(grepl('scen._',scenario_name)){
            scenario_name <- gsub('scen','scen0',scenario_name)
          }
          project_summary$scenario <- scenario_name
          
          # names(project_summary)
          # if(!any(grepl('tracing_efficency_household',names(project_summary)))){
          #         print('typo!')
          #         project_summary$tracing_efficency_household <- 0.9
          # }
          # if(!any(grepl('tracing_efficency_other',names(project_summary)))){
          #         print('typo!')
          #         project_summary$tracing_efficency_other <- 0.5
          # }
          
          
          # retrieve all variable model parameters
          input_opt_design   <- .rstride$get_variable_model_param(project_summary)
          
          names(input_opt_design)
          
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
          flag_opt_input_tracing <- (grepl('cnt_reduction',names(project_summary)) & grepl('exit',names(project_summary)))
          if(any(flag_opt_input_tracing)){
                  colnames_contact           <- names(project_summary)[flag_opt_input_tracing]
                  project_summary$contact_id  <- apply((1-project_summary[,colnames_contact])*100,1,paste, collapse=',')
          } else{
                  project_summary$contact_id <- NA
          }
          
          # fix for typo "efficency"
#          names(project_summary) <- gsub('efficency','efficiency',names(project_summary))
          #project_summary$school_system_adjusted <- NULL
          smd_print(i_file,scenario_name)
          smd_print(dim(project_summary))
          
          # return
          project_summary
        } -> project_summary_scenario

project_summary_scenario$tracing_id

dim(project_summary_scenario)
dim(data_incidence)
unique(project_summary_scenario$tracing_id)
table(project_summary_scenario$contact_id)


##### MONTLY AVERAGE ####
## AGGREGATE => AVERAGE

flag_date     <- data_incidence$sim_date %in% (as.Date('2020-08-01'):as.Date('2020-08-31'))
mean_hosp_adm <- aggregate(new_hospital_admissions ~ contact_id + tracing_id + scenario, data_incidence[flag_date,], mean)
names(data_incidence)

table(mean_hosp_adm$contact_id)
table(project_summary_scenario$contact_id)
table(mean_hosp_adm$tracing_id)
table(project_summary_scenario$tracing_id)
table(mean_hosp_adm$scenario)
table(project_summary_scenario$scenario)

names(mean_hosp_adm)
names(project_summary_scenario)

tmp_summary <- unique(project_summary_scenario[,c('scenario','tracing_id','detection_probability','tracing_efficiency_household',
                                                  'tracing_efficiency_other','case_finding_capacity','delay_isolation_index',
                                                  'delay_contact_tracing','test_false_negative')])
names(mean_hosp_adm)
names(tmp_summary)

mean_hosp_adm <- merge(mean_hosp_adm,tmp_summary)
head(mean_hosp_adm)

## REFERENCE ----
# flag_reference  <- data_incidence$tracing_id == "" & flag_date & data_incidence$contact_id == "40,30"
flag_reference <- grepl('scen01',mean_hosp_adm$scenario) &  mean_hosp_adm$contact_id == "50,30,50"
#flag_reference  <- data_incidence$scenario_id == 1 & data_incidence$contact_id == '40,30,-1300' & data_incidence$sim_date == max(data_incidence$sim_date)
#mean_hosp_adm_ref <- aggregate(cumulative_hospital_cases ~ tracing_id + scenario, data_incidence[flag_reference,], mean)
mean_hosp_adm_ref <- mean_hosp_adm[flag_reference,]
mean_hosp_adm_ref

mean_hosp_adm$relative_hospital_admissions <- mean_hosp_adm$new_hospital_admissions / mean_hosp_adm_ref$new_hospital_admissions

flag_baseline <- grepl('scen17',mean_hosp_adm$scenario) &  mean_hosp_adm$contact_id == "50,30,50"
mean_hosp_adm$relative_hosp_baseline <- mean_hosp_adm$relative_hospital_admissions[flag_baseline] 

## PLOT FUNCTION: MATRIX FORMAT ----
plot_cts_matrix_avg <- function(mean_hosp_adm_sel,
                                col1_name,col1_tag, col1_unit,
                                col2_name,col2_tag, col2_unit,
                                legend_label){
        
        
        col1_values <- mean_hosp_adm_sel[,col1_name]
        col1_opt    <- unique(col1_values)
        col1_labels <- col1_opt
        if(col1_unit=='%') { col1_labels <- paste0(col1_labels*100,'%')}
        
        col2_values <- mean_hosp_adm_sel[,col2_name]
        col2_opt    <- unique(col2_values)
        col2_labels <- col2_opt
        if(col2_unit=='%') { col2_labels <- paste0(col2_labels*100,'%')}
        
        # initiate matrix
        cts_matrix <- matrix(NA, nrow=length(col1_opt), ncol=length(col2_opt))
        
        # fill matrix manual... to be sure!
        i_out <- 3
        for(i_out in 1:nrow(mean_hosp_adm_sel)){
                i_row = mean_hosp_adm_sel[i_out,col1_name] == col1_opt
                i_col = mean_hosp_adm_sel[i_out,col2_name] == col2_opt
                cts_matrix[i_row,i_col] <- mean_hosp_adm_sel$relative_hospital_admissions[i_out]
        }
        
        # limit at 1
        cts_matrix[cts_matrix>1] <- 1
        
        # calculate reduction
        cts_matrix <- 1 - cts_matrix
        
        # convert to %
        cts_matrix <- cts_matrix*100
        
        # set color space
        redc <- rev(colorspace::sequential_hcl(5))
        
        legend_range <- range(pretty(cts_matrix))
        # if(max(legend_range)<80){
        #    legend_range <- c(35,65)
        #    redc <- rev(colorspace::sequential_hcl(6))
        # }
        
        # set figure margins
        par(mar=c(5, 6, 2, 7),mgp=c(3,0.5,0))
        p <- simage(s = cts_matrix, 
                    xlab=col1_tag,
                    ylab=col2_tag, 
                    legend.width=1.5,
                    #legend.lab = legend_label,
                    legend.mar = 6,
                    #legend.shrink = 0.8,
                    #cex.lab = 2,
                    #slim=c(min(cts_matrix,na.rm=T), 1), 
                    slim=legend_range, 
                    cex.lab=1,
                    cex.main=1.2, 
                    las=0.1,
                    xaxt="n",
                    yaxt="n",
                    col=redc)
        
        # set x-axis 
        plt_xticks <- seq(0,1,length=nrow(cts_matrix))
        axis(1, at=plt_xticks, labels = col1_labels,cex.axis=1.0,tick = FALSE)
        
        # set y-axis 
        plt_yticks <- seq(0,1,length=ncol(cts_matrix))        
        axis(2, at=plt_yticks, labels = col2_labels,cex.axis=1.0,tick = FALSE,las=1)
        
        # set legend label(with adjusted size)
        mtext(legend_label,side=4,cex=0.6,padj=2.8)
        
        print(cts_matrix)
}


## OPEN PDF STREAM ----
pdf(smd_file_path(dir_results,paste0(output_tag,'_manuscript_CTS_august.pdf')),10,3)
par(mar=c(5,5,2,1),mfrow=c(1,3))
#par(mar=c(4,4,1,2))

##################################################################################### #
## DETECTION ----

flag_scen <- grepl('cts3',mean_hosp_adm$scenario)
mean_hosp_adm_sel <- mean_hosp_adm[flag_scen,]

col1_name   <- 'test_false_negative'
col1_tag    <- "False negative predictive value"
col1_unit   <- '%'

col2_name <- 'detection_probability'
col2_tag    <- "Symptomatic cases included as index cases"
col2_unit   <- '%'

## RELATIVE
plot_cts_matrix_avg(mean_hosp_adm_sel,
                    col1_name,col1_tag, col1_unit,
                    col2_name,col2_tag, col2_unit,
                    legend_label='Reduction of hospital admissions (%)')
points(0/3,2/3,pch=4)
points(8.05/5,2.5/3,pch=4,xpd=T)


##################################################################################### #
## DELAY ----

flag_scen <- grepl('cts2',mean_hosp_adm$scenario)
mean_hosp_adm_sel <- mean_hosp_adm[flag_scen,]


mean_hosp_adm_sel$delay_isolation_index_str <- paste0('D',mean_hosp_adm_sel$delay_isolation_index)
mean_hosp_adm_sel$delay_contact_tracing_str <- paste0('Di+',mean_hosp_adm_sel$delay_contact_tracing)


col1_name   <- 'delay_isolation_index_str'
col1_tag    <- "Isolation index case"
col1_unit   <- 'days'

col2_name <- 'delay_contact_tracing_str'
col2_tag    <- "Test and isolation infected contacts"
col2_unit   <- 'days'

## RELATIVE
plot_cts_matrix_avg(mean_hosp_adm_sel,
                    col1_name,col1_tag, col1_unit,
                    col2_name,col2_tag, col2_unit,
                    legend_label='Reduction of hospital admissions (%)')

points(0,0,pch=4)
points(6.93/5,5/5,pch=4,xpd=T)

############################################################################@ #
## SUCCES RATE
flag_scen <- grepl('cts1',mean_hosp_adm$scenario)
mean_hosp_adm_sel <- mean_hosp_adm[flag_scen,]

col1_name   <- 'tracing_efficiency_household'
col1_tag    <- "CTS success rate for household contacts"
col1_unit   <- '%'

col2_name <- 'tracing_efficiency_other'
col2_tag    <- "CTS success rate for non-household contacts"
col2_unit   <- '%'

## RELATIVE
plot_cts_matrix_avg(mean_hosp_adm_sel,
                    col1_name, col1_tag, col1_unit,
                    col2_name, col2_tag, col2_unit,
                    legend_label='Reduction of hospital admissions (%)')
points(3/3,2/3,pch=4)
points(7.4/5,2.3/3,pch=4,xpd=T)



# close pdf stream
dev.off()





