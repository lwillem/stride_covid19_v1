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
#  Copyright 2020, Willem L et al.
#############################################################################
#
# Additional figures for the paper revision
#
#############################################################################

# Clear work environment
rm(list=ls())

# load rStride
source('./bin/rstride/rStride.R')

# read disease xml
# plot probabilities

disease_data <- xmlToList('data/disease_covid19_lognorm.xml')

p_start_symp  <- as.numeric(disease_data$start_symptomatic)
p_time_asymp  <- as.numeric(disease_data$time_asymptomatic)
p_time_infect <- as.numeric(disease_data$time_infectious)
p_time_symp   <- as.numeric(disease_data$time_symptomatic)

# convert cumulative probability (0:n) into day-specific probability (1:n)
p_start_symp  <- diff(p_start_symp)
p_time_asymp  <- diff(p_time_asymp)
p_time_infect <- diff(p_time_infect)
p_time_symp   <- diff(p_time_symp)

n_sample <- 1e5
start_symp  <- sample(1:length(p_start_symp), size = n_sample, p_start_symp,replace = T)
time_asymp  <- sample(1:length(p_time_asymp), size = n_sample, p_time_asymp,replace = T)
time_infect <- sample(1:length(p_time_infect),size = n_sample, p_time_infect,replace = T)
time_symp   <- sample(1:length(p_time_symp),  size = n_sample, p_time_symp,replace = T)

flag_unvalid <- start_symp - time_asymp < 1
table(flag_unvalid) / n_sample
start_symp  <- start_symp[!flag_unvalid]
time_asymp  <- time_asymp[!flag_unvalid]
time_infect <- time_infect[!flag_unvalid]

add_hist_vline <- function(bool_infection = TRUE){
  
  # add grid
  grid(nx=NA,ny=NULL)
  
  abline(v=0,lty=2)
  
  if(bool_infection)
    text(0, par("usr")[4]*2/3,'infection',srt=90,pos=2,cex=0.7)
  else
    text(0, par("usr")[4]*2/3,'symptom onset',srt=90,pos=4,cex=0.7)
}


# open pdf stream
pdf('stride_disease_distribtions.pdf',5,5)

dist_start_symp <- hist(start_symp,right=F,0:22-0.5,freq = F,
                        xlab='Days since infection',
                        ylab='Probability',
                        ylim=c(0,0.25),
                        main='Incubation period')
add_hist_vline()

dist_start_symp <- hist(-time_asymp,right=F,-10:0-0.5,freq = F,
                        xlab='Days since symptom onset',
                        ylab='Probability',
                        ylim=c(0,0.5),
                        xlim=c(-10,0.5),
                        main='Start infectious period\n')
add_hist_vline(bool_infection = F)

dist_start_inf <- hist(start_symp - time_asymp,right=F,0:22-0.5,freq = F,
                       xlab='Days since infection',
                       ylab='Probability',
                       ylim=c(0,0.25),
                       main='Start infectious period')
add_hist_vline()

dist_symp_inf <- hist(time_infect,right=F,0:22-0.5,freq = F,
                      xlim=c(0,14),
                      ylim=c(0,0.5),
                      xlab='Days',
                      ylab='Probability',
                      main='Infectious period\n(total)')

dist_symp_inf <- hist(time_infect - time_asymp,right=F,-5:22-0.5,freq = F,
                      ylim=c(0,0.5),
                      xlab='Days since symptom onset',
                      ylab='Probability',
                      main='Infectious period\n(after symptom onset)')
add_hist_vline(bool_infection = F)


# Incubation period ----
# from Li et al NEJM 2020
# lognormal mean = 5.2; 95% CI = c(4.1, 7.0)
ln.par1 = 1.434065
ln.par2 = 0.6612

# Explore incubation period 
curve(dlnorm(x, ln.par1, ln.par2), from=0, to=14, axes=F, ann=T, 
      ylim=c(0,0.3), xlim=c(0,14),
      xlab='Days from infection to symptom onset',
      ylab = 'Density')
axis(2, las=1, at=0:3*0.1, lab=paste0(0:3*10,'%'))
axis(1, at=0:5*3, lab=0:5*3, cex.axis=1)

# --- start infectious period ----
# He et al
inf.par1 = 20.516508
inf.par2 = 1.592124
inf.par3 = 12.272481
plot(NA, axes=F, ann=T, ylim=c(0,0.3), xlim=c(-10,8),
     xlab = 'Days after symptom onset',
     ylab='Density')
axis(2, las=1, at=0:3*0.1, lab=paste0(0:3*10,'%'))
abline(v=0, col=gray(0.8))
curve(dgamma(x+inf.par3, inf.par1, inf.par2), from=-10, to=8, add=T)
axis(1, at=(-5:4)*2, lab=(-5:4)*2, cex.axis=1)

# close pdf stream
dev.off()

par(mfrow=c(2,3))
# SIMULATION BASED ----
project_dir <- "/Users/lwillem/Documents/university/research/stride/results_covid19_revision/20200916_r0_timeInfectious/20200917_092033_r0_d6"
#project_dir <- "/Users/lwillem/Documents/university/research/stride/results_covid19_revision/20200916_r0_timeInfectious/20200917_155422_r0_d6_child/"

data_person_all      <- .rstride$load_aggregated_output(project_dir,'data_participants')
dim(data_person_all)
names(data_person_all)

hist(data_person_all$start_symptomatic,right=F,0:22-0.5,freq = F,
                        xlab='Days since infection',
                        ylab='Probability',
                        ylim=c(0,0.25),
                        main='Incubation period\n(from infection to symptom onset)')
add_hist_vline()


hist(data_person_all$start_infectiousness - data_person_all$start_symptomatic,right=F,-10:1-0.5,freq = F,
                        xlab='Days',
                        ylab='Probability',
                        ylim=c(0,0.5),
                        xlim=c(-10,0.5),
                        main='Start infectious period\n')
add_hist_vline(bool_infection = F)

hist(data_person_all$start_infectiousness,right=F,0:22-0.5,freq = F,
     xlab='Days since infection',
     ylab='Probability',
     ylim=c(0,0.25),
     main='Start infectious period')
add_hist_vline()

time_infect <- data_person_all$end_infectiousness - data_person_all$start_infectiousness
hist(time_infect,right=F,0:22-0.5,freq = F,
                      ylim=c(0,0.5),
                      xlab='Days',
                      ylab='Probability',
                      main='Infectious period')

time_infect_sym <- data_person_all$end_infectiousness - data_person_all$start_symptomatic
hist(time_infect_sym,right=F,-5:22-0.5,freq = F,
                      ylim=c(0,0.5),
                      xlab='Days since symptom onset',
                      ylab='Probability',
                      main='Infectious period after symptom onset')
add_hist_vline(bool_infection = F)


## RELATIVE PROPORTION HOSPITAL ADMISSIONS BY AGE

# set age categories
age_cat_breaks <- c(0,19,59,79,110)

names(data_person_all)
data_person_all <- data.table(data_person_all)

# set age categories and whether the infection was symptomatic
data_person_all$age_cat <- cut(data_person_all$part_age,age_cat_breaks,right=T,include.lowest = T)
data_person_all$bool_symptomatic <- data_person_all$start_symptomatic != data_person_all$end_symptomatic

# aggregate
table_age_symp <- table(data_person_all$age_cat,data_person_all$bool_symptomatic)
distr_age_symp <- data.frame(matrix(table_age_symp,nrow=nrow(table_age_symp),ncol=ncol(table_age_symp)))
names(distr_age_symp) <- paste0('symp_',colnames(table_age_symp))
distr_age_symp$prop_symp_age   <- distr_age_symp$symp_TRUE / sum(distr_age_symp$symp_TRUE + distr_age_symp$symp_FALSE)
distr_age_symp$prop_symp_total <- distr_age_symp$symp_TRUE / sum(distr_age_symp$symp_TRUE)

# add age categories and check
distr_age_symp$age_cat   <- rownames(table_age_symp)
distr_age_symp


# hospital survey data from Faes et al (medxriv)
num_hosp_age <- c(227, 4166, 4717, 3254)
p_hosp <- prop_hosp_age <- num_hosp_age / sum(num_hosp_age)


# calculate relative proportion hospital admission 
factor_hosp <- p_hosp / distr_age_symp$prop_symp_total
factor_hosp

# rescale to obtain a hospital probability
factor_hosp <- factor_hosp / factor_hosp[4] * 0.6
factor_hosp


