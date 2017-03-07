rm(list=ls())
require(depmixS4)
require(ggplot2)
source("Functions.R")
source("TimeSeriesStateMeansPlot.R")

###############
# IMPORT DATA #
###############
#data_analysis <- read.csv("Data/Subject_1_(19).csv")
data_analysis <- read.csv("Data/Subject_2_(21).csv")
#data_analysis <- read.csv("Data/Subject_3_(23).csv")
data_analysis$Time <- as.POSIXct(data_analysis$Time, format = "00%y-%m-%d %H:%M:%S") #Specify that this is a date-time

##################
# TRANSFORM DATA #
##################
data_transformed <- data_analysis
data_transformed$Activity <- sqrt(data_transformed$Activity)

##############
# COMPARISON #
##############
#exploratoryAnalysis(data_analysis)
#exploratoryAnalysis(data_transformed)

#######
# HMM #
#######
mod <- depmix(response = Activity ~ 1, data=data_transformed, family=gaussian(), nstates = 3)
set.seed(13)
fm <- fit(mod)
summary(fm)
HMMpost <- posterior(fm)
summaryfm <- summary(fm)
plot_trans <- TimeSeriesStateMeansPlot(data_transformed,HMMpost,summaryfm) + ggtitle("HMM Transformed Data")

#######################
# HMM NOT TRANSFORMED #
#######################

mod_orig <- depmix(response = Activity ~ 1, data=data_analysis, family=gaussian(), nstates = 3)
set.seed(1729)
fm_orig <- fit(mod_orig)
summary(fm_orig)
HMMpost_orig <- posterior(fm_orig)
summaryfm_orig <- summary(fm_orig)
plot_orig <- TimeSeriesStateMeansPlot(data_analysis,HMMpost_orig,summaryfm_orig) + ggtitle("HMM Original Data")

#################
# ADD COVARIATE #
#################

#Model on original dataset (no transformation)
mod_covariate_original <- depmix(response = Activity ~ Temp, data=data_analysis, family=gaussian(), nstates = 3) 
set.seed(17)
fm_covariate_original <- fit(mod_covariate_original)
summary(fm_covariate_original)
HMMpost_covariate_original <- posterior(fm_covariate_original)
# Compute the mean of the values for every state
summaryfm_covariate_original <- matrix(c(mean(data_analysis$Activity[HMMpost_covariate_original$state==1]),
                      mean(data_analysis$Activity[HMMpost_covariate_original$state==2]),
                      mean(data_analysis$Activity[HMMpost_covariate_original$state==3])), ncol=1)
plot_orig_cov <- TimeSeriesStateMeansPlot(data_analysis,HMMpost_covariate_original,summaryfm_covariate_original) + 
  ggtitle("HMM Original Data + Covariate")

#Model on transformed dataset (sqrt)
mod_covariate_transf <- depmix(response = Activity ~ Temp, data=data_transformed, family=gaussian(), nstates = 3) 
set.seed(1991)
fm_covariate_transf <- fit(mod_covariate_transf)
summary(fm_covariate_transf)
HMMpost_covariate_transf <- posterior(fm_covariate_transf)
summaryfm_covariate_transf <- matrix(c(mean(data_transformed$Activity[HMMpost_covariate_transf$state==1]),
                      mean(data_transformed$Activity[HMMpost_covariate_transf$state==2]),
                      mean(data_transformed$Activity[HMMpost_covariate_transf$state==3])), ncol=1)
plot_transf_cov <- TimeSeriesStateMeansPlot(data_analysis = data_transformed,HMMpost_covariate_transf,summaryfm_covariate_transf) +
  ggtitle("HMM Transformed Data + Covariate")

########################
# USE TEMP AS RESPONSE #
########################

mod_resp_orig <- depmix(response = list(Activity ~ 1, Temp ~ 1), data=data_analysis, family=list(gaussian(),gaussian()), nstates = 3)
set.seed(1503)
fm_resp_orig <- fit(mod_resp_orig)
summary(fm_resp_orig)
HMMpost_resp_orig <- posterior(fm_resp_orig)
summaryfm_resp_orig <- summary(fm_resp_orig)
plot_resp_orig <- TimeSeriesStateMeansPlot(data_analysis,HMMpost_resp_orig,summaryfm_resp_orig) + ggtitle("HMM Original Data + Temp as response")

mod_resp_transf <- depmix(response = list(Activity ~ 1, Temp ~ 1), data=data_transformed, family=list(gaussian(),gaussian()), nstates = 3)
set.seed(1503)
fm_resp_transf <- fit(mod_resp_transf)
summary(fm_resp_transf)
HMMpost_resp_transf <- posterior(fm_resp_transf)
summaryfm_resp_transf <- summary(fm_resp_transf)
plot_resp_transf <- TimeSeriesStateMeansPlot(data_transformed,HMMpost_resp_transf,summaryfm_resp_transf) + ggtitle("HMM Transformed Data + Temp as response")



###############
# FANCY PLOTS #
###############

multiplot(plot_orig,plot_trans,plot_orig_cov, plot_transf_cov , plot_resp_orig, plot_resp_transf , cols=3)
