rm(list=ls())
require(depmixS4)
require(ggplot2)

###############
# IMPORT DATA #
###############
data_analysis <- read.csv("Data/Subject_1_(19).csv")
data_analysis$Time <- as.POSIXct(data_analysis$Time, format = "00%y-%m-%d %H:%M:%S") #Specify that this is a date-time

#######
# HMM #
#######

#Just to understand how the package works, fit a mixture of Gaussian with 3 hidden states
mod <- depmix(response = Activity ~ 1, data=data_analysis, family=gaussian(), nstates = 3) #Specify the model (default starting parameters -> check other choices so that the EM convergence is more likely)
# Note that the package allows to specify different distributions (e.g. Gamma), as well as covariates.
# It can also deals with multivariate variables, so that we woyld try using as Response both Activity and Temperature
set.seed(13)
fm <- fit(mod)
fm
summary(fm)
HMMpost <- posterior(fm)
head(HMMpost) #Posterior probabilities of being in each state
#Plot the timeseries with different color for each group
ggplot(cbind(data_analysis,HMMpost),aes(x=Time,y=Activity,group=1,colour=as.factor(state))) + geom_line()
