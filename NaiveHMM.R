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

##############
# LIKELIHOOD #
##############

#This is just a test to check how the likelihood can be computed (and if it is computed correctly)
#Note that depmixS4 already provides the Likelihood as well as the AIC, BIC, forward variables and backward variables (forwardbackqard function)
forwardbackward(fm)$logLike

#Get parameters of the fitted HMM
fm_params <- getpars(fm)
nStates <- 3
delta <- matrix(fm_params[1:nStates], nrow=1) #Initial distribution (the same as fm@init)
gamma <- matrix(fm_params[(nStates+1):(nStates*nStates+nStates)], nrow=nStates, ncol=nStates, byrow = TRUE) #Transition matrix (the same as t(fm@trDens))
gaussian_coeff <- matrix(fm_params[(nStates*nStates+nStates+1):length(fm_params)], ncol=2, byrow = TRUE) #mean and sd of Gaussian distribution for each state 

# alpha <- matrix(NA, nrow=nrow(data_analysis), ncol=3) #Initialise vectorof forward probabilities
# alpha[1,] <- delta%*%diag(dnorm(data_analysis$Activity[1], mean = gaussian_coeff[,1], sd = gaussian_coeff[,2]))
# for (t in 2:nrow(data_analysis)) {
#   alpha[t,] <- alpha[(t-1),]%*%gamma%*%diag(dnorm(data_analysis$Activity[t], mean = gaussian_coeff[,1], sd = gaussian_coeff[,2]))
# }
#Note that there are underflow problems!

#Based on page 47
phi <- matrix(NA, nrow=nrow(data_analysis)+1, ncol=3) #Initialise vectorof forward probabilities
phi[1,] <- delta
logLikelihood <- 0
for (t in 2:nrow(data_analysis)) {
  v <- phi[(t-1),]%*%gamma%*%diag(dnorm(data_analysis$Activity[(t-1)], mean = gaussian_coeff[,1], sd = gaussian_coeff[,2]))
  u <- sum(v)
  logLikelihood <- logLikelihood+log(u)
  phi[t,] <- v/u
}
print(logLikelihood)
#It is a little bit different that the one returned by the package:
forwardbackward(fm)$logLike

#Maybe this is due to different roundings OR in the package stationarity is not supposed!
#Let's try and recompute the loglikelihood without supposing stationarity (pag. 47)
phi <- matrix(NA, nrow=nrow(data_analysis), ncol=3) #Initialise vectorof forward probabilities
w1 <- sum(delta%*%diag(dnorm(data_analysis$Activity[(1)], mean = gaussian_coeff[,1], sd = gaussian_coeff[,2])))
phi[1,] <- (delta%*%diag(dnorm(data_analysis$Activity[(1)], mean = gaussian_coeff[,1], sd = gaussian_coeff[,2])))/w1
logLikelihood2 <- log(w1)
for (t in 2:nrow(data_analysis)) {
  v <- phi[(t-1),]%*%gamma%*%diag(dnorm(data_analysis$Activity[t], mean = gaussian_coeff[,1], sd = gaussian_coeff[,2]))
  u <- sum(v)
  logLikelihood2 <- logLikelihood2+log(u)
  phi[t,] <- v/u
}
print(logLikelihood2)
#Ok this is equal to the one returned by the package :D Great!
forwardbackward(fm)$logLike
