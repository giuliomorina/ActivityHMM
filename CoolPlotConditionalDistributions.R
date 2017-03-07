cond_probs_dfrm(list=ls())
require(depmixS4)
require(ggplot2)
require(reshape2)

###############
# IMPORT DATA #
###############
data_analysis <- read.csv("Data/Subject_1_(19).csv")
data_analysis$Time <- as.POSIXct(data_analysis$Time, format = "00%y-%m-%d %H:%M:%S") #Specify that this is a date-time

#######
# HMM #
#######

nStates <- 3
mod <- depmix(response = Activity ~ 1, data=data_analysis, family=gaussian(), nstates = nStates) #Specify the model (default starting parameters -> check other choices so that the EM convergence is more likely)
set.seed(13)
fm <- fit(mod)

###########################
# CONDITIONAL DISTRIBUTION
###########################

forward_backward_vars <- forwardbackward(fm)
alpha <- forward_backward_vars$alpha
beta <- forward_backward_vars$beta
fm_params <- getpars(fm)
gamma <- matrix(fm_params[(nStates+1):(nStates*nStates+nStates)], nrow=nStates, ncol=nStates, byrow = TRUE) #Transition matrix (the same as t(fm@trDens))
gaussian_coeff <- matrix(fm_params[(nStates*nStates+nStates+1):length(fm_params)], ncol=2, byrow = TRUE)

x_seq <- seq(-10,60,by=0.1)
cond_probs <- matrix(NA, nrow=nrow(data_analysis), ncol=length(x_seq))
for(t in 2:nrow(data_analysis)) {
  for (x in x_seq) {
    cond_probs[t,x_seq %in% x] <- alpha[(t-1),]%*%gamma%*%diag(dnorm(x, mean = gaussian_coeff[,1], sd = gaussian_coeff[,2]))%*%as.matrix(beta[t,])
  }
  cond_probs[t,] <- cond_probs[t,]/sum(cond_probs[t,])
}

cond_probs_df <- melt(cond_probs)

ggplot(cond_probs[175,]) + geom_density()

plot(x_seq,cond_probs[15,],type="l",ylab='',xlab='',main='')
abline(v=data_analysis$Activity[15], col="red")

par(mfrow=c(4,4))
for(t in seq(2,288,length.out=16)) {
  plot(x_seq,cond_probs[t,],type="l",ylab='',xlab='',main=paste(data_analysis$Time[t]))
  abline(v=data_analysis$Activity[t], col="red")
}

