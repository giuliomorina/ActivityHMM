rm(list=ls())
require(depmixS4)
require(ggplot2)
require(reshape2)
source("Functions.R")

###############
# IMPORT DATA #
###############
data_analysis <- read.csv("Data/Subject_3_(23).csv")
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

x_seq <- seq(-10,max(data_analysis$Activity)+10,by=0.1)
cond_probs <- matrix(NA, nrow=nrow(data_analysis), ncol=length(x_seq))
for(t in 2:nrow(data_analysis)) {
  for (x in x_seq) {
    cond_probs[t,x_seq %in% x] <- alpha[(t-1),]%*%gamma%*%diag(dnorm(x, mean = gaussian_coeff[,1], sd = gaussian_coeff[,2]))%*%as.matrix(beta[t,])
  }
  cond_probs[t,] <- cond_probs[t,]/sum(cond_probs[t,])
}

cond_probs_df <- melt(cond_probs)
colnames(cond_probs_df) <- c("Time","X","Value")
cond_probs_df$X <- x_seq[cond_probs_df$X]
#cond_probs_df$Time <- data_analysis$Time[cond_probs_df$Time]
#cond_probs_df$Time <- as.POSIXct(cond_probs_df$Time, format = "00%y-%m-%d %H:%M:%S") #Specify that this is a date-time
# 
# ggplot(cond_probs_df[cond_probs_df$Time==5,], aes(x=x_seq,y=Value)) + 
#   geom_area(colour="black", fill="blue", alpha=.2) +
#   geom_vline(aes(xintercept=data_analysis$Activity[5]), color="red", linetype="solid", size=1) +
#   #scale_color_manual(name = "statistics", values = c(Obs.Value = "red")) +
#   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
#   annotate("text", y= max(cond_probs_df[cond_probs_df$Time==5,"Value"]), x =max(x_seq),
#            label=substr(data_analysis$Time[5], 12, 16),hjust=1) 


seq_t <- floor(seq(2,288,by=12))
allplots <- list()
counter <- 1
for(t in seq_t) {
  allplots[[counter]]  <- ggplot(cond_probs_df[cond_probs_df$Time==t,], aes(x=x_seq,y=Value)) + 
    geom_area(colour="black", fill="blue", alpha=.2) +
    geom_vline(data=data.frame(valore = data_analysis$Activity[t]),aes(xintercept=as.numeric(valore)), color="red", linetype="solid", size=1) +
    #geom_vline(aes(xintercept=as.numeric(data_analysis$Activity[t])), color="red", linetype="solid", size=1) +
    #scale_color_manual(name = "statistics", values = c(Obs.Value = "red")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    annotate("text", y= max(cond_probs_df[cond_probs_df$Time==t,"Value"]), x =max(x_seq),
             label=substr(data_analysis$Time[t], 12, 16),hjust=1)
  counter <- counter+1
}

pdf(file=paste("Images/conditional_distributions_subject_3.pdf",sep=""), width=15, height=9)
multiplot(plotlist = allplots, cols=4)
dev.off()
