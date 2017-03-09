set.seed(17)
rm(list=ls())
require(mhsmm)
require(ggplot2)
source("TimeSeriesStateMeansPlot.R")

BOOL_sqrt = T # Use a square root transformation for the data?
J <- 3 # Number of states; Needs to be adjusted still to automatically alter the transition matrix
subject_nr <- 1

###############
# IMPORT DATA #
###############
subject <- c("1_(19)", "2_(21)", "3_(23)")[subject_nr]
data_analysis <- read.csv(paste("Data/Subject_", subject, ".csv", sep=""))
data_analysis$Time <- as.POSIXct(data_analysis$Time, format = "00%y-%m-%d %H:%M:%S") #Specify that this is a date-time
# Use square root transformation:
if (BOOL_sqrt) data_analysis$Activity <- sqrt(data_analysis$Activity)

## First try a shifted Poisson distribution of the Sojourn time:

# Starting values for optimization:
# Note that optimization depends a lot on good starting values. For example, the sojourn time can collapse on 0.
init <- c(0, 0, 1)
P <- matrix(c(0, 0.1, 0.4, 0.5, 0, 0.6, 0.5, 0.9, 0), nrow = J) # Transition matrix for the states
# The parameters for the emission probabilities (hand tuned at the moment):
quantiles = quantile(data_analysis$Activity, (seq_len(J))/(J+1))
mu_mat = matrix(c(.5,4.2,5.5,1,7,11,3,5,11), nrow = J, byrow = T)
sigma_mat = matrix(c(1.5,1.5,1.5,1,1.5,1.5,1,1.5,1.5), nrow = J, byrow = T)
B <- list(mu = mu_mat[subject_nr,], sigma = sigma_mat[subject_nr,]) 
d <- list(lambda = c(40, 10, 10), shift = c(2, 2, 2), type = "poisson") # Inititial values for the Sojourn distribution, a shifted Poisson distribution

# Simulate an example model, but then store the data in the same format, overwriting the simulation:
model <- hsmmspec(init, P, parms.emis = B, sojourn = d,
                  dens.emis = dnorm.hsmm)
train <- simulate(model, nsim = 100, seed = 123456, # Simulate the model.
                  rand.emis = rnorm.hsmm)
train$N <- length(data_analysis$Activity)
train$x <- data_analysis$Activity
train$s <- rep(1, train$N)

# Create an object with the starting values:
# TODO: do automatic grid search...
start.pois <- hsmmspec(
  init = init,
  transition = P,
  parms.emis = B,
  sojourn = d,
  dens.emis = dnorm.hsmm)
# Note that sigma are variances, not stdevs!!
h.poisson <- hsmmfit(train, start.pois, mstep = mstep.norm)
plot(h.poisson$loglik, type = "b", ylab = "Log-likelihood",
     xlab = "Iteration")
# The estimated parameters:
summary(h.poisson)
# the loglikelihood:
print("loglikelihood:")
h.poisson$loglik

# Plot the most likely states:
#train$s <- h.poisson$yhat
HMMpost <- data.frame(state=h.poisson$yhat)
TimeSeriesStateMeansPlot(data_analysis, HMMpost, as.matrix(h.poisson$model$parms.emission$mu, nrow = 3)) +
  scale_color_manual(labels = c("State 1", "State 2","State 3"), values = c("red", "darkgreen","yellow"), name="") +
  theme(axis.title=element_text(size=20), axis.text = element_text(size=15))
# Save the plot:
pdf(file=paste("Images/activity_states_",gsub(" ", "_", subject_nr),".pdf",sep=""), width=15, height=14)
TimeSeriesStateMeansPlot(data_analysis, HMMpost, as.matrix(h.poisson$model$parms.emission$mu, nrow = 3)) +
  scale_color_manual(labels = c("State 1", "State 2","State 3"), values = c("red", "darkgreen","yellow"), name="") 
dev.off()

