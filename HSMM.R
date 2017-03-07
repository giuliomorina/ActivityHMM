rm(list=ls())
require(mhsmm)
require(ggplot2)

set.seed(17)
BOOL_sqrt = T # Use a square root transformation for the data?
J <- 3 # Number of states

###############
# IMPORT DATA #
###############
data_analysis <- read.csv("Data/Subject_1_(19).csv")
data_analysis$Time <- as.POSIXct(data_analysis$Time, format = "00%y-%m-%d %H:%M:%S") #Specify that this is a date-time
# Use square root transformation:
if (BOOL_sqrt) data_analysis$Activity <- sqrt(data_analysis$Activity)

## First try a shifted Poisson distribution of the Sojourn time:

# Starting values for optimization:
# Note that optimization depends a lot on good starting values. For example, the sojourn time can collapse on 0.
init <- c(0, 0, 1)
P <- matrix(c(0, 0.1, 0.4, 0.5, 0, 0.6, 0.5, 0.9, 0), nrow = J) # Transition matrix for the states
# The parameters for the emission probabilities:
quantiles = quantile(data_analysis$Activity, (seq_len(J))/(J+1))
B <- list(mu = quantiles, sigma = c(.5, .5, 1)) 
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
start.pois <- hsmmspec(
  init = init,
  transition = P,
  parms.emis = B,
  sojourn = d,
  dens.emis = dnorm.hsmm)
# This seems to be a bad fit though at first start: see the means and variances of the model (note that sigma are variances, not stdevs!!)
M <- 300
h.poisson <- hsmmfit(train, start.pois, mstep = mstep.norm, M = M)
plot(h.poisson$loglik, type = "b", ylab = "Log-likelihood",
     xlab = "Iteration")
summary(h.poisson)
train$s <- h.poisson$yhat
plot.hsmm.data(train)





