rm(list=ls())
require(depmixS4)
############################
# IMPORT DATA AND CLEANING #
############################
data_subj1 <- read.csv("Data/Subject_1_(19).csv")
data_subj2 <- read.csv("Data/Subject_2_(21).csv")
data_subj3 <- read.csv("Data/Subject_3_(23).csv")
data_subj1$Time <- as.POSIXct(data_subj1$Time, format = "00%y-%m-%d %H:%M:%S") #Specify that this is a date-time
data_subj2$Time <- as.POSIXct(data_subj2$Time, format = "00%y-%m-%d %H:%M:%S")
data_subj3$Time <- as.POSIXct(data_subj3$Time, format = "00%y-%m-%d %H:%M:%S")

#####################
# UTILITY FUNCTIONS #
#####################
compute_loglikelihood <- function(fm,data_analysis,nStates,distribution) {
  if(distribution != "gaussian") {
    stop("Sorry...Only gaussian implemented so far :(")
  }
  fm_params <- getpars(fm)
  delta <- matrix(fm_params[1:nStates], nrow=1) #Initial distribution (the same as fm@init)
  gamma <- matrix(fm_params[(nStates+1):(nStates*nStates+nStates)], nrow=nStates, ncol=nStates, byrow = TRUE) #Transition matrix (the same as t(fm@trDens))
  gaussian_coeff <- matrix(fm_params[(nStates*nStates+nStates+1):length(fm_params)], ncol=2, byrow = TRUE)
  phi <- matrix(NA, nrow=nrow(data_analysis), ncol=nStates) #Initialise vectorof forward probabilities
  w1 <- sum(delta%*%diag(dnorm(data_analysis$Activity[(1)], mean = gaussian_coeff[,1], sd = gaussian_coeff[,2])))
  phi[1,] <- (delta%*%diag(dnorm(data_analysis$Activity[(1)], mean = gaussian_coeff[,1], sd = gaussian_coeff[,2])))/w1
  logLikelihood2 <- log(w1)
  for (t in 2:nrow(data_analysis)) {
    v <- phi[(t-1),]%*%gamma%*%diag(dnorm(data_analysis$Activity[t], mean = gaussian_coeff[,1], sd = gaussian_coeff[,2]))
    u <- sum(v)
    logLikelihood2 <- logLikelihood2+log(u)
    phi[t,] <- v/u
  }
  return(logLikelihood2)
}

possible_distributions <- list(gaussian()) #Possible distribution considered. Must be an argument of family
possible_num_states <- 2:10 #Possible number of states
set.seed(17)

data_analysis <- data_subj3
#data_analysis$Activity <- log(2+data_analysis$Activity) #Transformation

#Splitting in training and validation set 
#The validation set is long at least 1 day (max 2 days) and it is the "last part" of the observed time series
day_length <- as.numeric(max(data_analysis$Time) - min(data_analysis$Time)) #how many days the whole time series is long
training_length <- (floor(day_length)-1)*60*60*24 #How many seconds the training set length is 
training_set <- data_analysis[data_analysis$Time < min(data_analysis$Time)+training_length,]
validation_set <- tail(data_analysis,-nrow(training_set)) #Remaining part of the dataset

#Prepare where to save results
results <- matrix(NA, nrow = length(possible_distributions)*length(possible_num_states), ncol=8) #This is where the results will be stored
colnames(results) <- c("Distribution","Number of states","Loglikelihood Training","AIC Training","BIC Training", "Loglikelihood Validation",
                       "AIC Validation", "BIC Validation")
counter <- 1

#Compute everything :D
for (distribution in possible_distributions) {
  for(num_states in possible_num_states) {
    #Specify the model
    mod <- depmix(response = Activity ~ 1, data=training_set, family=distribution, nstates = num_states)
    #Fit the model
    fm <- fit(mod, verbose = FALSE) 
    
    #Compute loglikelihood,AIC,BIC on training set
    loglikelihood_training <- compute_loglikelihood(fm,training_set,num_states,as.character(distribution)[[1]])
    p <- freepars(fm)
    n_training <- nrow(training_set)
    AIC_training <- 2*p-2*loglikelihood_training
    BIC_training <- log(n_training)*p-2*loglikelihood_training
    
    #Compute loglikelihood,AIC,BIC on validation set
    loglikelihood_validation <- compute_loglikelihood(fm,validation_set,num_states,as.character(distribution)[[1]])
    n_validation <- nrow(validation_set)
    AIC_validation <- 2*p-2*loglikelihood_validation
    BIC_validation <- log(n_validation)*p-2*loglikelihood_validation
    
    #Save results
    results[counter,1] <- as.character(distribution)[[1]]
    results[counter,2:8] <- c(num_states,loglikelihood_training,AIC_training,BIC_training,loglikelihood_validation,AIC_validation,BIC_validation)
    counter <- counter +1
  }
}
