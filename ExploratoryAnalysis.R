rm(list=ls())
require(ggplot2)
require(reshape2)
###############
# IMPORT DATA #
###############
data_subj1 <- read.csv("Data/Subject_1_(19).csv")
data_subj2 <- read.csv("Data/Subject_2_(21).csv")
data_subj3 <- read.csv("Data/Subject_3_(23).csv")

##################
# CLEANING FOR R #
##################

data_subj1$Time <- as.POSIXct(data_subj1$Time, format = "00%y-%m-%d %H:%M:%S") #Specify that this is a date-time
data_subj2$Time <- as.POSIXct(data_subj2$Time, format = "00%y-%m-%d %H:%M:%S")
data_subj3$Time <- as.POSIXct(data_subj3$Time, format = "00%y-%m-%d %H:%M:%S")

########################
# EXPLORATORY ANALYSIS #
########################

exploratoryAnalysis <- function (data_analysis) {
  op <- par(ask=TRUE)
  print(summary(data_analysis))
  plot(data_analysis$Time, data_analysis$Activity, type="l")
  hist(data_analysis$Activity)
  #Create extra column that count the different days (24 hours)
  data_analysis$Hours24Count <- NA
  counter24 <- 0
  while(TRUE) {
    #Select observations in a 24 hours interval
    data_analysis$Hours24Count[data_analysis$Time >= min(data_analysis$Time)+counter24*60*60*24 & 
                                 data_analysis$Time < min(data_analysis$Time)+(counter24+1)*60*60*24] <- counter24
    counter24 <- counter24+1
    if(min(data_analysis$Time)+counter24*60*60*24 > max(data_analysis$Time)) { #There are no other observations to check
      break
    }
  }
  data_analysis$Hours24Count <- factor(data_analysis$Hours24Count)
  #Boxplot to check the distribution of the different days (to see if independendence sequences can be a sensible hypothesis)
  xlabs <- paste(levels(data_analysis$Hours24Count),"\n(N=",table(data_analysis$Hours24Count),")",sep="")
  p <- ggplot(data_analysis, aes(x=Hours24Count, y=Activity)) + geom_boxplot() + scale_x_discrete(labels=xlabs)
  print(p)
  
  #Plot ACF
  acf(data_analysis$Activity, lag.max = length(which(data_analysis$Hours24Count==0)),
      main=paste("24 hours ACF - Starting time: ",substr(data_analysis$Time[1], 12, 16), sep=""))
  #In thic case we can not assume stationarity in the classical sense, given the periodicty...so does doing the ACF make sense?!
  
  par(op) #Remove par(ask=TRUE)
}

exploratoryAnalysis(data_subj1)
exploratoryAnalysis(data_subj2)
exploratoryAnalysis(data_subj3)
