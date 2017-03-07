rm(list=ls())
require(ggplot2)
require(reshape2)
source("Functions.R")
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

exploratoryAnalysis(data_subj1)
exploratoryAnalysis(data_subj2)
exploratoryAnalysis(data_subj3)
