# Plot showing the probabilities of being in each state over time.
# data_analysis is the time series data with Time and Activity variables.
# HMMpost has posterior probabilities of each state at each time point in columns 2, 3 and 4.
# green colour is state 1, red is state 2 and blue is state 3 (didn't include a key as the ordering of states isn't fixed)

StateProbsPlot <- function(data_analysis, HMMpost){
  x <- matrix(, nrow = dim(HMMpost)[1], ncol = dim(HMMpost)[2] - 1)
  x[, 1] <- HMMpost[, 2]
  x[, 2] <- x[, 1] + HMMpost[, 3]
  x[, 3] <- x[, 2] + HMMpost[, 4]
  x <- as.data.frame(x)
  pl <- ggplot(cbind(data_analysis, x), aes(Time))
  pl <- pl + geom_ribbon(aes(ymin=0, ymax=V1), fill = "lightgreen") + geom_line(aes(y=V1))
  pl <- pl + geom_ribbon(aes(ymin=V1, ymax=V2), fill = "lightsalmon") + geom_line(aes(y=V2))
  pl <- pl + geom_ribbon(aes(ymin=V2, ymax=V3), fill = "lightblue")
  pl
}