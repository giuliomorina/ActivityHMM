# function for plotting the time series of the data with the mean of the observation density from the most likely state
# data_analysis is the time series data with Time and Activity variables
# HMMpost is the posterior probabilities of each state at each time point. Needs a variable state which gives the most likely state at each time point.
# summaryfm is the summary of the fitted model. This should be a matrix with the means of the observation distributions for each state in the first column.

TimeSeriesStateMeansPlot <- function(data_analysis, HMMpost, summaryfm) {
ggplot(cbind(data_analysis,HMMpost),aes(x=Time,y=Activity,group=1)) + 
    geom_line(size=.3) + 
    geom_segment(data=cbind(data_analysis,HMMpost), 
                 aes(x=Time,xend=Time+330,y=summaryfm[state,1],yend=summaryfm[state,1],colour=as.factor(state)),size=5)
}

