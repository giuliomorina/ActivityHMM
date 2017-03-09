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

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


relabelStatus <- function(summaryfm_orig, HMMpost_orig) {
  relabel <- sort(summaryfm_orig[,1], index.return=TRUE)
  summaryfm_orig2 <- summaryfm_orig[relabel$ix,]
  HMMpost_orig2 <- HMMpost_orig[,c(1,(relabel$ix+1))]
  for(i in 1:nrow(HMMpost_orig2)) {
    HMMpost_orig2[i,1] <- which.max(HMMpost_orig2[i,2:(nrow(summaryfm_orig)+1)])
  }
  colnames(HMMpost_orig2) <- colnames(HMMpost_orig)
  return(list(summaryfm_orig2,HMMpost_orig2))
}