####NEGTRACKER: Visualization and Analyses: RTs

setwd("~/Documents/Work/Publications/Writing/negtracker_revisions/materials")

#Run beginning of OCplots first, to get contingent.data df
#source('analysis/negtracker_OCplots.R')

#####Calculate RTs:

####Figure out RTs (time from distractor to target)

#trials that start on the distractor item, starting at the onset of the noun in each trial
distractor.onset <- contingent.data[contingent.data$contingent.type == "distractor",]

#A function that identifies the first look to target, and marks t.target at that point as the reaction time.
calculateRT <- function(d) {
	i <- min(which(d$on.target==1)) 
	d$RT<- d$t.target[i] * 1000
	return(d)
}

#data frame including a column for RTs
distractor.onset <- ddply(distractor.onset, ~subtrial, calculateRT)

#####PLOT RTs
data.exp1 <- distractor.onset[distractor.onset$condition == "Exp. 1",]
data.exp2 <- distractor.onset[distractor.onset$condition == "Exp. 2",]

##SET THESE VARIABLES TO MAKE RELEVANT GRAPH:
data <- data.exp1 #either data.nothing or data.something
title <- ""    

mss <- aggregate(RT ~ type + condition + agegroup + item + subid, data,mean) 

###Histograms:
#quartz()
qplot(RT, data=mss, geom="histogram", binwidth=100) + 
  facet_grid(type ~ agegroup) + 
  xlim(0, 3000) + ylab("Count") + 
  ggtitle(title) + 
  plot.style


#####
#rm(distractor.onset, mss)
