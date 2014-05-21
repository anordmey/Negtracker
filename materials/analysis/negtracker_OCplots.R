####NEGTRACKER: Visualization and Analyses: ONSET-CONTINGENT PLOTS

setwd("~/Documents/Work/Publications/Writing/negtracker_revisions/materials")

#Run negtracker_analysisprelims.R first
source('analysis/negtracker_analysisPrelims.R')
#####ONSET-CONTINGENT PLOTS:

#start at the onset of the noun in each trial
onset.data <- all.data[all.data$t.target >= 0,]

#add a vector indicating the start of the onset of the noun in each trial
onset.data <- arrange(onset.data, subid, trial.num, t.target)
trial.change <- c(diff(onset.data$trial.num) != 0,0)
trial.change <- c(1, trial.change)
trial.change <- trial.change[1:nrow(onset.data)]
onset.data$trial.change <- trial.change

#a function to identify whether started on target or distractor, and to flip for target initial.
onset.contingent <- function(d) {
	if(!is.na(d$on.target[d$trial.change==1])) {
		#check whether this is T initial or D initial
		if(d$on.target[d$trial.change==1]==TRUE) {
			#if it's T initial, flip the "on target" index so T is zero
			d$flip.target <- 1 - d$on.target
			d$contingent.type <- "target"
		} else if (d$on.target[d$trial.change==1]==FALSE) {
			d$flip.target <- d$on.target
			d$contingent.type <- "distractor"
		}
	} else {
		d <- c()
	}
	return(d)
}

#now use that function on each subject/trial combo (subtrial):
contingent.data <- ddply(onset.data, ~subtrial, onset.contingent)
contingent.data$contingent.type <- as.factor(contingent.data$contingent.type)


#######MAKE PLOT
exp1.data <- contingent.data[contingent.data$condition=="Exp. 1",]
exp2.data <- contingent.data[contingent.data$condition=="Exp. 2",]

##Define variables: 
data <- exp2.data    #change to exp1.data or exp2.data
title <- ""  #Write title here

#Make plot:
mss <- aggregate(flip.target ~ t.target.binned + agegroup + contingent.type + type + subid, data ,mean) 
ms <- aggregate(flip.target ~ t.target.binned + agegroup + type + contingent.type, mss, mean)
ms$cih <- aggregate(flip.target ~ t.target.binned + agegroup + type + contingent.type, mss, ci.high)$flip.target
ms$cil <- aggregate(flip.target ~ t.target.binned + agegroup + type + contingent.type, mss, ci.low)$flip.target


#quartz()
ggplot(data=ms, aes(x=t.target.binned, y=flip.target, color=type, linetype=contingent.type)) + 
	geom_line() + 
  scale_color_manual(values=c("#EB97A1","#123159")) +
	facet_wrap(~ agegroup, ncol=2) +  
	#geom_smooth(aes(ymin=ms$flip.target-ms$cil,ymax=ms$flip.target+ms$cih),stat="identity", lty=0) + 
	ylim(0,1) + ylab("Proportion Fixation") + 
	scale_x_continuous(breaks=seq(0, 3000, 400), name="Time (ms)") +
	coord_cartesian(xlim=c(0,3000)) +
	geom_hline(yintercept=.5,lty=2, size=.25) + 
	ggtitle(title) + 
	plot.style