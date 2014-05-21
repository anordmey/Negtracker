## NEGTRACKER: Reject subjects & trials
## ANN 
rm(list=ls())

#Libraries
library(reshape)

#Load data
setwd("~/Documents/Work/Publications/Writing/negtracker_revisions/materials")
all.data <- read.csv("data/negtracker.longdata.csv")

#### REMOVE SUBJECTS AND TRIALS
all.data$count.na <- is.na(all.data$x.pos)

#remove subjects who complete fewer than 8 trials
subject.items <- ddply(all.data,c("subid","item"),"nrow")
subject.itemsums <- ddply(subject.items, c("subid"), "nrow")
reject.total <- subject.itemsums[subject.itemsums$nrow<8,]
for (i in reject.total$subid) {
	all.data<-all.data[all.data$subid !=i,]
}

#reject subjects who have NAs for over 30% of their samples
subject.nas <- ddply(all.data,c("subid","count.na"),"nrow")
subject.nas<- reshape(subject.nas, timevar="count.na", idvar="subid", direction="wide")
names(subject.nas) <- c("subid","false","true")
subject.nas$prop <- subject.nas$false / (subject.nas$true + subject.nas$false)

reject.subjects <- subject.nas[subject.nas$prop < .7,]
for(i in reject.subjects$subid) {
	all.data <- all.data[all.data$subid !=i,]
}

#remove trials with NAs for over 30% of samples
trial.nas <- ddply(all.data,c("subid","trial.num","count.na"),"nrow")
trial.nas<- reshape(trial.nas, timevar="count.na", idvar=c("subid","trial.num"), direction="wide")
names(trial.nas) <- c("subid","trial.num","false","true")
trial.nas[is.na(trial.nas)] <- 0
trial.nas$prop <- trial.nas$false / (trial.nas$true + trial.nas$false)

reject.trials <- trial.nas[trial.nas$prop < .7,]
all.data$subtrial <- paste(all.data$subid,"_",all.data$trial.num,sep="")
reject.trials$subtrial <- paste(reject.trials$subid,"_",reject.trials$trial.num,sep="")

for(i in reject.trials$subtrial) {
	all.data <- all.data[all.data$subtrial !=i,]
}

##Write to csv
#write.csv(all.data, "data/negtracker.longdata.cleaned.csv")