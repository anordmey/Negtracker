####NEGTRACKER: Visualization and Analyses: SIMPLE PLOTS

setwd("~/Documents/Work/Publications/Writing/negtracker_revisions/materials")

#Run negtracker_analysisprelims.R first
source('analysis/negtracker_analysisPrelims.R')

#####SIMPLE PLOTS:  Looks to target over time

##split all.data into something and nothing groups
data.exp1 <- all.data[all.data$condition == "Exp. 1",]
data.exp2 <- all.data[all.data$condition == "Exp. 2",]

##SET UP TIMING INFO:
timing1 <- read.csv("annotations/timing_exp1.csv")
timing2 <- read.csv("annotations/timing_exp2.csv")
timing <- rbind(timing1,timing2)
timing$start <- timing$sentence_onset - timing$noun_onset
timing$person <- timing$person_onset - timing$noun_onset
timing$has <- timing$has_onset - timing$noun_onset
timing$no <- timing$no_onset - timing$noun_onset
timing$end <- timing$sentence_offset - timing$noun_onset
timing$tag <- timing$tag_onset - timing$noun_onset

start.times <- aggregate(start ~ type, timing, mean)
pos.start <- start.times[start.times$type=='positive',2]*1000
neg.start <- start.times[start.times$type=='negative',2]*1000

person.times <- aggregate(person ~ type, timing, mean)
pos.person <- person.times[person.times$type=='positive',2]*1000
neg.person <- person.times[person.times$type=='negative',2]*1000

has.times <- aggregate(has ~ type, timing, mean)
pos.has <- has.times[has.times$type=='positive',2]*1000
neg.has <- has.times[has.times$type=='negative',2]*1000

no.times <- aggregate(no ~ type, timing, mean)
neg.no <- no.times[no.times$type=='negative',2]*1000

end.times <- aggregate(end ~ type, timing, mean)
pos.end <- end.times[end.times$type=='positive',2]*1000
neg.end <- end.times[end.times$type=='negative',2]*1000

tag.times <- aggregate(tag ~ type, timing, mean)
pos.tag <- tag.times[tag.times$type=='positive',2]*1000
neg.tag <- tag.times[tag.times$type=='negative',2]*1000

pos.timing <- as.data.frame(matrix(c(pos.start,pos.person,pos.has,0,pos.person,pos.has,0,pos.end,"#A43946","#EB97A1","#A43946","#EB97A1","Look at the","boy who","has","X"),nrow=4, ncol=4))
names(pos.timing) <- c("start","end","colors","sentences")
pos.timing$start <- as.numeric(as.character(pos.timing$start))
pos.timing$end <- as.numeric(as.character(pos.timing$end))
pos.timing$colors <- as.character(pos.timing$colors)
pos.timing$sentences <- as.character(pos.timing$sentences)

neg.timing <- as.data.frame(matrix(c(neg.start,neg.person,neg.has,neg.no,0,neg.person,neg.has,neg.no,0,neg.end,"#123159","#628DC5","#123159","#628DC5","#123159","Look at the","boy who","has","no","X"),nrow=5, ncol=4))
names(neg.timing) <- c("start","end","colors","sentences")
neg.timing$start <- as.numeric(as.character(neg.timing$start))
neg.timing$end <- as.numeric(as.character(neg.timing$end))
neg.timing$colors <- as.character(neg.timing$colors)
neg.timing$sentences <- as.character(neg.timing$sentences)



##SET THESE VARIABLES TO MAKE RELEVANT GRAPH:
data <- data.exp2 #either data.nothing or data.something
title <- ""    


##RUN THIS TO MAKE GRAPH
mss <- aggregate(on.target ~ t.target.binned + type + subid + agegroup, data,mean) 
ms <- aggregate(on.target ~ t.target.binned + type + agegroup, mss, mean)
ms$cih <- aggregate(on.target ~ t.target.binned + type + agegroup, mss, ci.high)$on.target
ms$cil <- aggregate(on.target ~ t.target.binned + type + agegroup, mss, ci.low)$on.target

#quartz()
ggplot(data=ms, aes(x=t.target.binned, y=on.target, color=type)) + 
	geom_line() + 
	#scale_size_discrete(range=c(.3,1)) +
	scale_color_manual(values=c("#EB97A1","#123159")) +
	geom_smooth(aes(ymin=ms$on.target-ms$cil,ymax=ms$on.target+ms$cih), stat="identity",lty=0) + 
	facet_wrap(~ agegroup, ncol=2) + 
  ylim(0,1) + ylab("Proportion Target Looks") + 
	scale_x_continuous(breaks=seq(-2000, 2800, 500), name="Time (ms)") +
	coord_cartesian(xlim=c(-2000,3000)) + 
	geom_hline(yintercept=.5,lty=2, size=.25) + 
	#sentence timing for positive sentences
	geom_segment(data=pos.timing, aes(x=start, xend=end, y=1, yend=1), size=4, colour=pos.timing$colors) + 
	#annotate("text",x=pos.timing$start,y=.95,label=pos.timing$sentences,size=1.5,pos=4,adj=0,color="#A43946") +
	#sentence timing for negative sentences
	geom_segment(data=neg.timing, aes(x=start, xend=end, y=0, yend=0), size=4, colour=neg.timing$colors) + 
	#annotate("text",x=neg.timing$start,y=.05,label=neg.timing$sentences,size=1.5,pos=4,adj=0,color="#123159") +
	geom_vline(xintercept=0,lty=3) + 
	ggtitle(title) + 
	plot.style
	
#####
rm(data.nothing, data.something, mss, ms, data, timing1, timing2, timing, pos.timing, neg.timing)