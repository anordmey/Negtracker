####NEGTRACKER: Visualization and Analyses: PRELIMS
rm(list=ls())

## helper functions
to.n <- function(x) {as.numeric(as.character(x))}

#for bootstrapping 95% confidence intervals
theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
  mean(x) - quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
  quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975) - mean(x)}

#Libraries
library(reshape)
library(lattice)
library(ggplot2)
library(plotrix)
library(reshape)
library(stringr)
library(bootstrap)
library(lme4)

#Load data (run negtracker_exclusionCriteria.R first to remove subjects & trials based on rejection criteria)
setwd("~/Documents/Work/Publications/Writing/negtracker_revisions/materials")
all.data <- read.csv("data/negtracker.longdata.cleaned.csv")

####PRELIMS FOR ANALYSIS AND VISUALIZATION
##PLOT STYLE:
theme_set(theme_bw())
plot.style <- theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	 panel.border = element_blank(), axis.line = element_line(colour="black",size=.5),
	 axis.ticks = element_line(size=.5),legend.justification=c(1,0),
	 legend.position=c(.25,.75),legend.title=element_blank(),
	 axis.title.x = element_text(vjust=-.5),
	 axis.title.y = element_text(angle=90,vjust=0.25))
	 
##Formatting
all.data$type <- factor(all.data$type, levels=c("positive","negative"), labels=c("Positive","Negative"))
all.data$condition <- factor(all.data$condition,levels=c("nothing","something"),labels=c("Exp. 1","Exp. 2"))

##SET UP TIMING BINS:
bin.size <- 10
all.data$t.target.binned <- round(all.data$t.target * bin.size)/bin.size * 1000

#trim binned data 
all.data <- all.data[all.data$t.target.binned < 5100,]

##subject ns
ns <- aggregate(subid ~ agegroup + condition,all.data,function(x) {length(unique(x))})
