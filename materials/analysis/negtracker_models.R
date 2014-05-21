####NEGTRACKER: Visualization and Analyses: MODELS

setwd("~/Documents/Work/Publications/Writing/negtracker_revisions/materials")

#Run negtracker_analysisprelims.R first
source('analysis/negtracker_analysisPrelims.R')

#Libraries
library(languageR)

##trim data: only 300-2300 ms
trimmed.data <- all.data[all.data$t.target.binned >= 300 & all.data$t.target.binned < 2300,]
trimmed.data$period <- "early"
trimmed.data[trimmed.data$t.target.binned >= 1300,]$period <- "late"
trimmed.data$period <- factor(trimmed.data$period)

##Experiment 1: Nothing
##simple model
#aggregate data across subjects and items
exp1 <- aggregate(on.target ~ type + agegroup + item + subid, subset(trimmed.data, condition=="Nothing"), mean)
exp1$subid <- factor(exp1$subid)

model.exp1 <- lmer(on.target ~ agegroup*type + 
	(type | subid) + 
	(agegroup | item),
	data=exp1)
	#note that (agegroup+type|item does not converge)

#Experiment 1: compare groups
model.exp1.twos <- lmer(on.target ~ type + 
                            (type|subid) + 
                            (type|item), 
                          data=subset(exp1, agegroup=="2-year-olds"))

model.exp1.threes <- lmer(on.target ~ type + 
                       (type|subid) + 
                       (type|item), 
                     data=subset(exp1, agegroup=="3-year-olds"))

model.exp1.fours <- lmer(on.target ~ type + 
                      (type|subid) + 
                      (type|item), 
                    data=subset(exp1, agegroup=="4-year-olds"))

model.exp1.adults <- lmer(on.target ~ type + 
                       (type|subid) + 
                       (type|item), 
                     data=subset(exp1, agegroup=="Adults"))


#are neg sentences different from chance?
exp1.subs <- aggregate(on.target ~ type + agegroup + subid, exp1, mean)

t.test(subset(exp1.subs, subset=agegroup=="2-year-olds"&type=="Negative")$on.target, mu=.5)
t.test(subset(exp1.subs, subset=agegroup=="3-year-olds"&type=="Negative")$on.target, mu=.5)
t.test(subset(exp1.subs, subset=agegroup=="4-year-olds"&type=="Negative")$on.target, mu=.5)
t.test(subset(exp1.subs, subset=agegroup=="Adults"&type=="Negative")$on.target, mu=.5)




##Experiment 2: Something
##Consider early v. late windows
exp2.windows <- aggregate(on.target ~ type + agegroup + item + period + subid, subset(trimmed.data, condition=="Something"), mean)

model.exp2.windows <- lmer(on.target ~ agegroup*type*period + 
	(type + period | subid) + 
	(type + agegroup + period| item),
	data=exp2.windows)




################
##Both experiments
both <- aggregate(on.target ~ type + agegroup + item + condition + period + subid, trimmed.data, mean)
  
model.both <- lmer(on.target ~ agegroup*type*period*condition + 
	(type + period | subid) + 
	(type + period + agegroup| item), 
	data=both)


#Both experiments: compare groups

model.both.twos <- lmer(on.target ~ type*period*condition + 
                     (type + period | subid) + 
                     (type + period | item), 
                   data=subset(both, subset=agegroup=="2-year-olds"))

model.both.threes <- lmer(on.target ~ type*period*condition + 
                          (type + period | subid) + 
                          (type + period | item), 
                        data=subset(both, subset=agegroup=="3-year-olds"))

model.both.fours <- lmer(on.target ~ type*period*condition + 
                          (type + period | subid) + 
                          (type + period | item), 
                        data=subset(both, subset=agegroup=="4-year-olds"))

model.both.adults <- lmer(on.target ~ type*period*condition + 
                          (type + period | subid) + 
                          (type + period | item), 
                        data=subset(both, subset=agegroup=="Adults"))




#################################
#Look at 600-2600 ms window (post-hoc)
trimmed.data <- all.data[all.data$t.target.binned >= 600 & all.data$t.target.binned < 2600,]
trimmed.data$period <- "early"
trimmed.data[trimmed.data$t.target.binned >= 1600,]$period <- "late"
trimmed.data$period <- factor(trimmed.data$period)

both <- aggregate(on.target ~ type + agegroup + item + condition + period + subid, trimmed.data, mean)

model.both.threes.window2 <- lmer(on.target ~ type*period*condition + 
                            (type + period | subid) + 
                            (period | item), 
                          data=subset(both, subset=agegroup=="3-year-olds"))

