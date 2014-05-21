## NEGTRACKER: make long data
## ANN MCF
rm(list=ls())

## helper functions
to.n <- function(x) {as.numeric(as.character(x))}

## preliminaries 
library(reshape)
library(stringr)

setwd("~/Documents/Work/Publications/Writing/negtracker_revisions/materials")

# variables
x.max <- 1680 #this is the resolution
all.data <- data.frame()

#Load in demographics
demographics <- read.csv("demographics/negtracker_demographics.csv")
reject <- demographics[demographics$exclude != 0,]
demographics <- demographics[demographics$exclude == 0,]

#get list of files for data analysis
files <- 0
for (i in 1:length(demographics$subid)) {
  files[i] <- paste("negtracker",demographics$study.version[i],"_",demographics$subid[i],"-eye_data Samples.txt", sep="")
}

#Make longform dataframe
for (f in 1:length(files)) {
  print(files[f])
  ############ DATA CLEANING ###########
  #Load in data file (skip removes header rows)
  idf.data <- read.table(paste("data/",files[f],sep=""),sep="\t",header=TRUE,fill=TRUE, comment.char="", skip=40)
  names(idf.data) <- c("Time","Type","Trial","L.POR.X..px.","L.POR.Y..px.","R.POR.X..px.","R.POR.Y..px.","Frame","Aux1")
  
  ### split data into messages and data
  ###First get data:
  data <- subset(idf.data,idf.data$Type=="SMP")
  
  ## average monocular gaze data to get binocular vision
  data$"L.POR.X..px." <- to.n(data$"L.POR.X..px.")
  data$"R.POR.X..px." <- to.n(data$"R.POR.X..px.")
  data$"L.POR.Y..px." <- to.n(data$"L.POR.Y..px.")
  data$"R.POR.Y..px." <- to.n(data$"R.POR.Y..px.")
  data$x.pos <- rowMeans(data[,c("L.POR.X..px.","R.POR.X..px.")])
  data$y.pos <- rowMeans(data[,c("L.POR.Y..px.","R.POR.Y..px.")])
  
  #clean up data
  data <- data[,c("Time","x.pos")]
  
  ###Now get messages: 
  msgs <- subset(idf.data,idf.data$Type=="MSG")
  msgs <- msgs[,c("Time","L.POR.X..px.")]
  names(msgs) <- c("Time","Message")
  msgs$Message <- as.character(msgs$Message)
  msgs$trial <- gsub("# Message: ", "",msgs$Message)
  
  ## merge trial information back into data frame
  data$trial <- sapply(data$Time,function(x) {set <- msgs$trial[msgs$Time < x]
                                              set[length(set)]})
  
  ## drop the times before the first video
  data <- data[grep(".",data$trial,fixed=TRUE),]
  data$trial <- unlist(data$trial)
  
  ## set up some timing variables
  #Mark trial change
  data$stim.change <- c(diff(as.numeric(as.factor(data$trial))) != 0,0)
  #count time from start of trial to end of experiment
  data$t <- (data$Time - data$Time[1])/(1000000)
  
  #count time from beginning to end of each trial
  data$dt <- c(diff(data$t),0)
  t <- 0
  data$t.stim <- mapply(function (x,y) { 
    if(x==T) {
      t <<- 0
      return(t)
    } else {
      t <<- t + y
      return(t)
    }},data$stim.change,data$dt)
  
  #Find test trials only (no fillers, practice trials, etc.)
  data <- data[grepl("item",data$trial),]
  data <- data[!grepl("practice",data$trial),]
  data <- data[!grepl("pos",data$trial),] #feedback slide
  data <- data[!grepl("neg",data$trial),] #feedback slide
  
  #get trial number
  data$trial.num <- cumsum(data$stim.change)+1
  
  #Get info out of file name
  splits <- strsplit(files[f],"_")[[1]]
  data$subid <- paste(splits[3],str_sub(splits[4],start=1,end=2),sep="_")
  
  #get condition.  "nothing" is Exp 1 and "something" is Exp 2
  data$condition <- 
    if (splits[2] == "1" | splits[2] == "2") {
      data$condition <- "nothing"
    } else if (splits[2] == "3" | splits[2] == "4"){ 		
      data$condition <- "something"
    }
    
  #Label item
  data$item <- as.character(sapply(data$trial,function(x) {strsplit(x,"_")[[1]][1]}))
    
  ##Merge onsets: this gives onsets of multiple parts of the trial sentence
  if (splits[2] == "1" | splits[2] == "3") {
    onsets <- read.csv("annotations/timing_exp1.csv")
  } else if (splits[2] == "2" | splits[2] == "4"){ 		
    onsets <- read.csv("annotations/timing_exp2.csv")
  }

  data <- merge(data,onsets,sort=FALSE,all.x=T)

  #t.target centers timing around onset of the target noun
  data$t.target <- data$t.stim - data$noun_onset
  
  #Use sentence type and item side to determine what side the target character was on
  data$left.side <- grepl("itemL",data$trial) #what side of the screen was the character with target items on?
  data$target.side <- mapply(function (x,y) { 
    if(x=="positive" & y==T) {
      t <<- "left"
      return(t)
    } else if (x=="positive" & y==F) {
      t <<- "right"
      return(t)
    } else if (x=="negative" & y==T) {
      t <<- "right"
      return(t)
    } else if (x=="negative" & y==F) {
      t <<- "left"
      return(t)
    }},data$type,data$left.side)
  
  ##clean up x position data
  data$x.pos[data$x.pos < 1 | data$x.pos > x.max] <- NA
  
  #Identify whether gaze was on target side 
  data$target.looks <- data$x.pos
  data$target.looks[data$target.side == "left"] <- x.max - data$target.looks[data$target.side == "left"]
  data$on.target <- data$target.looks > (x.max / 2) + 200 
  
  ## clean up data frame
  data <- data[,c("subid","condition","item","trial.num","type","t.target","x.pos","on.target")]

  all.data <- rbind.fill(all.data,data)
}

all.data$condition <- as.factor(all.data$condition)

#### MERGE IN AGES
ages <- demographics[,c("subid","agegroup")]

# exclude ages outside range of interest
ages <- ages[ages$agegroup == "2" | ages$agegroup == "3" | ages$agegroup == "4" | ages$agegroup == "adult",] 
all.data <- merge(all.data,ages,by.x="subid",by.y = "subid",all.x=F,all.y=F)
all.data$agegroup <- factor(all.data$agegroup, levels=c("2","3","4","adult"), labels = c("2-3","3-4","4-5","Adult"))

##Write this to a data file
#write.csv(all.data, "~/Documents/Work/Publications/Writing/negtracker_revisions/materials/data")

