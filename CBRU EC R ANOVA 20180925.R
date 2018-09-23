#20180923
#Piia Turunen

#R DEMONSTRATION FOR CBRU EXCHANGE CLUB on 25.9.2018
#PART 3: ANOVA

##WORK IN PROGRESS, MISSING MOST CONTENT##

#setting the working directory so that R knows where to get files from and where to save them
setwd("C:/Users/Hiekka/Documents/Psykologia/Väikkäri/Data/CBRUEC")

#activating modules with functions that I have previously installed
library(psych)
library(plyr); library(ggplot2); library(GGally)

#In part 1 we simulated data for our analyses and saved it. 
#We have already examined the data in Part 2 so we will only take a cursory look here

ReadingData <- read.csv("SimulationDataVars.txt", header = TRUE, sep = ";")
View(ReadingData) #open up the data viewer
str(ReadingData)
describe(ReadingData[,2:5])

#will we need this for something later? let's see
ReadingData$sexM<-as.factor(ReadingData$sexM)
#ReadingData$PP<-as.numeric(ReadingData$PP)
#ReadingData$RAN<-as.numeric(ReadingData$RAN)
#ReadingData$Reading<-as.numeric(ReadingData$Reading)

#Let's pretend we have a well justified reason to group our cases into categories of
#phonological processing (PP) and rapid naming (RAN) skills. In real life, we could have 
#real discrete categories for ANOVA but let's use the data that we produced for this simulation.

#We need to edit our variables to get the groups formed. Please note that I'm not condoning the logic
#behind what we're doing next and this is simply for demonstration purposes.

#I was going to use -1sd performance on PP and RAN as cutoffs and create groups of 
#poor performance on both, on one of them, and on neither. This produced too few observations/cell
#so I had to revise my cutoffs.

#PPcutoff<-mean(ReadingData$PP)-sd(ReadingData$PP) #code I would have used
#RANcutoff<-mean(ReadingData$RAN)-sd(ReadingData$RAN)

#let's split down the middle this time
PPcutoff<-mean(ReadingData$PP)
RANcutoff<-mean(ReadingData$RAN)

cbind(PPcutoff, RANcutoff)

ReadingData$PPRANcategories<-ifelse(ReadingData$PP<PPcutoff&ReadingData$RAN<RANcutoff, "BOTH", 
                                    ifelse(ReadingData$PP<PPcutoff&ReadingData$RAN>RANcutoff, "PP", 
                                    ifelse(ReadingData$PP>PPcutoff&ReadingData$RAN<RANcutoff, "RAN", "NEITHER")))

count(ReadingData$PPRANcategories)
ReadingData[1:15,]

#visual summary of the data
p <- ggpairs(ReadingData[2:6], mapping = aes(), lower = list(combo = wrap("facethist", bins = 20)))  
p

ReadingData$PPRANcategories<-as.factor(ReadingData$PPRANcategories)

#zooming in on the categorization and reading skills
ggplot(ReadingData, aes(PPRANcategories, Reading)) + geom_boxplot()

#####
#ANOVA
#This section heavily leans on a book by Field, Miles, & Field (2012): Discovering statistics using R, chapter 10 on ANOVA.
#Any errors that remain are mine, and their views are not necessarily reflected in what follows.

