#20180919
#Piia Turunen

#R DEMONSTRATION FOR CBRU EXCHANGE CLUB on 25.9.2018
#PART 1: GETTING FAMILIAR WITH R + DATA SIMULATION

#when you put "#" before your text, anything on the same for following it is a comment
#when you execute that row, comments won't be executed unless you have chosen them specifically and not "#"

#example:
#3+5
3+5 #+6+12


#setting the working directory so that R knows where to get files from and where to save them
setwd("C:/Users/Hiekka/Documents/Psykologia/Väikkäri/Data/linear20180730")

#reading imaginary data from a file in working directory
data_x<-read.csv("data_x_version1.1.txt", header = TRUE, sep = ";")

#Installing a package/module that I didn't have before
install.packages("simstudy")

#activating modules with functions that I have previously installed
library(psych); library(simstudy); library(plyr)

#I'm simulating data that we will use, and saving it for later use so the results that we get can
#be reproduced

#I'll simulate based on an imaginary correlation matrix to make the data "natural"-like

#We're going to create three variables: phonological processing (PP), rapid naming skills (RAN), reading accuracy (Reading), and male sex (sexM).
#We're going to pretend these are standard points and only full points 
#I'm taking inspiration to the correlation matrix from the matrix in our stury

#creating a correlation matrix CMatrix in four rows (four variables)
CMatrix<-matrix(c(1, -0.2, 0.4, -0.1, -0.2, 1, -0.1, -0.05, 0.4, -0.1, 1, -0.15, -0.1, -0.05, -0.15, 1), nrow = 4)
CMatrix #executing this row prints the created matrix onto the console

#generating data with these arguments: n=600, var means are 10, 10, 50, 0.5 and var sd:s are 3,3,10,0.5,
#and we're using the correlation matrix that we just created
#please note that when this is run, it creates a different data set each time which means that any results
#you get at one time point will differ from the rerun unless you save your creations and read them on later tries
#instead of generating new data
vars<-genCorData(300, mu = c(10, 10, 50, 0.5), sigma = c(3, 3, 10, 0.1), corMatrix = CMatrix)
vars

vars<-round(vars, digits=0)  #rounding to whole numbers
vars

#when using this genCorData function, the data that is created automatically has variable names: id, V1, V2...
vars[, round(cor(cbind(V1, V2, V3, V4)), 2)]
#we're checking a correlation matrix for the three variables, rounded to 2 decimal points

vars[, round(sqrt(diag(var(cbind(V1, V2, V3, V4)))), 2)]
#checking sd's

#renaming the variables to meaningful names
names(vars)<-c("id", "PP", "RAN", "Reading", "sexM")

summary(vars)

describe(vars) #describe the data set: here we see also id being described, but this is not very useful information
describe(vars[,2:5]) #out of vars, selecting rows (no selection) and columns (from 2 to 5)

#this is what selecting rows would look like: we're seeing the first 10 observations here
vars[1:10,]

#this could be saved to it's own data set:
small_sample<-vars[1:10,]
small_sample #we see it's here now

#to make the data more realistic, let's add missingness
#I'm going to add it in a style where it's missing completely at random to make things easier for this demonstration
#first I'm making a new dataset so we don't destroy the old one!

varsMiss<-vars
varsMiss$missingnessIndicatorPP<-rbinom(300,1,0.97) #3% of data missing
varsMiss$missingnessIndicatorRAN<-rbinom(300,1,0.97) #3% of data missing
varsMiss$missingnessIndicatorReading<-rbinom(300,1,0.95) #5% of data missing

varsMiss$missingnessIndicatorPP
count(varsMiss[,6]) #count data from the PP missingness indicator

varsMiss$PP<-ifelse(varsMiss$missingnessIndicatorPP==1, vars$PP, NA) #if missingnessIndicator is 1 for the observation, the value is the PP value, otherwise it is NA
varsMiss$PP
varsMiss$RAN<-ifelse(varsMiss$missingnessIndicatorRAN==1, vars$RAN, NA)
varsMiss$Reading<-ifelse(varsMiss$missingnessIndicatorReading==1, vars$Reading, NA)

summary(varsMiss)
#let's remove the missingness indicators now that they are not needed
varsMiss[,6:8]<-NULL

#Now that we have the data we are going to use, let's save it so we can come back to it later!
#Otherwise it is lost when we rerun the simulation, as this writes over the old data
#I'm saving the full data as well so that it's possible to see how missingness affects results (if that is in the scope of what you want to do)

write.table(vars, "SimulationDataVars.txt", sep = ";")
write.table(varsMiss, "SimulationDataVarsWithMissingness.txt", sep = ";")
varsMiss <- read.csv("SimulationDataVarsWithMissingness.txt", header = TRUE, sep = ";")
describe(varsMiss)

