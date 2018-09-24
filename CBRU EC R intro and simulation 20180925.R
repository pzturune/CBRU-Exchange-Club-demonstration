#20180919
#Piia Turunen

#R DEMONSTRATION FOR CBRU EXCHANGE CLUB on 25.9.2018
#PART 1: GETTING FAMILIAR WITH R + DATA SIMULATION

#### COMMENTS AND EXECUTING COMMANDS

#when you put "#" before your text, anything on the same row following it is a comment
#when you execute that row, comments won't be executed unless you have chosen them specifically and not "#"
#to execute a row when a cursor is on it (anywhere on it), press ctrl+enter 
#you can first press ctrl and then while still holding it down, press enter
#if your cursor in on a row where there is nothing executable, R will just continue down one row
#at a time until it finds something to execute
#if you want to execute a part of something, paint it with a mouse or by ctrl+shift+arrows (might vary in OS's?) 
#like you would with a text document

#example:
#3+5
3+5 #+6+12

#### PREPARING FOR A SESSION

#setting the working directory so that R knows where to get files from and where to save them
#you need to change this to reflect wherever you want to keep your files
setwd("C:/Users/Hiekka/Documents/Psykologia/Väikkäri/Data/CBRUEC")

#R is the programming language that we use here. There are some basic functions that are included
#in the base version of R.
#However, for anything more complex, you will need to add modules that are called packages.
#These packages include the mathematical formulas etc that you use when you do statistical analyses.
#You could just program the code yourself: tell R how to calculate something.
#But that's often unnecessary when you can just use the code that exists.

#Here's how I install a package that I need but didn't have before. You only install once
install.packages("simstudy")  
#btw " and ' are basically interchangeable so no need to worry about which to use

#you can also just make installing a conditional part of your code for those instances 
#where someone else might use it or you will, on a different computer and do not necessarily have it
#if you don't have a package, this will install it for you, but if you do, it doesn't install it again
if(!require(psych)){install.packages("psych")}
#this also activates the package whether it also installed it or not so it wouldn't actually need
#to be on the activating list three rows down but I'll keep it there for easier use as it does no harm

#activating modules with functions that I have previously installed
library(psych); library(simstudy); library(plyr); library(pwr)

#### DATA SIMULATION

#I'm simulating the data that we will use, and saving it for later 
#so the results that we get can be reproduced

#I'll simulate based on an imaginary correlation matrix to make the data "natural"-like

#We're going to create four variables: phonological processing (PP), rapid naming skills (RAN), 
#reading accuracy (Reading), and (male) sex (sexM).
#We're going to pretend the first three are in standard points, and only full points 
#I'm taking inspiration to the correlation matrix we create here from the matrix in our real study

#creating a correlation matrix CMatrix in four rows (for our four variables)
#I'm naming it CMatrix. Could have been something else as well, for example "My_thing_X".
#Informative names are preferred as well as annotating code so you or someone who later 
#works on the code can understand what was done
CMatrix<-matrix(c(1, -0.2, 0.4, -0.1, -0.2, 1, -0.1, -0.05, 0.4, -0.1, 1, -0.15, -0.1, -0.05, -0.15, 1), nrow = 4)
# <- means you assign the thing on the right side to the entity on the left. You "create" the entity
#matrix and c are functions that you give arguments (specifications) to
#c makes a vector and matrix makes a matrix. Here we make basically combine values to make a "list" 
#and then feed that to the matrix and tell it there are four rows so it can make the vector into a matrix
CMatrix #executing this row prints the created matrix onto the console

#We will be generating data with these arguments: n=300, variable means are 10, 10, 50, 0.5,
#var sd:s are 3, 3, 10, 0.5, and we're using the correlation matrix that we just created to
#shape the relationships between the variables

#please note that when this is run, it creates a different data set each time which means that any results
#you get at one time point will differ from the rerun unless you save your creations and read them on 
#later use instead of generating new data
vars<-genCorData(300, mu = c(10, 10, 50, 0.5), sigma = c(3, 3, 10, 0.1), corMatrix = CMatrix)
#again, assigning to var the product of the function genCorData with the specifications fed to it
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
varsMiss$missingnessIndicatorPP<-rbinom(300,1,0.97) #3% probability for each observation to be assigned missing
varsMiss$missingnessIndicatorRAN<-rbinom(300,1,0.97) 
varsMiss$missingnessIndicatorReading<-rbinom(300,1,0.95) #5% probability for each observation to be assigned missing

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

write.table(vars, "SimulationDataVars.txt", sep = ";", row.names=FALSE)
write.table(varsMiss, "SimulationDataVarsWithMissingness.txt", sep = ";", row.names=FALSE)
varsMiss <- read.csv("SimulationDataVarsWithMissingness.txt", header = TRUE, sep = ";")
describe(varsMiss)

#####
#POWER

#if we would have been planning a study and collecting real data, 
#we would have wanted to make some power calculations

#CORRELATIONS
pwr.r.test(n=NULL, r=.10, power=.80, sig.level=.05)
#you can set any of the arguments to null and specify the rest to get the NULL one calculated
#if you want to easily export a single value, for example the n, do this:
pwr.r.test(n=NULL, r=.10, power=.80, sig.level=.05)$n

#MULTIPLE REGRESSION
pwr.f2.test(u=3, v=300-(3+1), f2=.15, power=NULL)
#u=number of predictors, v=(n-(u+1)), f2=Cohen's effect size f2 = r2 / (1  - r2)
#and interpreted as 0.02=small, 0.15=medium, 0.35=large
pwr.f2.test(u=1, v=NULL, f2=.15, power=.85)

#ANOVA
pwr.anova.test(k=2,n=NULL,f=.25, power=.9)
#again, play around. K=number of groups, n=observations/cell, f=Cohen's effect size f
#f values are interpreted as 0.1=small, 0.25=medium, 0.4=large

#if you want to know more about a package or about a function, you can do this:
??pwr
?pwr
?pwr.anova.test
