#20180922
#Piia Turunen

#R DEMONSTRATION FOR CBRU EXCHANGE CLUB on 25.9.2018
#PART 2: DESCRIPTIVES

#Please note that checking assumptions is not explicitly included in this at the moment

#setting the working directory so that R knows where to get files from and where to save them
setwd("C:/Users/Hiekka/Documents/Psykologia/Väikkäri/Data/CBRUEC")

#activating modules with functions that I have previously installed
library(psych); library(plyr); library(summarytools); library(ggplot2)
library(car); library(QuantPsyc); library(GGally)

#In part 1 we simulated data for our analyses and saved it. Here we read that data th

ReadingData <- read.csv("SimulationDataVarsWithMissingness.txt", header = TRUE, sep = ";")
View(ReadingData) #open up the data viewer
str(ReadingData)
describe(ReadingData[,2:5])

view(dfSummary(ReadingData))
view(dfSummary(subset(ReadingData, subset = ReadingData$sexM==0))) #if you only want to look at females

#maybe we're interested in whether males and females differ in their reading skills? Let's first look at summary data
describeBy(ReadingData[,2:4], ReadingData$sexM, mat = FALSE) #describe our reading-related variables sorted by sexM

#will we need this for something later? let's see
ReadingData$sexM<-as.factor(ReadingData$sexM)
#ReadingData$PP<-as.numeric(ReadingData$PP)
#ReadingData$RAN<-as.numeric(ReadingData$RAN)
#ReadingData$Reading<-as.numeric(ReadingData$Reading)

#visual summary of data
p <- ggpairs(ReadingData[2:5], mapping = aes(col = sexM, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))  
p

#looking at the correlations
CorrelationMatrix<-cor(ReadingData[,2:4], method="spearman", use = "complete.obs")
print(CorrelationMatrix, digits = 2)

#visualizing the relationships between variables
ggplot(ReadingData, aes(PP, RAN, colour=sexM)) + geom_point() + geom_smooth(method=lm)
ggplot(ReadingData, aes(PP, Reading, colour=sexM)) + geom_point() + geom_smooth(method=lm)
ggplot(ReadingData, aes(Reading, RAN, colour=sexM)) + geom_point() + geom_smooth(method=lm)

#####
#REGRESSION
#This section heavily leans on a book by Field, Miles, & Field (2012): Discovering statistics using R, chapter 7.
#Any errors that remain are mine, and their views are not necessarily reflected in what follows.

#I'm removing the outliers now to make running the analyses easier. Feel free to replace them with something sensible instead
ReadingData<-na.omit(ReadingData) #na.omit leaves out observations with missing values
str(ReadingData)

#Creating a regression model
model_Reading_1 <- lm(Reading ~ PP + RAN + sexM, data=ReadingData) #linear model, reading is predicted by PP, RAN, sexM
summary(model_Reading_1)

#another model
model_Reading_2 <- lm(Reading ~ sexM, data=ReadingData)
summary(model_Reading_2)

#comparing models that are nested
anova(model_Reading_1, model_Reading_2)

#whatever get's chosen, put it into the chosen model so you don't need to change around code
#later based on the model name
model_Reading<-model_Reading_1 #CHANGE TO 2 IF THAT IS CHOSEN

#now that we've chosen our model, I'll save the n and the number of predictors k
n<-length(ReadingData$id)
k<-3 #change to whatever it is. Constant is not counted into this now

#checking outliers & influential cases - choose model based on anova (small, original)
ReadingData$residuals<-resid(model_Reading)
ReadingData$standardized_residuals<-rstandard(model_Reading)
ReadingData$studentized_residuals<-rstudent(model_Reading)
ReadingData$cooks_distance<-cooks.distance(model_Reading)
ReadingData$dfbeta<-dfbeta(model_Reading)
ReadingData$dffits<-dffits(model_Reading)
ReadingData$leverage<-hatvalues(model_Reading)
ReadingData$covariance.ratios<-covratio(model_Reading)

ReadingData[,6:13]<-NULL
names(ReadingData)
write.table(ReadingData, "Reading_Data_with_diagnostics.txt", sep = ";")

#checking if there are more large residuals than there should be
#with standardized residuals, there should be ~5% scores that are <-1.96 or >1.96
#there should be ~1% scores that are <-2.58 or <2.58 and .1% <-3.29 or >3.29
ReadingData$st_resid_95<-ReadingData$standardized_residuals < -1.96 | ReadingData$standardized_residuals > 1.96
ReadingData$st_resid_99<-ReadingData$standardized_residuals < -2.58 | ReadingData$standardized_residuals > 2.58
ReadingData$st_resid_99.9<-ReadingData$standardized_residuals < -3.29 | ReadingData$standardized_residuals > 3.29

#let's see if there are more large residuals than expected
#this code will probably not work so let's make it work and then copy to other models
proportion_table<-cbind(y<-c(95, 99, 99.9), x<-c((sum(ReadingData$st_resid_95/n)),(sum(ReadingData$st_resid_99/n)),(sum(ReadingData$st_resid_99.9/n))))
print(proportion_table, digits=3)
#let's look at cook's distances
ReadingData$cook_over_1<-ReadingData$cooks_distance >1
sum(ReadingData$cook_over_1)

#let's look at leverages; average leverage should be (k+1)/n and we're looking for values double/triple this
ReadingData$large_leverage<-ReadingData$leverage > ((k+1)/n) #fill in value
sum(ReadingData$large_leverage)

#let's calculate the covariance ratio acceptable limits
UL<- 1+(3*(k+1)/n) #the upper limit
LL<- 1-(3*(k+1)/n) #the lower limit
#how many times is this limit broken?
sum(ReadingData$covariance.ratios>UL)
sum(ReadingData$covariance.ratios<LL)

#checking for independence of errors
dwt(model_Reading)

#checking for multicollinearity
vif(model_Reading) #VIF
mean(vif(model_Reading))
1/vif(model_Reading) #tolerance

#checking residuals by plotting

#first we need to add the fitted/predicted values from our model to our data
ReadingData$fitted_values<-model_Reading$fitted.values
head(cbind(ReadingData$Reading, ReadingData$fitted_values)) #with head we see the values of the first observations

#histogram of the studentized residuals vs a normal distribution with the same mean and sd as the residuals
stdResHist<-ggplot(ReadingData, aes(studentized_residuals)) + geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x="Studentized Residuals", y="Density")
stdResHist + stat_function(fun=dnorm, args=list(mean=mean(ReadingData$studentized_residuals),sd=sd(ReadingData$studentized_residuals)),colour="green", size=1)

#q-q plot
stdResQQ<-qplot(sample=ReadingData$studentized_residuals, stat="qq") + labs(x="Theoretical values", y="Observed values")
stdResQQ

stdResScatter<-ggplot(ReadingData, aes(fitted_values, studentized_residuals)) 
stdResScatter + geom_point() + geom_smooth(method="lm", colour="green") + labs(x="Fitted values", y="Studentized residual")
