#MJ Logistic Regression
library(MASS)
library(ISLR)
library(popbio)

#Creates dataset from the MJ stats csv
dataset <- read.csv("C:\\Users\\JOsDP23\\OneDrive\\Documents\\GitHub\\Math4322\\MJDataSci\\michael-jordan-nba-career-regular-season-stats-by-game.csv", header = TRUE)
#Creates the subset for the variable we will be using
dataset = subset(dataset,select = -c(EndYear,Rk,G,Date,Years,Days,Tm,Home,Opp))
#Array to hold our seed values
i <- c(1,2,3,4,5,6,7,8,9,10)
#Sets Win as a catagorical variable
dataset$Win <- factor(dataset$Win)
#Sets GS as catagorical
#dataset$GS <- factor(dataset$GS)
#Fills any NA values as Zeroes
dataset[is.na(dataset)]<-0
attach(dataset)
avg = 0
#For loop used to generate 10 different tests
for (val in i)
{
  set.seed(val)
  #Used to train model on the 80% training data and 20% testing data
  MJ.sample.train <- sample(1:dim(dataset)[1],dim(dataset)[1]*.8,rep=FALSE)
  MJ.train <- dataset[MJ.sample.train, ]
  MJ.test <- dataset[-MJ.sample.train, ]
  #generates our logistic regression
  MJ.log <- glm(Win ~ Age + GS + MP + FG_PCT + TP_PCT + FT_PCT + ORB + DRB + AST + STL + BLK + TOV + PF + PTS + GmSc, data = MJ.train, family = "binomial")
  #Preforms our prediction to see the variables most associated if MJ stats are the reason they win games
  MJ.train.predict<-predict(MJ.log,MJ.test,type="response")
  MJ.log.predict<-rep(0,length(MJ.train.predict))
  #prints iteration of for loop
  print(val)
  #prints summary of logistic model
  print(summary(MJ.log))
  #prints The test prediction error
  print(mean(MJ.log.predict != MJ.test$Win))
  avg = avg + mean(MJ.log.predict != MJ.test$Win)
  logi.hist.plot(MJ.test$Age,MJ.test$Win,boxp=FALSE,type="dit",col="gray", xlabel = "Age")
  logi.hist.plot(MJ.test$GS,MJ.test$Win,boxp=FALSE,type="dit",col="gray", xlabel = "Started Game")
  logi.hist.plot(MJ.test$MP,MJ.test$Win,boxp=FALSE,type="dit",col="gray", xlabel = "Minutes Played")
  logi.hist.plot(MJ.test$PTS,MJ.test$Win,boxp=FALSE,type="dit",col="gray", xlabel = "Points Scored")
  logi.hist.plot(MJ.test$GmSc,MJ.test$Win,boxp=FALSE,type="dit",col="gray", xlabel = "Game Score")
}
#prints average error across all runs
print(avg/10)