#MJ Logistic Regression
library(MASS)
library(ISLR)
#Creates dataset from the MJ stats csv
dataset <- read.csv("C:\\Users\\JOsDP23\\OneDrive\\Documents\\GitHub\\Math4322\\MJDataSci\\michael-jordan-nba-career-regular-season-stats-by-game.csv", header = TRUE)
#Creates the subset for the variable we will be using
dataset = subset(dataset,select = -c(EndYear,Rk,G,Date,Years,Days,Tm,Home,Opp))
#Array to hold our seed values
i <- c(1,2,3,4,5,6,7,8,9,10)
#Sets Win as a catagorical variable
dataset$Win <- factor(dataset$Win)
#Sets GS as catagorical
dataset$GS <- factor(dataset$GS)
#Fills any NA values as Zeroes
dataset[is.na(dataset)]<-0

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
  MJ.log <- glm(Win ~ Age + GS + MP + FG_PCT + TP_PCT + FT_PCT + ORB + DRB + AST + STL + BLK + TOV + PF + PTS + GmSc, data = MJ.train, family = "binomial", maxit = 100)
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
  
}
#pairs(MJ.train)
#prints average error across all runs
print(avg/10)

#attach(dataset)
par(mfrow=c(2,3))
boxplot(Age ~ Win, data = dataset, main = "Age vs Win")
boxplot(Diff~ Win, data = dataset, main = "Point Diff vs Win")
#boxplot(GS ~ Win, data = dataset, main = "Game Start vs Win") 
boxplot(MP ~ Win, data = dataset, main = "Minutes Played vs Win") 
boxplot(FG_PCT ~ Win, data = dataset, main = "Field Goal Percentage vs Win")
boxplot(TP_PCT ~ Win, data = dataset, main = "TP_PCT vs Win")
boxplot(FT_PCT ~ Win, data = dataset, main = "FT_PCT vs Win")
boxplot(ORB ~ Win, data = dataset, main = "Off. Rebound vs Win")
boxplot(DRB ~ Win, data = dataset, main = "Def. Rebound vs Win")
boxplot(AST ~ Win, data = dataset, main = "Assists vs Win")
boxplot(STL ~ Win, data = dataset, main = "Steals vs Win")
boxplot(BLK ~ Win, data = dataset, main = "Blocks vs Win")
boxplot(TOV ~ Win, data = dataset, main = "TurnOvers vs Win")
boxplot(PF ~ Win, data = dataset, main = "Personal Fouls vs Win")
boxplot(PTS ~ Win, data = dataset, main = "Points Scored vs Win")
boxplot(GmSc ~ Win, data = dataset, main = "Game Score vs Win")

 
  