#MJ Decision Tree
library(MASS)
library(ISLR)
library(tree)
#Creates dataset from the MJ stats csv
dataset <- read.csv("C:\\Users\\whenpigsfly050\\Desktop\\Math4322\\michael-jordan-nba-career-regular-season-stats-by-game.csv", header = TRUE)
#Creates the subset for the variable we will be using
dataset = subset(dataset,select = -c(EndYear,Rk,G,Date,Years,Days,Tm,Home,Opp,Diff))
#Array to hold our seed values
i <- c(1,2,3,4,5,6,7,8,9,10)
#Sets Win as a catagorical variable
dataset$Win <- factor(dataset$Win)
#Sets GS as catagorical
dataset$GS <- factor(dataset$GS)
#Fills any NA values as Zeroes
dataset[is.na(dataset)]<-0

#For loop used to generate 10 different tests
avg = 0
for (val in i)
{
  set.seed(val)
  
  #Used to train model on the 80% training data and 20% testing data
  MJ.sample.train <- sample(1:dim(dataset)[1],dim(dataset)[1]*.8,rep=FALSE)
  MJ.train <- dataset[MJ.sample.train, ]
  MJ.test <- dataset[-MJ.sample.train, ]
  
  #generates our decision tree
  MJ.tree <- tree(Win ~ Age + GS + MP + FG_PCT + TP_PCT + FT_PCT + ORB + DRB + AST + STL + BLK + TOV + PF + PTS + GmSc, data = MJ.train)
  
  #below comments used for pruning commented out to remove excessive output
  #cv.MJ = cv.tree(MJ.tree,FUN = prune.misclass)
  #par(mfrow = c(1,2))
  #plot(cv.MJ$size,cv.MJ$dev,type = "b")
  #plot(cv.MJ$k,cv.MJ$dev,type = "b")
  
  #prunes the tree
  MJ.prune <- prune.tree(MJ.tree,best=5)
  
  #prints the tree
  plot(MJ.prune)
  text(MJ.prune)
  #Performs our prediction
  MJ.train.predict<-predict(MJ.prune,newdata=MJ.test,type="class")
  
  #prints iteration of for loop
  print(val)
  
  #prints summary of decision tree model
  print(summary(MJ.prune))
  
  #prints The test prediction error
  print(mean(MJ.train.predict != MJ.test$Win))
  avg = avg + mean(MJ.train.predict != MJ.test$Win)
}

#prints average error across all runs
print(avg/10)