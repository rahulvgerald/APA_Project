setwd("E:/TAMU Course Work/Applied Predictive Analysis/Course Project")  
rm(list=ls())
library(ISLR)
library(class)
library(alr3)
library(dplyr)
library(caret)
library(MASS)
library(leaps)

## Read the season statistics, all star list, player data and mvp list csv file into R
seasonStats = read.csv("Seasons_Stats.csv")
allStars = read.csv("NBA All Star Games.csv")
playersData = read.csv("players.csv")
mvp = read.csv("MVP.csv")
salary = read.csv("NBA Salaries.csv")
attach(seasonStats)
attach(allStars)
attach(playersData)
attach(mvp)
attach(salary)
##remove the "*" symbol in a few player names
seasonStats$Player = gsub("\\*","",seasonStats$Player)
allStars$Player = gsub("\\*","",allStars$Player)
playersData$Player = gsub("\\*","",playersData$Player)
mvp$Player = gsub("\\*","",mvp$Player)
salary$Player = gsub("\\*","",salary$Player)

## Subset the data with only data from year 2000
seasonStats = subset(seasonStats,Year >= 2000)

## Use only the columns whoose values are to be averaged
seasonStatsMean = seasonStats[,c(2:5,11:21,23:26,28:30,33,36,39,40,43)]
seasonStatsMean[is.na(seasonStatsMean)] = 0

## Use only the columns whoose values are to be summed
seasonStatsSum = seasonStats[,c(2:4,7:9,31:32,34:35,37:38,41:42,44:53)]
seasonStatsSum[is.na(seasonStatsSum)] = 0

## aggregate the mean values for a player, since one player played for more than one team
seasonStatsMean = aggregate(.~Year+Player+Pos, seasonStatsMean, mean)

## aggregate the sum values for a player, since one player played for more than one team
seasonStatsSum = aggregate(.~Year+Player+Pos, seasonStatsSum, sum)

## merger them both to get complete statistics
seasonStats = merge(seasonStatsMean,seasonStatsSum)

allStars = allStars[,c(1,2,11)]
playersData = playersData[,c(2:8)]

salary = subset(salary, salary$season_end >= 2000)
salary = subset(salary, salary$season_end <=2017)
salary = salary[,c(1,2,3)]
names(salary)[3] = "Year"

## merge stats with player data such as height, weight etc
seasonStats = merge(seasonStats,playersData)

## merge with all star list to get all the allstar players
seasonStats = merge(seasonStats,allStars,all = TRUE, fill = FALSE)

## merge with mvp list to get all mvp's
seasonStats = merge(seasonStats,mvp,all = TRUE, fill = FALSE)

## merge with salary to get salary of players
seasonStats = merge(seasonStats,salary,all = TRUE, fill = FALSE)

## if they are not an allstar or mvp replace NA with False
seasonStats[, 55:56][is.na(seasonStats[, 55:56])] = 0

## Remove all rows with NA values
seasonStats = na.omit(seasonStats)


########## LOgistic Regression All Star ##########

########## Training 2000, testing on 2001 ########### 

train2000 = subset(seasonStats, Year == 2000)
test2001 = subset(seasonStats, Year == 2001)

train2000 = train2000[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

test2001 = test2001[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2000.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2000,family=binomial)
summary(train2000.results)

probs<-predict(train2000.results,train2000,type="response")
pred2001<-rep("0",nrow(train2000))
pred2001[probs>0.5]<-"1"
table(train2000$AllStar,pred2001)
mean(pred2001==train2000$AllStar)

########## Training 2001, testing on 2002 ##########

train2001 = subset(seasonStats, Year == 2001)
test2002 = subset(seasonStats, Year == 2002)

train2001 = train2001[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

regfit.full=regsubsets(AllStar~.,data = train2001)
summary(regfit.full)

test2002 = test2002[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2001.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2001,family=binomial)
summary(train2001.results)

probs<-predict(train2001.results,train2001,type="response")
pred2002<-rep("0",nrow(train2001))
pred2002[probs>0.5]<-"1"
table(train2001$AllStar,pred2002)
mean(pred2002==train2001$AllStar)

########## Training 2002, testing on 2003 ##########

train2002 = subset(seasonStats, Year == 2002)
test2003 = subset(seasonStats, Year == 2003)

train2002 = train2002[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2003 = test2003[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2002.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2002,family=binomial)
summary(train2002.results)

probs<-predict(train2002.results,train2002,type="response")
pred2003<-rep("0",nrow(train2002))
pred2003[probs>0.5]<-"1"
table(train2002$AllStar,pred2003)
mean(pred2003==train2002$AllStar)

########## Training 2003, testing on 2004 ##########

train2003 = subset(seasonStats, Year == 2003)
test2004 = subset(seasonStats, Year == 2004)

train2003 = train2003[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2004 = test2004[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2003.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2003,family=binomial)
summary(train2003.results)

probs<-predict(train2003.results,train2003,type="response")
pred2004<-rep("0",nrow(train2003))
pred2004[probs>0.5]<-"1"
table(train2003$AllStar,pred2004)
mean(pred2004==train2003$AllStar)

########## Training 2004, testing on 2005 ##########

train2004 = subset(seasonStats, Year == 2004)
test2005 = subset(seasonStats, Year == 2005)

train2004 = train2004[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2005 = test2005[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2004.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2004,family=binomial)
summary(train2004.results)

probs<-predict(train2004.results,train2004,type="response")
pred2005<-rep("0",nrow(train2004))
pred2005[probs>0.5]<-"1"
table(train2004$AllStar,pred2005)
mean(pred2005==train2004$AllStar)

########## Training 2005, testing on 2006 ##########

train2005 = subset(seasonStats, Year == 2005)
test2006 = subset(seasonStats, Year == 2006)

train2005 = train2005[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2006 = test2006[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2005.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2005,family=binomial)
summary(train2005.results)

probs<-predict(train2005.results,train2005,type="response")
pred2006<-rep("0",nrow(train2005))
pred2006[probs>0.5]<-"1"
table(train2005$AllStar,pred2006)
mean(pred2006==train2005$AllStar)

########## Training 2006, testing on 2007 ##########

train2006 = subset(seasonStats, Year == 2006)
test2007 = subset(seasonStats, Year == 2007)

train2006 = train2006[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2007 = test2007[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2006.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2006,family=binomial)
summary(train2006.results)

probs<-predict(train2006.results,train2006,type="response")
pred2007<-rep("0",nrow(train2006))
pred2007[probs>0.5]<-"1"
table(train2006$AllStar,pred2007)
mean(pred2007==train2006$AllStar)

########## Training 2007, testing on 2008 ##########

train2007 = subset(seasonStats, Year == 2007)
test2008 = subset(seasonStats, Year == 2008)

train2007 = train2007[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2008 = test2008[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2007.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2007,family=binomial)
summary(train2007.results)

probs<-predict(train2007.results,train2007,type="response")
pred2008<-rep("0",nrow(train2007))
pred2008[probs>0.5]<-"1"
table(train2007$AllStar,pred2008)
mean(pred2008==train2007$AllStar)

########## Training 2008, testing on 2009 ##########

train2008 = subset(seasonStats, Year == 2008)
test2009 = subset(seasonStats, Year == 2009)

train2008 = train2008[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2009 = test2009[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2008.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2008,family=binomial)
summary(train2008.results)

probs<-predict(train2008.results,train2008,type="response")
pred2009<-rep("0",nrow(train2008))
pred2009[probs>0.5]<-"1"
table(train2008$AllStar,pred2009)
mean(pred2009==train2008$AllStar)

########## Training 2009, testing on 2010 ##########

train2009 = subset(seasonStats, Year == 2009)
test2010 = subset(seasonStats, Year == 2010)

train2009 = train2009[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2010 = test2010[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2009.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2009,family=binomial)
summary(train2009.results)

probs<-predict(train2009.results,train2009,type="response")
pred2010<-rep("0",nrow(train2009))
pred2010[probs>0.5]<-"1"
table(train2009$AllStar,pred2010)
mean(pred2010==train2009$AllStar)

########## Training 2010, testing on 2011 ##########

train2010 = subset(seasonStats, Year == 2010)
test2011 = subset(seasonStats, Year == 2011)

train2010 = train2010[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2011 = test2011[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2010.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2010,family=binomial)
summary(train2010.results)

probs<-predict(train2010.results,train2010,type="response")
pred2011<-rep("0",nrow(train2010))
pred2011[probs>0.5]<-"1"
table(train2010$AllStar,pred2011)
mean(pred2011==train2010$AllStar)

########## Training 2011, testing on 2012 ##########

train2011 = subset(seasonStats, Year == 2011)
test2012 = subset(seasonStats, Year == 2012)

train2011 = train2011[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2012 = test2012[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2011.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2011,family=binomial)
summary(train2011.results)

probs<-predict(train2011.results,train2011,type="response")
pred2012<-rep("0",nrow(train2011))
pred2012[probs>0.5]<-"1"
table(train2011$AllStar,pred2012)
mean(pred2012==train2011$AllStar)

########## Training 2012, testing on 2013 ##########

train2012 = subset(seasonStats, Year == 2012)
test2013 = subset(seasonStats, Year == 2013)

train2012 = train2012[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2013 = test2013[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2012.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2012,family=binomial)
summary(train2012.results)

probs<-predict(train2012.results,train2012,type="response")
pred2013<-rep("0",nrow(train2012))
pred2013[probs>0.5]<-"1"
table(train2012$AllStar,pred2013)
mean(pred2013==train2012$AllStar)

########## Training 2013, testing on 2014 ##########

train2013 = subset(seasonStats, Year == 2013)
test2014 = subset(seasonStats, Year == 2014)

train2013 = train2013[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2014 = test2014[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2013.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2013,family=binomial)
summary(train2013.results)

probs<-predict(train2013.results,train2013,type="response")
pred2014<-rep("0",nrow(train2013))
pred2014[probs>0.5]<-"1"
table(train2013$AllStar,pred2014)
mean(pred2014==train2013$AllStar)

########## Training 2014, testing on 2015 ##########

train2014 = subset(seasonStats, Year == 2014)
test2015 = subset(seasonStats, Year == 2015)

train2014 = train2014[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2015 = test2015[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2014.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2014,family=binomial)
summary(train2014.results)

probs<-predict(train2014.results,train2014,type="response")
pred2015<-rep("0",nrow(train2014))
pred2015[probs>0.5]<-"1"
table(train2014$AllStar,pred2015)
mean(pred2015==train2014$AllStar)

########## Training 2015, testing on 2016 ##########

train2015 = subset(seasonStats, Year == 2015)
test2016 = subset(seasonStats, Year == 2016)

train2015 = train2015[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2016 = test2016[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2015.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2015,family=binomial)
summary(train2015.results)

probs<-predict(train2015.results,train2015,type="response")
pred2016<-rep("0",nrow(train2015))
pred2016[probs>0.5]<-"1"
table(train2015$AllStar,pred2016)
mean(pred2016==train2015$AllStar)

########## Training 2016, testing on 2017 ##########

train2016 = subset(seasonStats, Year == 2016)
test2017 = subset(seasonStats, Year == 2017)

train2016 = train2016[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
test2017 = test2017[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]

train2016.results = glm(AllStar~WS+G+GS+FG+ORB+BLK+Salary,data=train2016,family=binomial)
summary(train2016.results)

probs<-predict(train2016.results,train2016,type="response")
pred2017<-rep("0",nrow(train2016))
pred2017[probs>0.5]<-"1"
table(train2016$AllStar,pred2017)
mean(pred2017==train2016$AllStar)

######### Creating Overall Training and Testing sets #########

overallStats = seasonStats[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
set.seed(25)
trainindex<-sample(nrow(overallStats),trunc(nrow(overallStats)/2))
train<-overallStats[trainindex,]
test<-overallStats[-trainindex,]

regfit.full=regsubsets(AllStar~.,data = train)
summary(regfit.full)

train.results = glm(AllStar~ Age + WS + G + FG. + FT + Salary,data=train,family=binomial)
summary(train.results)

probs<-predict(train.results,test,type="response")
pred<-rep("0",nrow(test))
pred[probs>0.5]<-"1"
table(test$AllStar,pred)
mean(pred==test$AllStar)


########## KNN Classification ##########
overallStats = seasonStats[,c(4,6,7,10:15,18,21,22,25,26,28,29,32:35,38,40,41,43,45,49,55,57)]
overallStats = na.omit(overallStats)
sum(is.na(overallStats))
sapply(overallStats,class)
########## Train is 2000 and test is 2001 ##########
prep<-preProcess(overallStats[,c(1:26,28)],method=c("scale"))

set.seed(20)
knnTrain2000 = sample(nrow(overallStats),trunc(nrow(overallStats)/2))
knnTrain2000 = na.omit(knnTrain2000)
knnTrain2000.x = overallStats[knnTrain2000,c(1:26,28)]  
knnTrain2000.y = overallStats[knnTrain2000,27]       
knnTest2001.x = overallStats[-knnTrain2000,c(1:26,28)]
knnTest2001.y = overallStats[-knnTrain2000,27]

# fit the model using k=10 as an arbitrary first cut

knn2001.fit = knn(data.frame(knnTrain2000.x),data.frame(knnTest2001.x),knnTrain2000.y,k=10)
table(knn2001.fit,knnTest2001.y)
mean(knn2001.fit==knnTest2001.y)

# Tune the model and find the best value of k

acc<-rep(0,100)
for (knum in 1:100) {
  knn2001.fit<-knn(data.frame(knnTrain2000.x),data.frame(knnTest2001.x),knnTrain2000.y,k=knum)
  acc[knum]<-mean(knn2001.fit==knnTest2001.y)
}

print(paste("Highest accuracy of",as.character(acc[which.max(acc)]),"at k =",as.character(which.max(acc) )))
plot(1:100,acc,type="p",xlab="k",ylab="Test Acc")


