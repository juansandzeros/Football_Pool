#Here is a sample of these features.
#{weekNo VT_TeamNo , final_score, AVG_YARDS_BY PASSING, AVG_YARDS_BY PENALTY[1]
#  AVG_THIRD DOWN EFFICIENCY, AVG_FOURTH DOWN EFFICIENCY EFFICIENCY[2],TOTAL NET
#  YARDS, TOTAL OFFENSIVE PLAYS AVERAGE GAIN PER OFFENSIVE PLAY, NET YARDS RUSHING,
#  TOTAL RUSHING PLAYS, AVERAGE GAIN PER RUSH, TACKLES FOR A LOSS-NUMBER,...}  **78 per team

#{weekNo, VT_TeamNo VT_BothWin-ratio VT_HomeWinRatio VT_AwayWinRatio VT_3weekWinRatio
#  VT_AvgPointsScored VT_AvgPointsAllowed HT_TeamNo HT_BothWinRatio HT_HomeWinRatio
#  HT_AwayWinRatio HT_3weekWinRatio HT_AvgPtsScored HT_AvsPtsAllowed }

# Look at Point differential - is better measure of future wins than its actual win total
# also  Pythagorean projection (win projection) %
# points for ^ 2.37 / points for ^ 2.37 + points against ^ 2.37 X games played 16
#https://www.footballoutsiders.com/stat-analysis/2017/presenting-adjusted-pythagorean-theorem

# then subtract out the stength of schedule (SoS)

# also try Tow/Tol (turn overs)

# *** Yards per Attempt or Adjusted ANY/A
# *** DVOA = defense adjusted value over avg

#https://www.espn.com/nfl/story/_/id/20114211/the-nfl-stats-matter-most-2017-offseason-bill-barnwell
#https://www.footballoutsiders.com/stat-analysis/2003/pythagoras-gridiron
#https://www.footballoutsiders.com/info/methods
#https://www.r-bloggers.com/nfl-series/
#https://statsbylopez.netlify.com/post/r-for-nfl-analysis/

df <- read.csv(file="C:/Users/jharrington/Documents/Projects/FootballPool/Data/NFLGamesDataForModel2.csv", header=TRUE, sep=",")

#df$HomeStrengthSchedule <- log10(df$HomeStrengthSchedule + 1 - min(df$HomeStrengthSchedule))
#df$HomeMarginVictory <- log10(df$HomeMarginVictory + 1 - min(df$HomeMarginVictory))
#df$AwayStrengthSchedule <- log10(df$AwayStrengthSchedule + 1 - min(df$AwayStrengthSchedule))
#df$AwayMarginVictory <- log10(df$AwayMarginVictory + 1 - min(df$AwayMarginVictory))

#df <- subset(df,select=c(1,4,5,6,7,9,10,13,14,15,16,18,19,21))
df <- subset(df,select=-c(3,14))

#modelbase <- glm(IsHomeWin ~.,family=binomial,data=df)
#summary(modelbase)

train <- df[1:1536,]
test <- df[1537:2048,]

model.base <- glm(IsHomeWin ~.,family=binomial,data=train)
summary(model.base)

fitted.results <- predict(model.base,newdata=subset(test,select=-c(23)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$IsHomeWin)
print(paste('Accuracy',1-misClasificError))

##61.5

#dt = sort(sample(nrow(df), nrow(df)*.75))
#train<-df[dt,]
#test<-df[dt,]

#train <- subset(train,select=c(4,5,6,7,8,9,10,11,13,14,15,16,17,18,19))
#test <- subset(test,select=c(4,5,6,7,8,9,10,11,13,14,15,16,17,18,19))


# fit logistic regression
model <- glm(IsHomeWin ~ HomeWinProjection+AwayWinProjection,family=binomial(link='logit'),data=train)
summary(model)

fitted.results <- predict(model,newdata=subset(test,select=-c(23)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$IsHomeWin)
print(paste('Accuracy',1-misClasificError))


#stepwise
library(tidyverse)
library(MASS)
step.model <- model.base %>% stepAIC(trace = FALSE)
stp=stepAIC(modelbase, trace = FALSE)
stp$anova
coef(step.model)

# Make predictions
probabilities <- predict(step.model, test, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
# Prediction accuracy
observed.classes <- test$IsHomeWin
mean(predicted.classes == observed.classes)


##################################

# load in current week
dfCurrent <- read.csv(file="C:/Users/jharrington/Documents/Projects/FootballPool/Data/NFLWeek2DataModel.csv", header=TRUE, sep=",")

#
#validation  <- subset(dfCurrent,select=c(4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20))
validationdf <- subset(dfCurrent,select=c(1,4,5,6,7,8,9,10,13,15,16,18,19,21,22,23,24))


validationdf <- subset(dfCurrent,select=c(8,19))

#validation$HomeStrengthSchedule <- log10(validation$HomeStrengthSchedule + 1 - min(validation$HomeStrengthSchedule))
#validation$HomeMarginVictory <- log10(validation$HomeMarginVictory + 1 - min(validation$HomeMarginVictory))
#validation$AwayStrengthSchedule <- log10(validation$AwayStrengthSchedule + 1 - min(validation$AwayStrengthSchedule))
#validation$AwayMarginVictory <- log10(validation$AwayMarginVictory + 1 - min(validation$AwayMarginVictory))

# create standalone model using all training data

# make a predictions on "new data" using the final model
final_predictions <- predict(model, validationdf[,1:2], type = "response")
final.results <- ifelse(final_predictions > 0.5,1,0)

dfCurrent$IsHomeWin <- final.results
dfCurrent$Pred <- final_predictions

clm <- c("HomeTeam", "AwayTeam", "IsHomeWin", "Pred")
list <- subset(dfCurrent, select=clm)
list

# fit decision tree

library(rpart)
library(rpart.plot)

control <- rpart.control(minsplit = 2, minbucket = round(5/3), maxdepth = 6, cp = 0)
fit <- rpart(IsHomeWin~., data = train, method = 'class', control = control)
#rpart.plot(fit, extra = 106)
#printcp(fit)

predict_unseen <-predict(fit, test, type = 'class')

table_mat <- table(test$IsHomeWin, predict_unseen)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

# svm cv

# Create 5 folds
library(e1071)
k=5
folds <- sample(1:k, nrow(train), replace = T)

# Create empty data frame to store error rates
mush.err <- NULL

# perform 5-fold cross validation
for(i in 1:k){
  # fit logistic regression
  mush.svm.cv <- svm(IsHomeWin ~ .,
                     data = train[folds!=i,])
  
  # predict
  prob.mush.svm.cv <- predict(mush.svm.cv, 
                              train[folds==i,],
                              type="response")
  pred.mush.svm.cv <- ifelse(prob.mush.svm.cv > 0.5, 1, 0)
  
  # calculate error and add to array
  mush.err[i] <- mean(pred.mush.svm.cv != train$IsHomeWin[folds==i]) 
}

# display error rates
data.frame(mush.err)

# error rate
mush.svm.cv.error <- round(mean(mush.err), 7)

#knn
library(class)
train.class = df[1:1440, 19]
test.class = df[1441:1920, 19]
pred.mush.knn.vsa <- knn(train = train,
                        test = test,
                        cl = train.class, k = 10, use.all = T)

# confusion matrix
table(pred.mush.knn.vsa, test.class)


# error rate
mush.knn.vsa.error <- mean(pred.mush.knn.vsa != test.class)


# fit boosted tree
library(gbm)
set.seed(70)
boosted <- gbm(IsHomeWin~., data=train, distribution="bernoulli",n.trees=5000,interaction.depth=7)
summary(boosted)


yhat.boost <- predict(boosted, newdata=test, n.trees=5000,type="response")

fitted.results <- ifelse(yhat.boost > 0.5,1,0)
table_mat <- table(test$IsHomeWin, fitted.results)
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))




### test 2

dfCurrent <- read.csv(file="C:/Users/jharrington/Documents/Projects/FootballPool/Data/NFLWeek17DataModel.csv", header=TRUE, sep=",")

#
#validation  <- subset(dfCurrent,select=c(4,5,6,7,8,9,10,11,13,14,15,16,17,18,19,20))
validationdf <- subset(dfCurrent,select=-c(3,14))


#validation$HomeStrengthSchedule <- log10(validation$HomeStrengthSchedule + 1 - min(validation$HomeStrengthSchedule))
#validation$HomeMarginVictory <- log10(validation$HomeMarginVictory + 1 - min(validation$HomeMarginVictory))
#validation$AwayStrengthSchedule <- log10(validation$AwayStrengthSchedule + 1 - min(validation$AwayStrengthSchedule))
#validation$AwayMarginVictory <- log10(validation$AwayMarginVictory + 1 - min(validation$AwayMarginVictory))

# create standalone model using all training data

# make a predictions on "new data" using the final model
final_predictions <- predict(model.base, validationdf[,1:22],type = 'response')
final.results <- ifelse(final_predictions > 0.5,1,0)

dfCurrent$IsHomeWin <- final.results
dfCurrent$Pred <- final_predictions

clm <- c("HomeTeam", "AwayTeam", "IsHomeWin", "Pred")
list <- subset(dfCurrent, select=clm)
list




