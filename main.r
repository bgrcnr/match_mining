require(data.table)
require(anytime)
require(scatterplot3d)
require(FNN)
require(glmnet)
require(TunePareto)
require(data.table)
require(RANN.L1)
require(lubridate)
require(devtools)
require(nnet)
library(adabag)
require(pracma)
require(gbm)
require(xgboost)
require(e1071)
require(Ckmeans.1d.dp)


source('data_preprocessing.r')
source('feature_extraction.r')
source('performance_metrics.r')
source('train_models.r')
### match_processing is for adding average goals, average scores and days before the match
source('match_processing.r')


#save paths
matches_data_path = "Files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"
odd_details_data_path = "Files/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds"

#train and test dates
testStart=as.Date('2018-03-16')
trainStart=as.Date('2011-09-15')
rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold


# read data
city_distances <- read.csv("city_distances.csv")
s_2018 <- as.data.table(read.csv("Files/2018_data.csv"))
s_2017 <- as.data.table(read.csv("Files/2017_data.csv"))
s_2016 <- as.data.table(read.csv("Files/2016_data.csv"))
s_2015 <- as.data.table(read.csv("Files/2015_data.csv"))
s_2014 <- as.data.table(read.csv("Files/2014_data.csv"))
s_2013 <- as.data.table(read.csv("Files/2013_data.csv"))
s_2012 <- as.data.table(read.csv("Files/2012_data.csv"))
s_2011 <- as.data.table(read.csv("Files/2011_data.csv"))
s_2010 <- as.data.table(read.csv("Files/2010_data.csv"))

matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)

# preprocess matches
#matched datapreprocessing function is edited, it adds unixdate and weekday columns

matches=matches_data_preprocessing(matches_raw)



additional_data <- rbind(s_2010[,1:23],s_2011[,1:23],s_2012[,1:23],s_2013[,1:23],s_2014[,1:23]
                           ,s_2015[,1:23],s_2016[,1:23],s_2017[,1:23],s_2018[,1:23])


additional_data$HomeTeam <- as.character(additional_data$HomeTeam)
additional_data$AwayTeam <- as.character(additional_data$AwayTeam)
additional_data$Date <- dmy(additional_data$Date)


additional_data <- additional_data[complete.cases(additional_data)]

names1 <- sort(unique(additional_data$HomeTeam))
names2 <- sort(unique(matches$Home))
names3 <- cbind(names1,names2)



for(i in 1:nrow(names3))
{
  additional_data[HomeTeam == names3[i,1]]$HomeTeam <- names3[i,2]
  additional_data[AwayTeam == names3[i,1]]$AwayTeam <- names3[i,2]
  
}

additional_data <- additional_data[,Div:=NULL]

col_names <- c("Date", "Home", "Away", "Full Time Home Team Goals",
               "Full Time Away Team Goals", "Full Time Result" ,"Half Time Home Team Goals",
               "Half Time Away Team Goals", "Half Time Result", "Referee", "Home Team Shots",
               "Away Team Shots", "Home Team Shots on Target", "Away Team Shots on Target",
               "Home Team Fouls Committed", "Away Team Fouls Committed", "Home Team Corners",
               "Away Team Corners", "Home Team Yellow Cards", "Away Team Yellow Cards",
               "Home Team Red Cards", "Away Team Red Cards")

colnames(additional_data) <- col_names

comp_data <- merge(matches, additional_data, by.x = c("Match_Date", "Home", "Away"), by.y  = c("Date", "Home", "Away"), all.x = TRUE)



## Add extra features to matches (winning average, score average, days before the match)
matches <- match_processing(comp_data, 3, "t")

# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches,which_bets = c("1x2"))

# extract open and close odd type features from multiple bookmakers
features=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)

# divide data based on the provided dates 
train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
test_features=features[Match_Date>=testStart] 

#keep complete cases
train_features <- train_features[complete.cases(train_features)]
test_features <- test_features[complete.cases(test_features)]

#Seperate Results and Data, remove matchID, MatchDate and LeagueID columns
trainclass <- train_features$Match_Result
traindata <- train_features[,-c(1,2,6)]
testclass <- test_features$Match_Result
testdata <- test_features[,-c(1,2,6)]

#Results as numeric values
trainclass <- (trainclass == "Home")*1 + (trainclass == "Away")*2
testclass <- (testclass == "Home")*1 + (testclass == "Away")*2

#Matrix of Results to be used as an input to the RPS function
results <- matrix(1:(length(testclass)*3), 3)
results[1,] <- (testclass == 1)*1
results[2,] <- (testclass == 0)*1
results[3,] <- (testclass == 2)*1


cols <- names(traindata)

##### Model 2: Multinomial Regression

#Model inputs determined
train2 <- traindata[,..cols]
test2 <- testdata[,..cols]

#Multinomial model requires results to be in the data
train2$Match_Result <- trainclass
test2$Match_Result <- testclass

#Model is generated
multinomModel <- multinom(Match_Result ~ ., data=train2)

#Probabilities for each class and the results are generated
predicted_scores <- predict (multinomModel, test2, "probs") # predict on new data
predicted_class <- predict (multinomModel, test2)

#Confusion matrix and accuracy
confusion_matrix_multinom <- table(predicted_class, testclass)
accuracy_multinom <- sum(predicted_class == testclass)/length(testclass)

prob_rearranged <- predicted_scores
prob_rearranged[,1] <- predicted_scores[,2]
prob_rearranged[,2] <- predicted_scores[,1]


##  average RPS and RPS Matrix
rps2mat <- RPS_matrix(prob_rearranged, t(results))
rps2 <- mean(rps2mat)
rps2
?multinom
bugra_results <- rbind(bugra_results, data.table(day = i, method = j, sample = k, traindate = a, testdate = b, sample_no = g, time = time[1], accu = accuracy_multinom, rps = rps2))
}}}}}}


sub <- cbind(prob_rearranged, test_features$matchId, test_features$Unix_Date,test_features$Match_Result)
sub <- as.data.table(sub)
sub <- sub[order(V5)]
sub[158:180,]
sub <- sub[158:172,]
sub <- merge(sub,matches[,c("matchId","Home","Away","Match_Date")], by.x = "V4", by.y = "matchId")
sub <- sub[order(Match_Date)]
write.csv(sub, file = "C:/Users/Bugra/Documents/GitHub/match_mining/sub7.csv")
getwd()
test_features[order(Match_Date)]
gulsah <- bugra_results[order(rps)]
gulsah[1:50,]
unique(gulsah$testdate)
unique(gulsah$traindate)
test_date
train_date
gulsah2 <- gulsah
train1 <- traindata[,..cols]
test1 <- testdata[,..cols]
trainc1 <- as.factor((trainclass==1)*0 + (trainclass==0)*1+(trainclass==2)*2)
testc1 <- as.factor((testclass==1)*0 + (testclass==0)*1+(testclass==2)*2)
train1 <- cbind(train1,trainc1)

############

# Inputs are generated from data files
cols <- 2:51
train1 <- traindata[,..cols]
test1 <- testdata[,..cols]
train1 <- as.data.table(scale(train1))
test1 <- as.data.table(scale(test1))


#knn model is constructed. k is determined arbitrarily, no cross validation
pred11 <- knn(train1,test1, trainclass, k = 95, prob = TRUE)

#confusion matrix and accuracy
confusion_matrix_knn <- table(pred11,testclass)
accuracy_knn <- sum(pred11==testclass)/length(testclass)


# Bind train and test data to be used as an input in KODAMA's knn.dist function
x <- rbind(train1,test1)

#Distances are calculated
kdist <- KODAMA::knn.dist(x)

#Prediction is made
pred <- KODAMA::knn.predict(1:nrow(train1), (nrow(train1)+1):nrow(x), trainclass, kdist, k=95, agg.meth = "majority")

# display the confusion matrix and accuracy
confusion_matrix_knn_kodama <- table(pred,testclass)
accuracy_knn_kodama <- sum(pred==testclass)/length(testclass)
accuracy_knn_kodama
# view probabilities (all class probabilities are returned)
prob <- KODAMA::knn.probability(1:nrow(train1), (nrow(train1)+1):nrow(x), trainclass, kdist, k=95)

prob_rearranged <- prob
prob_rearranged[1,] <- prob[2,]
prob_rearranged[2,] <- prob[1,]

# RPS Results are calculated
rps1mat <- RPS_matrix(t(prob_rearranged),t(results))
rps1 <- mean(rps1mat)
rps1
#Output of RPS_Matrix function


########## End of Nearest Neighbor Analysis

#Inputs
train4 <- traindata[,..cols]
test4 <- testdata[,..cols]
train4 <- as.data.table(scale(train4))
test4 <- as.data.table(scale(test4))

#Bagging Model requires results to be in input data
train4$Match_Result <- as.factor(trainclass)
test4$Match_Result <- as.factor(testclass)

#Model is generated
match.bagging <- bagging(Match_Result ~ ., data = train4,boos = TRUE, mfinal = 10, control = (minsplit = 0))
?bagging

#Predictions are made
match.predbegging <- predict.bagging(match.bagging, newdata = test4)

#Probabilities for each class
boosting_probs <- t(match.predbegging$prob)

?bagging
# Average RPS and RPS Matrix

prob_rearranged <- boosting_probs
prob_rearranged[1,] <- boosting_probs[2,]
prob_rearranged[2,] <- boosting_probs[1,]

rps4mat <- RPS_matrix(t(prob_rearranged),t(results))
rps4 <- mean(rps4mat)
rps4

### tree


cols <- names(traindata)

cbind(names(traindata), c(1:ncol(traindata)))
cols <- cols[c(1:37)]


require(rpart)
train5 <- traindata[,..cols]
test5 <- testdata[,..cols]
train5 <- as.data.table(scale(train5))
test5 <- as.data.table(scale(test5))
test5class <- test_features$Match_Result
train5 <- cbind(train_features$Match_Result
,train5)
colnames(train5)[1] <- "Match_Class"

?rpart
tree.match <- rpart(Match_Class~., train5)
tree.pred <- predict(tree.match, test5, type = "class")
table(tree.pred,test5class)

sum(tree.pred==test5class)/nrow(test5)

cv_results <- as.data.table(xpred.rpart(tree.match))

printcp(tree.match)
plotcp(tree.match)
which.min(tree.match$cptable)

tree.prune <- prune(tree.match, cp = 0.01)
tree_probs <- as.data.table(predict(tree.prune, test5))

tprobs <- t(tree_probs)
tprobs[1,] <- t(tree_probs[,2])
tprobs[2,] <- t(tree_probs[,3])
tprobs[3,] <- t(tree_probs[,1])

tprobs <- t(tprobs)

rps5 <- RPS_matrix(tprobs,t(results))
rps5 <- mean(rps5)
rps5


library(randomForest)
train5 <- traindata[,..cols]
test5 <- testdata[,..cols]
train5 <- as.data.table(scale(train5))
test5 <- as.data.table(scale(test5))
train5class <- as.factor(train_features$Match_Result)

rf_matches <- randomForest(train5, y = train5class, ntree = 1000, proximity = TRUE)
prob_classes <- predict(rf_matches, test5)
table(prob_classes, test5class)
sum(tree.pred==test5class)/nrow(test5)

probs_rf <- predict(rf_matches,test5, type = "prob")

probs5 <- probs_rf
probs5[,1] <- probs_rf[,2]
probs5[,2] <- probs_rf[,3]
probs5[,3] <- probs_rf[,1]

importance(rf_matches)

rps6 <- RPS_matrix(probs5, t(results))
rps6 <- mean(rps6)
rps6

########boosting

train5 <- cbind(traindata[,..cols],trainclass)
test5 <- testdata[,..cols]


gbm.model <- gbm(trainclass~., data = train5, n.trees = 100, interaction.depth = 1,
                        n.minobsinnode = 10, shrinkage =0.1, distribution = "multinomial")
gbm.prob <- predict(gbm.model, test5, n.trees = 100, type = "response")
gbm.pred <- apply(gbm.prob, 1, which.max)
table(gbm.pred-1, testclass)
sum((gbm.pred-1)==testclass)/nrow(test5)

probboost <- as.data.frame(gbm.prob)
probboost2 <- as.data.frame(gbm.prob)
probboost[,1] <- probboost2[,2]
probboost[,2] <- probboost2[,1]

rps7 <- RPS_matrix(probboost, t(results))
rps7 <- mean(rps7)
rps7

####### support vector machines 


train6 <- traindata[,..cols]
test6 <- testdata[,..cols]
trainlabel <- as.factor(trainclass)
testlabel <- as.factor(testclass)

train6 <- data.frame(train6,trainlabel)

tune.out <- tune(svm, trainlabel~., data = train6, kernel = "radial", 
                 ranges = list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))

svmfit <- svm(trainlabel~., data = train6, kernel = "radial", cost = 0.1, scale = TRUE, prob = TRUE)

bestsvm <- tune.out$best.model

svmpred <- predict(bestsvm, test6)
table(svmpred,testlabel)
sum(svmpred==testlabel)/nrow(test6)

svmprob <- predict(svmfit, test6, probability = TRUE)
attributes(svmprob)$probabilities


probsvm1 <- attributes(svmprob)$probabilities
probsvm <- probsvm1
probsvm[,1] <- probsvm1[,3]
probsvm[,3] <- probsvm1[,1]

rps8 <- RPS_matrix(probsvm, t(results))
rps8 <- mean(rps8)
rps8
