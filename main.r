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
testStart=as.Date('2018-04-16')
trainStart=as.Date('2013-09-15')
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
avg_days <- 3
avg_method <- "w"




## Add extra features to matches (winning average, score average, days before the match)
time <- system.time(matches <- match_processing(comp_data, avg_days, avg_method) )

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


cbind(names(train_features), c(1:ncol(train_features)))

#Seperate Results and Data, remove matchID, MatchDate and LeagueID columns
trainclass <- train_features$Match_Result
traindata <- train_features[,c(3:5,7:42,43,60,77,95,112,129)]
testclass <- test_features$Match_Result
testdata <- test_features[,c(3:5,7:42,43,60,77,95,112,129)]

#Results as numeric values
trainclass <- (trainclass == "Home")*1 + (trainclass == "Away")*2
testclass <- (testclass == "Home")*1 + (testclass == "Away")*2

#Matrix of Results to be used as an input to the RPS function
results <- matrix(1:(length(testclass)*3), 3)
results[1,] <- (testclass == 1)*1
results[2,] <- (testclass == 0)*1
results[3,] <- (testclass == 2)*1


cols <- names(traindata)

cbind(names(traindata), c(1:ncol(traindata)))
cols <- cols[c(9:45)]

# Inputs are generated from data files
train1 <- traindata[,..cols]
test1 <- testdata[,..cols]
#Inputs are scaled
train1 <- as.data.table(scale(train1))
test1 <- as.data.table(scale(test1))


#knn model is constructed. k is determined arbitrarily, no cross validation
pred11 <- knn(train1,test1, trainclass, k = 29, prob = TRUE)

#confusion matrix and accuracy
confusion_matrix_knn <- table(pred11,testclass)
accuracy_knn <- sum(pred11==testclass)/length(testclass)


# Bind train and test data to be used as an input in KODAMA's knn.dist function
x <- rbind(train1,test1)

#Distances are calculated
kdist <- KODAMA::knn.dist(x)

#Prediction is made
pred <- KODAMA::knn.predict(1:nrow(train1), (nrow(train1)+1):nrow(x), trainclass, kdist, k=29, agg.meth = "majority")

# display the confusion matrix and accuracy
confusion_matrix_knn_kodama <- table(pred,testclass)
accuracy_knn_kodama <- sum(pred==testclass)/length(testclass)
accuracy_knn_kodama
# view probabilities (all class probabilities are returned)
prob <- KODAMA::knn.probability(1:nrow(train1), (nrow(train1)+1):nrow(x), trainclass, kdist, k=29)

prob_rearranged <- prob
prob_rearranged[1,] <- prob[2,]
prob_rearranged[2,] <- prob[1,]

# RPS Results are calculated
  rps1mat <- RPS_matrix(t(prob_rearranged),t(results))
rps1 <- mean(rps1mat)
rps1
#Output of RPS_Matrix function


########## End of Nearest Neighbor Analysis

##### Model 2: Multinomial Regression

#Model inputs determined
train2 <- traindata[,..cols]
test2 <- testdata[,..cols]

train2 <- as.data.table(scale(train2))
test2 <- as.data.table(scale(test2))
#Multinomial model requires results to be in the data
train2$Match_Result <- trainclass
test2$Match_Result <- testclass

#Model is generated
multinomModel <- multinom(Match_Result ~ ., data=train2)
summary (multinomModel)

#Probabilities for each class and the results are generated
predicted_scores <- predict (multinomModel, test2, "probs") # predict on new data
predicted_class <- predict (multinomModel, test2)

#Confusion matrix and accuracy
confusion_matrix_multinom <- table(predicted_class, testclass)
accuracy_multinom <- sum(predicted_class == testclass)/length(testclass)
accuracy_multinom

prob_rearranged <- predicted_scores
prob_rearranged[,1] <- predicted_scores[,2]
prob_rearranged[,2] <- predicted_scores[,1]


##  average RPS and RPS Matrix
rps2mat <- RPS_matrix(prob_rearranged, t(results))
rps2 <- mean(rps2mat)
rps2


#Inputs
train4 <- traindata[,..cols]
test4 <- testdata[,..cols]
train4 <- as.data.table(scale(train4))
test4 <- as.data.table(scale(test4))

#Bagging Model requires results to be in input data
train4$Match_Result <- as.factor(trainclass)
test4$Match_Result <- as.factor(testclass)

#Model is generated
match.bagging <- bagging(Match_Result ~ ., data = train4)

#Predictions are made
match.predbegging <- predict.bagging(match.bagging, newdata = test4)

#Probabilities for each class
boosting_probs <- t(match.predbegging$prob)


# Average RPS and RPS Matrix

prob_rearranged <- boosting_probs
prob_rearranged[1,] <- boosting_probs[2,]
prob_rearranged[2,] <- boosting_probs[1,]

rps4mat <- RPS_matrix(t(prob_rearranged),t(results))
rps4 <- mean(rps4mat)
rps4

### tree
require(rpart)

cols <- names(traindata)

cbind(names(traindata), c(1:ncol(traindata)))
cols <- cols[c(1:37)]



train5 <- traindata[,..cols]
test5 <- testdata[,..cols]
train5 <- as.data.table(scale(train5))
test5 <- as.data.table(scale(test5))
test5class <- test_features$Match_Result
train5 <- cbind(train_features$Match_Result
,train5)
colnames(train5)[1] <- "Match_Class"


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

train5 <- traindata[,..cols]
test5 <- testdata[,..cols]
train5 <- as.matrix(scale(train5))
test5 <- as.matrix(scale(test5))
trainlabel <- as.matrix(trainclass)
testlabel <- as.matrix(testclass)

train_matrix <- xgb.DMatrix(data = train5, label = trainlabel)
test_matrix <- xgb.DMatrix(data = test5, label = testlabel)

numberOfClasses <- length(unique(testlabel))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)
cv_model$pred
require(dplyr)
OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = trainlabel + 1)
head(OOF_prediction)

confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)

# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = testlabel + 1,
         max_prob = max.col(., "last"))

probs_boost <- test_prediction[,1:3]
probs_boost[,1] <- test_prediction[,2]
probs_boost[,2] <- test_prediction[,1]
probs_boost[,3] <- test_prediction[,3]

rps7 <- RPS_matrix(probs_boost, t(results))
rps7 <- mean(rps7)
rps7


# confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

names <-  colnames(train5)
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)

####### support vector machines 


train6 <- traindata[,..cols]
test6 <- testdata[,..cols]
trainlabel <- as.factor(trainlabel)
testlabel <- as.factor(testlabel)

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
