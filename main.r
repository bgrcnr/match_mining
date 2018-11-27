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

#url <- "https://cran.r-project.org/src/contrib/Archive/KODAMA/KODAMA_0.0.1.tar.gz"

#install.packages(url, repos = NULL, type = "source", dependencies = FALSE)

getwd()
setwd("C:/Users/Bugra/Google Drive/Courses/MS_IE_Boun/IE 582 - Statistical Learning for Data Mining/Project/fall18-instructor-master")
rm(list=ls())
gc()
#save paths
matches_data_path = "C:/Files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"
odd_details_data_path = "C:/Files/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds"

testStart=as.Date('2018-08-16')
trainStart=as.Date('2012-07-15')
rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold

source('data_preprocessing.r')
source('feature_extraction.r')
source('performance_metrics.r')
source('train_models.r')



# read data
matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)

# preprocess matches
matches=matches_data_preprocessing(matches_raw)

# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)

# extract open and close odd type features from multiple bookmakers
features=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)

# divide data based on the provided dates 
train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
test_features=features[Match_Date>=testStart] 



train_features <- train_features[complete.cases(train_features)]
test_features <- test_features[complete.cases(test_features)]

trainclass <- train_features$Match_Result
traindata <- train_features[,c(-1,-2,-3,-7)]

testclass <- test_features$Match_Result
testdata <- test_features[,c(-1,-2,-3,-7)]

trainclass <- (trainclass == "Home")*1 + (trainclass == "Away")*2
testclass <- (testclass == "Home")*1 + (testclass == "Away")*2


results <- matrix(1:(90*3), 3)
results[1,] <- (testclass == 0)*1
results[2,] <- (testclass == 1)*1
results[3,] <- (testclass == 2)*1

cols <- c(3:5,25:30,(ncol(traindata)-3):ncol(traindata))



#### model 1 - nn

train1 <- traindata[,..cols]
test1 <- testdata[,..cols]

train1 <- scale(train1)
test1 <- scale(test1)

pred11 <- knn(train1,test1, trainclass, k = 29, prob = TRUE)

table(pred11,testclass)
sum(pred11==testclass)/length(testclass)



x <- rbind(train1,test1)
kdist <- KODAMA::knn.dist(x)
pred <- KODAMA::knn.predict(1:nrow(train1), (nrow(train1)+1):nrow(x), trainclass, kdist, k=29, agg.meth = "majority")
# display the confusion matrix
table(pred,testclass)
sum(pred==testclass)/length(testclass)
# view probabilities (all class probabilities are returned)
prob <- KODAMA::knn.probability(1:nrow(train1), (nrow(train1)+1):nrow(x), trainclass, kdist, k=29)



RPS_res <- RPS(prob, results)
RPS_res
avgrps1 <- RPS_res/length(testclass)
avgrps1

rps1mat <- RPS_matrix(prob,results)

########## End of Nearest Neighbor Analysis




require(nnet)


train2 <- traindata[,..cols]
test2 <- testdata[,..cols]

train2$Match_Result <- trainclass
test2$Match_Result <- testclass


multinomModel <- multinom(Match_Result ~ ., data=train2)
summary (multinomModel)

predicted_scores <- predict (multinomModel, test2, "probs") # predict on new data
predicted_class <- predict (multinomModel, test2)

table(predicted_class, testclass)
accuracy <- sum(predicted_class == testclass)/length(testclass)
accuracy

RPS2 <- RPS(t(predicted_scores), results)
RPS2
avgrps2 <- RPS2/length(testclass)
avgrps2

rps2mat <- RPS_matrix(t(predicted_scores), results)

####### End of Working Multinomial


train3 <- traindata[,..cols]
test3 <- testdata[,..cols]

sample_model <- train_glmnet(train_features,test_features)

sample_model

sample_mat <- results

sample_mat[1,] <- t(sample_model$predictions[,4])
sample_mat[2,] <- t(sample_model$predictions[,3])
sample_mat[3,] <- t(sample_model$predictions[,5])

rps3 <- RPS(sample_mat,results)
avgrps3 <- rps3/length(testclass)
avgrps3

rps3mat <- RPS_matrix(sample_mat,results)

#### End of Hoca Model

#### boosting
library(adabag)  # the main algorithm

train4 <- traindata[,..cols]
test4 <- testdata[,..cols]

train4$Match_Result <- as.factor(trainclass)
test4$Match_Result <- as.factor(testclass)

match.bagging <- bagging(Match_Result ~ ., data = train4, boos = TRUE, mfinal = 10, control = (minsplit = 0))

match.predbegging <- predict.bagging(match.bagging, newdata = test4)

boosting_probs <- t(match.predbegging$prob)

rps4 <- RPS(boosting_probs,results)

rps4
avgrps4 <- rps4/length(testclass)
avgrps4


rps4mat <- RPS_matrix(boosting_probs,results)

rps1mat
rps2mat
rps3mat
rps4mat
