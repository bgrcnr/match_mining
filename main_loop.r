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

source('data_preprocessing.r')
source('feature_extraction.r')
source('performance_metrics.r')
source('train_models.r')

#save paths
matches_data_path = "Files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"
odd_details_data_path = "Files/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds"

# read data
matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)


# preprocess matches
#matched datapreprocessing function is edited, it adds unixdate and weekday columns
matches=matches_data_preprocessing(matches_raw)


# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)

#dummy dates list
dates <- as.Date('2018-06-16')
dates <- rep(dates, (2018-2011)*12)


#fill dates list by date
for (j in 2011:2018)
  {
  for(i in 1:12)
    {
    dates[i+(j-2011)*12] <- as.Date(paste(j,"-",i,"-",15,sep=""))
    }
}

##results table
loopresult <- data.table()


##main loop
for(i in 20:70)
{
  for(j in 91:93)
  {
    
#train and test dates
testStart=as.Date(dates[j])
trainStart=as.Date(dates[i])
rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold



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
traindata <- train_features[,c(-1,-2,-3,-7)]
testclass <- test_features$Match_Result
testdata <- test_features[,c(-1,-2,-3,-7)]

#Results as numeric values
trainclass <- (trainclass == "Home")*1 + (trainclass == "Away")*2
testclass <- (testclass == "Home")*1 + (testclass == "Away")*2


#Matrix of Results to be used as an input to the RPS function
results <- matrix(1:(length(testclass)*3), 3)
results[1,] <- (testclass == 0)*1
results[2,] <- (testclass == 1)*1
results[3,] <- (testclass == 2)*1


#choose which features to be used as inputs to the model
cols <- c("Odd_Open_odd1_Pinnacle", "Odd_Open_oddX_Pinnacle", "Odd_Open_odd2_Pinnacle", 
          "Odd_Close_odd1_Pinnacle", "Odd_Close_odd2_Pinnacle", "Odd_Close_oddX_Pinnacle")


#### Model 1 - Nearest Neighbor

# Inputs are generated from data files
train1 <- traindata[,..cols]
test1 <- testdata[,..cols]

#Inputs are scaled
train1 <- scale(train1)
test1 <- scale(test1)

#knn model is constructed. k is determined arbitrarily, no cross validation
pred11 <- knn(train1,test1, trainclass, k = 29, prob = TRUE)

#confusion matrix and accuracy
confusion_matrix_knn <- table(pred11,testclass)
acuracy_knn <- sum(pred11==testclass)/length(testclass)


# Bind train and test data to be used as an input in KODAMA's knn.dist function
x <- rbind(train1,test1)

#Distances are calculated
kdist <- KODAMA::knn.dist(x)

#Prediction is made
pred <- KODAMA::knn.predict(1:nrow(train1), (nrow(train1)+1):nrow(x), trainclass, kdist, k=29, agg.meth = "majority")

# display the confusion matrix and accuracy
confusion_matrix_knn_kodama <- table(pred,testclass)
accuracy_knn_kodama <- sum(pred==testclass)/length(testclass)

# view probabilities (all class probabilities are returned)
prob <- KODAMA::knn.probability(1:nrow(train1), (nrow(train1)+1):nrow(x), trainclass, kdist, k=29)

# RPS Results are calculated
rps1 <- RPS_single(prob, results)
rps1

#Output of RPS_Matrix function
rps1mat <- RPS_matrix(t(prob),t(results))

########## End of Nearest Neighbor Analysis

##### Model 2: Multinomial Regression

#Model inputs determined
train2 <- traindata[,..cols]
test2 <- testdata[,..cols]

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

##  average RPS and RPS Matrix
rps2 <- RPS_single(t(predicted_scores), results)

rps2mat <- RPS_matrix(predicted_scores, t(results))

####### End of Multinomial Logistic Regression Model

### Instructor's Benchmark Model

# Input Data
train3 <- traindata[,..cols]
test3 <- testdata[,..cols]

# Model is constructed
sample_model <- train_glmnet(train_features,test_features)

sample_model


#Model results as a matrix
sample_mat <- results
sample_mat[1,] <- t(sample_model$predictions[,4])
sample_mat[2,] <- t(sample_model$predictions[,3])
sample_mat[3,] <- t(sample_model$predictions[,5])

# average RPS and RPS Matrix
rps3 <- RPS_single(sample_mat,results)
rps3mat <- RPS_matrix(t(sample_mat),t(results))


#### End of Instructor's Model


#### Boosting, Bagging

#Inputs
train4 <- traindata[,..cols]
test4 <- testdata[,..cols]

#Bagging Model requires results to be in input data
train4$Match_Result <- as.factor(trainclass)
test4$Match_Result <- as.factor(testclass)

#Model is generated
match.bagging <- bagging(Match_Result ~ ., data = train4, boos = TRUE, mfinal = 10, control = (minsplit = 0))

#Predictions are made
match.predbegging <- predict.bagging(match.bagging, newdata = test4)

#Probabilities for each class
boosting_probs <- t(match.predbegging$prob)


# Average RPS and RPS Matrix
rps4 <- RPS_single(boosting_probs,results)
rps4

rps4mat <- RPS_matrix(t(boosting_probs),t(results))

#write the results to the table
loopresult = rbind(loopresult, data.table(TrainDate = dates[i], TestDate = dates[j], 
                    NN_accuracy_1 = acuracy_knn, NN_accuracy_2 = accuracy_knn_kodama, 
                    Multinom_accuracy = accuracy_multinom,  NN_rps = rps1, 
                    Multinom_rps = rps2, Instructor_rps = rps3, Bagging_rps = rps4))
  }
}

loopresult
summary(loopresult)

write.csv(ordered_result)


ordered_result <- loopresult[order(Multinom_rps,Instructor_rps,Bagging_rps,NN_rps)]

pdf("result_table.pdf", width = 7, height = 12)
par(mfrow = c(2,1))
plot(loopresult$NN_rps, ylim = c(min(loopresult$NN_rps), max(loopresult$Bagging_rps)))
points(loopresult$Multinom_rps, col = "2")
points(loopresult$Instructor_rps, col = "3")
points(loopresult$Bagging_rps, col = "4")
legend("topleft", cex = 0.5, legend = c("NN", "Multinom", "Instructor", "Bagging"), col = c(1,2,3,4), pch = 1 )

plot(loopresult$NN_rps, ylim = c(min(loopresult$NN_rps), max(loopresult$NN_rps)))
points(loopresult$Multinom_rps, col = "2")
points(loopresult$Instructor_rps, col = "3")
legend("topleft", cex = 0.5, legend = c("NN", "Multinom", "Instructor"), col = c(1,2,3), pch = 1 )


dev.off()
