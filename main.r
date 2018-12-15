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
avg_days <- 5


## Add extra features to matches (winning average, score average, days before the match)
time <- system.time(matches <- match_processing(comp_data, avg_days))

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
traindata <- train_features[,c(3:5,7:35,36,53,70,94,111,128)]
testclass <- test_features$Match_Result
testdata <- test_features[,c(3:5,7:35,36,53,70,94,111,128)]

#Results as numeric values
trainclass <- (trainclass == "Home")*1 + (trainclass == "Away")*2
testclass <- (testclass == "Home")*1 + (testclass == "Away")*2

#Matrix of Results to be used as an input to the RPS function
results <- matrix(1:(length(testclass)*3), 3)
results[1,] <- (testclass == 1)*1
results[2,] <- (testclass == 0)*1
results[3,] <- (testclass == 2)*1

names(traindata)
#choose which features to be used as inputs to the model

cols <- names(traindata)

cbind(names(traindata), c(1:ncol(traindata)))
cols <- cols[c(21:31,33:38)]


#### Model 1 - Nearest Neighbor

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
match.bagging <- bagging(Match_Result ~ ., data = train4, boos = TRUE, mfinal = 10, control = (minsplit = 0))

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

