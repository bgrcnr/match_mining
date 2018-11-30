require(data.table)

setwd("C:\\Users\\gulsa\\Desktop\\2018-2019 Güz\\IE582\\Project\\match_mining")
rm(list = ls())
gc()

matches_data_path = "Files/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds"
odd_details_data_path = "Files/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds"

testStart=as.Date('2018-09-16')
trainStart=as.Date('2016-07-15')
rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold

source('data_preprocessing.r')
source('feature_extraction.r')
source('performance_metrics.r')

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

#keep complete cases
train_features <- train_features[complete.cases(train_features)]
test_features <- test_features[complete.cases(test_features)]

# data classes
trainclass <- train_features$Match_Result
testclass <- test_features$Match_Result

# NA's and nonnumeric columns omitted
features = features[complete.cases(features)]
features = features[,-c('matchId','leagueId','Match_Date',"Match_Result")]

# standardization is required for k-means
scaled_features = scale(features)

##############################################################################################
#######################K-MEANS CLUSTERING#####################################################
a = matrix(ncol=2,nrow=10)
colnames(a) = c("k","withinSSE")

for (k in 2:10){
clusters = kmeans(scaled_features,k,nstart = 10)
a[k,1] = k
a[k,2] = clusters$tot.withinss
}

plot(a[,1],a[,2],xlab = "k", ylab = "within SSE") #to decide k number

##############################################################################################
clusters = kmeans(scaled_features,6,nstart = 20)
features = cbind(features,clusters$cluster)

# divide data based on the provided dates (by converting unix time)
train_features=features[Unix_Date>=as.numeric(as.POSIXct.Date(trainStart)) & Unix_Date<as.numeric(as.POSIXct.Date(testStart))] 
test_features=features[Unix_Date>=as.numeric(as.POSIXct.Date(testStart))] 

#Results as numeric values
trainclass <- (trainclass == "Home")*1 + (trainclass == "Away")*2
testclass <- (testclass == "Home")*1 + (testclass == "Away")*2

results <- matrix(1:(length(testclass)*3), 3)
results[1,] <- (testclass == 0)*1
results[2,] <- (testclass == 1)*1
results[3,] <- (testclass == 2)*1

#Inputs are scaled
train <- scale(train_features)
test <- scale(test_features)

cols <- c(1:3,sample(3:100,6),106)

#Model inputs determined
train <- train[,cols]
test <- test[,cols]

train = as.data.frame(train)
test = as.data.frame(test)

library(nnet)
#Model is generated
multinomModel <- multinom(trainclass ~ ., data=train)
summary (multinomModel)

#Probabilities for each class and the results are generated
predicted_scores <- predict (multinomModel, test, "probs") # predict on new data
predicted_class <- predict (multinomModel, test)

#Confusion matrix and accuracy
confusion_matrix_multinom <- table(predicted_class, testclass)
accuracy_multinom <- sum(predicted_class == testclass)/length(testclass)
accuracy_multinom

##  average RPS and RPS Matrix
RPS2 <- RPS_single(t(predicted_scores), results)
RPS2
