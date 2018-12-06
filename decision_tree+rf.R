require(data.table)
require(rpart)
require(rattle)

setwd("C:\\Users\\gulsa\\Desktop\\2018-2019 Güz\\IE582\\Project\\match_mining")
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

train_features <- train_features[complete.cases(train_features)]
test_features <- test_features[complete.cases(test_features)]

trainclass <- train_features$Match_Result
traindata <- train_features[,-c('matchId','leagueId','Match_Date',"Match_Result")]

testclass <- test_features$Match_Result
testdata <- test_features[,-c('matchId','leagueId','Match_Date',"Match_Result")]

trainclass <- (trainclass == "Home")*1 + (trainclass == "Away")*2
testclass <- (testclass == "Home")*1 + (testclass == "Away")*2

##################################################################################################
#####################DECISION TREE################################################################
tree1 = rpart(trainclass~.,traindata,
              method='class',
              control=rpart.control(cp=0.001,maxdepth = 30))

fancyRpartPlot(tree1)

printcp(tree1) # display the results 
plotcp(tree1) # visualize cross-validation results 
summary(tree1) # detailed summary of splits
which.min(tree1$cptable) #cp with min cv error

ptree1 = prune(tree1, cp=0.005) #prune to determined cp
fancyRpartPlot(ptree1)

probs = as.data.frame(predict(ptree1,testdata))

results <- matrix(ncol=3,nrow=nrow(testdata))
results[,1] <- (testclass == 0)*1
results[,2] <- (testclass == 1)*1
results[,3] <- (testclass == 2)*1

RPS1 = RPS_matrix(probs,results)
RPS1 = mean(RPS1)
RPS1

##################################################################################################################
cols <- c(1:3,sample(3:ncol(traindata), 5))

train1 <- traindata[,..cols]
test1 <- testdata[,..cols]

tree1 = rpart(trainclass~.,train1,
            method='class',
            control=rpart.control(cp=0.003,maxdepth = 30))

fancyRpartPlot(tree1)

printcp(tree1) # display the results 
plotcp(tree1) # visualize cross-validation results 
summary(tree1) # detailed summary of splits
which.min(tree1$cptable) #cp with min cv error

ptree1 = prune(tree1, cp=0.006) #prune to determined cp
fancyRpartPlot(ptree1)

probs1 = as.data.frame(predict(ptree1,test1))

results1 <- matrix(ncol=3,nrow=nrow(test1))
results1[,1] <- (testclass == 0)*1
results1[,2] <- (testclass == 1)*1
results1[,3] <- (testclass == 2)*1

RPS2 = RPS_matrix(probs1,results1)
RPS2 = mean(RPS2)
RPS2 


##################################################################################################
#####################RANDOM FOREST################################################################
library(randomForest)
rf = randomForest(train1, y=as.factor(trainclass),
                  ntree=500, proximity=T)
probs3 = predict(rf, test1, type='prob') #to obtain proportion of each classes
rf
importance(rf)
rf$confusion

RPS3 = RPS_matrix(probs3,results1)
RPS3 = mean(RPS3)
RPS3 
