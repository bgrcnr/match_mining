setwd("C:/Users/gulsa/Desktop/2018-2019 Güz/IE582/fall18-gulsahakcakir/files")
library(data.table)
library("dataPreparation")
##### preparation of datasets ######
dataset1 <- read.table(file="sick.csv", header=TRUE, sep=";",na.strings = "NA")

dataset1 <- dataset1[complete.cases(dataset1),]
dataset1$sex <- as.numeric(dataset1$sex)
dataset1$FTI <- as.numeric(dataset1$FTI)
dataset1$referral_source <- as.numeric(dataset1$referral_source)
dataset1$Class <- as.numeric(dataset1$Class)
dataset1[dataset1$Class == 1,]$Class = 0
dataset1[dataset1$Class == 2,]$Class = 1
###########################################################################################
dataset2 <- read.table(file="Absenteeism_at_work.csv", header=TRUE, sep=";",na.strings = "")
###########################################################################################
dataset3 <- read.table(file="spam.csv", header=T, sep=",")
dataset3 <- dataset3[,-1]
###########################################################################################
dataset4 <- read.table(file="coil2000.csv", header= T,  sep=",")

####### partition of train & test instances #######

train_index <- sample(1:nrow(dataset3), 0.75 * nrow(dataset3))
test_index <- setdiff(1:nrow(dataset3), train_index)

traindata <- dataset3[train_index, -(ncol(dataset3))]
trainlabel <- dataset3[train_index, "Class"]

testdata <- dataset3[test_index, -(ncol(dataset3))]
testlabel <- dataset3[test_index, "Class"]

##############################################################
require(lqa)

lambda=exp (seq (-3, 3, length = 6))
print(lambda) #check what they are
lambdas=list(lambda,1)

traindata_lqa <- traindata[,-(ncol(traindata))]
trainlabel_lqa <- trainlabel
for (i in 1:length(trainlabel)){
  if (trainlabel[i] == "X0")
    trainlabel_lqa[i] = 0
  else
    trainlabel_lqa[i] = 1
}

trainlabel_lqa <- as.numeric(trainlabel_lqa)

cvFused=cv.lqa(trainlabel_lqa,traindata_lqa,lambda.candidates = lambdas, intercept = FALSE,
               family=binomial(), penalty.family=fused.lasso ,n.fold=10,loss.func = "aic.loss")


cvFused$lambda.opt
mod_lqa <- cvFused$best.obj

traindata_lqa <- as.matrix(traindata_lqa)
trainlabel_lqa <- as.matrix(trainlabel_lqa)
testdata_lqa <- as.matrix(testdata)
model_lqa <- lqa(trainlabel_lqa~traindata_lqa,family=binomial(),penalty=fused.lasso(cvFused$lambda.opt))
predict_lqa <- predict.lqa(model_lqa, new.x = testdata_lqa)
predicted_class <- as.factor(make.names(prediction_lqa$lqa.obj$y))
conf_lqa <-confusionMatrix(predicted_class, testlabel)
print(conf_lqa)

##################################################################################################################
require(rpart)
control <- trainControl(method="cv", number=10, search="grid")
minbucket = c(2,3,5)
Grid <- expand.grid(cp = c(0.001,0.003,0.005,0.01,0.05,0.1)) #minbucket için bir döngü yazılabilir.

mod_dt_1 <- train(Class~., data=traindata, method="rpart", tuneGrid=Grid, trControl=control, minbucket=minbucket[1])
mod_dt_2 <- train(Class~., data=traindata, method="rpart", tuneGrid=Grid, trControl=control, minbucket=minbucket[2])
mod_dt_3 <- train(Class~., data=traindata, method="rpart", tuneGrid=Grid, trControl=control, minbucket=minbucket[3])

mod_dt_1$results
mod_dt_2$results
mod_dt_3$results

predict_dt <- predict(mod_dt_1,testdata)
conf_dt <-confusionMatrix(predict_dt, testlabel)
print(conf_dt)
##################################################################################################################
library(caret)
library(kernlab)
trainlabel <- as.factor(trainlabel)
trainlabel <- make.names(trainlabel)
testlabel <- make.names(testlabel)
testlabel <- as.factor(testlabel)
traindata$Class <- trainlabel
ctrl <- trainControl(method = "cv", number= 10, savePred=T)
Grid_Poly <- expand.grid(degree=c(1:3),scale=1,C=10^c(-1:1))
Grid_Rad <- expand.grid(.C = c(0.01,0.1,1, 10), .sigma = c(0.01,0.02,0.05) )
mod_polynomial <- train(Class~., data=traindata, method="svmPoly", trControl = ctrl, 
                        tuneGrid = Grid_Poly,
                        preProcess=c("scale","center"),
                        na.action = na.omit)
mod_gaussian <- train(Class~., data=traindata, method="svmRadial", trControl = ctrl, tuneGrid = Grid_Rad)
head(mod$pred)
mod_polynomial
mod_gaussian
plot(mod_gaussian)

predict_gaussian <-predict(mod_gaussian, testdata, na.action = na.pass)
conf_ga <-confusionMatrix(predict_gaussian, testlabel)
print(conf_ga)

predict_polynomial <-predict(mod_polynomial, testdata, na.action = na.pass)
conf_pol <-confusionMatrix(predict_polynomial, testlabel)
print(conf_pol)
###################################################################################################################
library(randomForest)
control <- trainControl(method="cv", number=10, search="grid")
tunegrid <- expand.grid(.mtry=c(5:10))
mod_rf <- train(Class~., data=traindata, method="rf", tuneGrid=tunegrid, trControl=control, ntree=500,nodesize=5)
print(mod_rf)
plot(mod_rf)

predict_rf <- predict(mod_rf, testdata)
conf_rf <-confusionMatrix(predict_rf, testlabel)
print(conf_rf)
####################################################################################################################
library(gbm)
library(plyr)
control <- trainControl(method="cv", number=3, search="grid")
grid <- expand.grid(n.trees = seq(300,500,100),
                        interaction.depth = seq(3,5),
                        shrinkage = c(0.5,0.1,0.05),
                        n.minobsinnode =10)
mod_gbm <- train(Class~., data=traindata, method="gbm", tuneGrid=grid, trControl=control)
print(mod_gbm)
plot(mod_gbm) #limitleri düzenle

predict_gbm <- predict(mod_gbm, testdata)
conf_gbm <-confusionMatrix(predict_gbm, testlabel)
print(conf_gbm)
