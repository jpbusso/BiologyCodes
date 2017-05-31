##########################################################################################################
## Loading Files and Packages
##########################################################################################################
## Load libraries
library(Hmisc)
library(caret)
library(plyr)
library(geomorph)
library(nnet)
library(CVST)
library(kernlab)
library(pROC)
library(h2o)

## Clear Rs brain
rm(list=ls())

## Read the dataset for Wings
dat <- read.csv("D:/My documents/University/Zuerich/PhD Thesis/Wing Species ML/MEGA wing data set.csv")

## Employ a new dataset to avoid having to load the original again
datSepsis<-dat
datSepsis$species<-factor(datSepsis$species)

## Create a DataSet only with the landmarks
landDat<-dat[,c(4:33)]
# str(landDat)

#### THIS PROCRUSTES METHOD IN R DOES NOT WORK VERY WELL
# ## Procrustes Superimposition
# landArray <- arrayspecs(landDat, 15, 2)
# procDat<-gpagen(landArray,PrinAxes = FALSE)
# 
# ## Plot Data
# plotAllSpecimens(procDat$coords,links=procDat$links)

#### EXPORT DATA TO MORPHOJ AND ALIGN WINGS THERE
# write.csv(landDat,"D:/My documents/University/Zuerich/PhD Thesis/Wing Species ML/MEGA wing data set.csv")

### IMPORT data from MORPHOJ
datProc<-read.csv("D:/My documents/University/Zuerich/PhD Thesis/Wing Species ML/procDataAll.csv")

## Remove the ID variable
datProc1<-datProc[,-c(1)]
## Convert Data to array
landArray <- arrayspecs(datProc1, 15, 2)
## Plot Data
plotAllSpecimens(landArray)

## Response Vars for Neural Net (logistic variables with each species name)
for (i in levels(datSepsis$species)){
  datSepsis[i]<-datSepsis$species == i
}

## Add Procrustes Data to Original Data
dat1<-cbind(datSepsis,datProc1)

str(dat1)

## Create the K-folds to train and test the data
## Subset the data per individual because the data from each individual is correlated
## I want a model that predicts for other users, not only the ones I have
## Hence, fitting to some sellers and testing on others will do that job
folds = 5
foldSamples = createFolds(1:nrow(dat1), k = folds, list = TRUE, returnTrain = FALSE)
str(foldSamples)

## This i values is to check that the loops work properly
i = 3


##############################################################################
## Fit Neural Network to Data #####
##############################################################################
## DO NOT SCALE X for NN
## Set the Data Frame where the model metrics will be stored
NNModelAccu = data.frame()

for (i in 1:folds)
{
  ## Subset data into the training and testing DATA
  testing = dat1[foldSamples[[i]],]
  training = dat1[-foldSamples[[i]],]
  ## FIT a NN
  NNFit<-nnet(x = training[,c(49:78)], y = training[,c(41:48)], 
              size = 11,
              rang = 0.5,
              decay = 5e-4,
              maxit = 800,
              linout = FALSE, 
              ##censored = TRUE,
              trace = TRUE)
  
  ## Random Data for comparing model performance
  testing$predYRandom<-sample(dat1$species, nrow(testing), replace=TRUE)
  ## Obtain the Confusion Matrix
  testConfMRandom<-confusionMatrix(data = testing$predYRandom, reference = testing$species,
                                   mode = "everything")
  
  
  ## Predict the testing results
  # predTestNN<-predict(object = NNFit, newdata = testing[,c(49:78)])
  predTestNN<-predict(object = NNFit, newdata = testing[,c(49:78)])
  predTestNNSp<-factor(max.col(predTestNN))
  ## Change the number values to species
  predTestNNSp<-factor(predTestNNSp, levels = seq(1,nlevels(testing$species)), 
                       labels = levels(testing$species))
  testing$predNNSp<-predTestNNSp
  ## Confusion Matrix for Testing (Statistics)
  testingResults<-confusionMatrix(data = testing$predNNSp, reference = testing$species,
                                  mode = "everything")
  
  ## Get the model performance values in the table (Accuracy and F1) for both random and testing
  tempTrainAccu = data.frame( foldNum = i, Random_F1 = mean(testConfMRandom$byClass[,"F1"]),
                              Random_Accuracy = testConfMRandom$overall["Accuracy"],
                              testing_F1 = mean(testingResults$byClass[,"F1"]),
                              testing_Accuracy = testingResults$overall["Accuracy"])
  ## Add to the final Table
  NNModelAccu = rbind(tempTrainAccu,NNModelAccu)
}

## Calcuate means of the k models for the performance measurements
sapply(NNModelAccu, mean, na.rm=TRUE)

##############################################################################
## Get the Accuracy vs NN Units Graph--- based on this 11 Units are enough #####
##############################################################################
## Since this requires training a lot of models, instead of the k-fold approach
## which is really good but requires me to test the data 5 times for each value
## I'll use the the simple partition idea. The error should be more or less
## reflected in the consecutive variation along the x axis.

##Create Data Set
errorUnitsTable<-data.frame()

## How many units will we evaluate?
unitsList<-c(1:26)

##Loop through all the units
for (i in unitsList){
  ## subset the Dataset
  inTrain<-createDataPartition( y = dat1$species,
                                p = .75,
                                list = FALSE)
  
  ## Set training and Testing
  training <- dat1[inTrain,]
  testing <- dat1[-inTrain,]
  
  ## Train the NN
  NNFit<-nnet(x = training[,c(49:78)], y = training[,c(41:48)], 
              size = i,
              rang = 0.5,
              decay = 5e-4,
              maxit = 500,
              linout = FALSE,
              # softmax = TRUE,
              trace = TRUE)
  
  ## Training Error
  predNN<-predict(object = NNFit, newdata = training[,c(49:78)])
  predNNSp<-factor(max.col(predNN))
  ## Change the number values to species
  predNNSp<-factor(predNNSp, levels = seq(1,nlevels(training$species)), 
                   labels = levels(training$species))
  training$predNNSp<-predNNSp
  training$modelNNError<-training$species==training$predNNSp
  ## Calculate the training error
  trainingError<-sum(training$modelNNError)/nrow(training)
  
  ## Testing Error
  predTestNN<-predict(object = NNFit, newdata = testing[,c(49:78)])
  predTestNNSp<-factor(max.col(predTestNN))
  ## Change the number values to species
  predTestNNSp<-factor(predTestNNSp, levels = seq(1,nlevels(testing$species)), 
                       labels = levels(testing$species))
  testing$predNNSp<-predTestNNSp
  testing$modelNNError<-testing$species==testing$predNNSp
  ## Calculate the testing error
  testingError<-sum(testing$modelNNError)/nrow(testing)
  ## Place Error values on Error Table
  errorTable<-data.frame(unitsNN = i, TrainingAccuracy = trainingError, TestingAccuracy = testingError)
  errorUnitsTable<-rbind(errorUnitsTable,errorTable)
}

## Graph the Training and Testing Errors vs the Units
ggplot()+
  geom_point(data = errorUnitsTable, aes(x = unitsNN, y = TrainingAccuracy), color = "red", size = 2)+
  stat_smooth(data = errorUnitsTable, aes(x = unitsNN, y = TrainingAccuracy), color = "red")+
  geom_point(data = errorUnitsTable, aes(x = unitsNN, y = TestingAccuracy), color = "darkgreen", size = 2)+
  stat_smooth(data = errorUnitsTable, aes(x = unitsNN, y = TestingAccuracy), color = "darkgreen")+
  geom_text(data = errorUnitsTable, aes(x = unitsNN, y = TestingAccuracy, label = unitsNN))

##############################################################################
## Get the Accuracy vs SampleSize Graph--- based on this SAMPLES are enough #####
##############################################################################
##Create Data Set
errorSampleTable<-data.frame()

## How many units will we evaluate?
sizeList<-seq(10,(nrow(dat1)*.75),20)

##Loop through all the units
for (i in sizeList){
  df<-dat1[sample(nrow(dat1), i), ]
  ## subset the Dataset
  inTrain<-createDataPartition( y = df$species,
                                p = .75,
                                list = FALSE)
  
  ## Set training and Testing
  training <- df[inTrain,]
  testing <- df[-inTrain,]
  
  ## Train the NN
  NNFit<-nnet(x = training[,c(49:78)], y = training[,c(41:48)], 
              size = 11,
              rang = 0.5,
              decay = 5e-4,
              maxit = 500,
              linout = FALSE,
              # softmax = TRUE,
              trace = TRUE)
  
  ## Training Error
  predNN<-predict(object = NNFit, newdata = training[,c(49:78)])
  predNNSp<-factor(max.col(predNN))
  ## Change the number values to species
  predNNSp<-factor(predNNSp, levels = seq(1,nlevels(training$species)), 
                   labels = levels(training$species))
  training$predNNSp<-predNNSp
  training$modelNNError<-training$species==training$predNNSp
  ## Calculate the training error
  trainingError<-sum(training$modelNNError)/nrow(training)
  
  ## Testing Error
  predTestNN<-predict(object = NNFit, newdata = testing[,c(49:78)])
  predTestNNSp<-factor(max.col(predTestNN))
  ## Change the number values to species
  predTestNNSp<-factor(predTestNNSp, levels = seq(1,nlevels(testing$species)), 
                       labels = levels(testing$species))
  
  testing$predNNSp<-predTestNNSp
  testing$modelNNError<-testing$species==testing$predNNSp
  ## Calculate the testing error
  testingError<-sum(testing$modelNNError)/nrow(testing)
  ## Place Error values on Error Table
  errorTable<-data.frame(sampleSize = i, TrainingAccuracy = trainingError, TestingAccuracy = testingError)
  errorSampleTable<-rbind(errorSampleTable,errorTable)
}

## Graph the Training and Testing Errors vs the Sample Size
ggplot()+
  geom_point(data = errorSampleTable, aes(x = sampleSize, y = TrainingAccuracy), color = "red", size = 2)+
  stat_smooth(data = errorSampleTable, aes(x = sampleSize, y = TrainingAccuracy), color = "red")+
  geom_point(data = errorSampleTable, aes(x = sampleSize, y = TestingAccuracy), color = "darkgreen", size = 2)+
  stat_smooth(data = errorSampleTable, aes(x = sampleSize, y = TestingAccuracy), color = "darkgreen")

##############################################################################
## Get the Accuracy vs lambda Graph--- based on this 5e-4 is good #####
##############################################################################
##Create Data Set
errorLambdaTable<-data.frame()

## How many units will we evaluate?
lambdaList<-seq(2e-5,(2e-4),2e-5)

##Loop through all the units
for (i in lambdaList){
  ## subset the Dataset
  inTrain<-createDataPartition( y = dat1$species,
                                p = .75,
                                list = FALSE)
  
  ## Set training and Testing
  training <- dat1[inTrain,]
  testing <- dat1[-inTrain,]
  
  ## Train the NN
  NNFit<-nnet(x = training[,c(49:78)], y = training[,c(41:48)], 
              size = 11,
              rang = 0.5,
              decay = i,
              maxit = 500,
              linout = FALSE,
              # softmax = TRUE,
              trace = TRUE)
  
  ## Training Error
  predNN<-predict(object = NNFit, newdata = training[,c(49:78)])
  predNNSp<-factor(max.col(predNN))
  ## Change the number values to species
  predNNSp<-factor(predNNSp, levels = seq(1,nlevels(training$species)), 
                   labels = levels(training$species))
  training$predNNSp<-predNNSp
  training$modelNNError<-training$species==training$predNNSp
  ## Calculate the training error
  trainingError<-sum(training$modelNNError)/nrow(training)
  
  ## Testing Error
  predTestNN<-predict(object = NNFit, newdata = testing[,c(49:78)])
  predTestNNSp<-factor(max.col(predTestNN))
  ## Change the number values to species
  predTestNNSp<-factor(predTestNNSp, levels = seq(1,nlevels(testing$species)), 
                       labels = levels(testing$species))
  testing$predNNSp<-predTestNNSp
  testing$modelNNError<-testing$species==testing$predNNSp
  ## Calculate the testing error
  testingError<-sum(testing$modelNNError)/nrow(testing)
  ## Place Error values on Error Table
  errorTable<-data.frame(lambdaSize = i, TrainingAccuracy = trainingError, TestingAccuracy = testingError)
  errorLambdaTable<-rbind(errorLambdaTable,errorTable)
}

## Graph the Training and Testing Errors vs the Lambda Value
ggplot()+
  geom_point(data = errorLambdaTable, aes(x = lambdaSize, y = TrainingAccuracy), color = "red", size = 2)+
  stat_smooth(data = errorLambdaTable, aes(x = lambdaSize, y = TrainingAccuracy), color = "red")+
  geom_point(data = errorLambdaTable, aes(x = lambdaSize, y = TestingAccuracy), color = "darkgreen", size = 2)+
  stat_smooth(data = errorLambdaTable, aes(x = lambdaSize, y = TestingAccuracy), color = "darkgreen")

##############################################################################
## Get the Accuracy vs maxit Graph--- based on this 800 is good #####
##############################################################################
##Create Data Set
errorMaxitdaTable<-data.frame()

## How many units will we evaluate?
maxitList<-seq(100,1200,100)

##Loop through all the units
for (i in maxitList){
  ## subset the Dataset
  inTrain<-createDataPartition( y = dat1$species,
                                p = .75,
                                list = FALSE)
  
  ## Set training and Testing
  training <- dat1[inTrain,]
  testing <- dat1[-inTrain,]
  
  ## Train the NN
  NNFit<-nnet(x = training[,c(49:78)], y = training[,c(41:48)], 
              size = 11,
              rang = 0.5,
              decay = 5e-4,
              maxit = i,
              linout = FALSE,
              # softmax = TRUE,
              trace = TRUE)
  
  ## Training Error
  predNN<-predict(object = NNFit, newdata = training[,c(49:78)])
  predNNSp<-factor(max.col(predNN))
  ## Change the number values to species
  predNNSp<-factor(predNNSp, levels = seq(1,nlevels(training$species)), 
                   labels = levels(training$species))
  training$predNNSp<-predNNSp
  training$modelNNError<-training$species==training$predNNSp
  ## Calculate the training error
  trainingError<-sum(training$modelNNError)/nrow(training)
  
  ## Testing Error
  predTestNN<-predict(object = NNFit, newdata = testing[,c(49:78)])
  predTestNNSp<-factor(max.col(predTestNN))
  ## Change the number values to species
  predTestNNSp<-factor(predTestNNSp, levels = seq(1,nlevels(testing$species)), 
                       labels = levels(testing$species))
  testing$predNNSp<-predTestNNSp
  testing$modelNNError<-testing$species==testing$predNNSp
  ## Calculate the testing error
  testingError<-sum(testing$modelNNError)/nrow(testing)
  ## Place Error values on Error Table
  errorTable<-data.frame(maxitSize = i, TrainingAccuracy = trainingError, TestingAccuracy = testingError)
  errorMaxitdaTable<-rbind(errorMaxitdaTable,errorTable)
}

## Graph the Training and Testing Errors vs the number of Iterations
ggplot()+
  geom_point(data = errorMaxitdaTable, aes(x = maxitSize, y = TrainingAccuracy), color = "red", size = 2)+
  stat_smooth(data = errorMaxitdaTable, aes(x = maxitSize, y = TrainingAccuracy), color = "red")+
  geom_point(data = errorMaxitdaTable, aes(x = maxitSize, y = TestingAccuracy), color = "darkgreen", size = 2)+
  stat_smooth(data = errorMaxitdaTable, aes(x = maxitSize, y = TestingAccuracy), color = "darkgreen")
##############################################################################
## FIT Deep NN to the data #####
##############################################################################
## Deep Neural Network
h2o.init(nthreads = -1)
## Partition the data
inTrain<-createDataPartition( y = dat1$species,
                              p = .75,
                              list = FALSE)

training <- dat1[inTrain,]
testing <- dat1[-inTrain,]


## Check partition of data
prop.table(table(training$species))
prop.table(table(testing$species))

##Fit Deep neural network
dnnModel <- h2o.deeplearning(x = c(49:78), y = c("species"), training_frame = as.h2o(training),
                             hidden = c(9,9), 
                             epochs = 100, ## times that data set is iterated
                             ##rho = 5e-4, ## decay of the learning rate
                             l1 = 0.0, ## regularization
                             train_samples_per_iteration = -1, ## 0:  one epoch, -1:  all available data (e.g., replicated training data), -2:automatic
                             variable_importances = TRUE, 
                             score_each_iteration = TRUE)

## Predict the classes for training
predDNN<- h2o.predict(dnnModel, as.h2o(training))
elephant<-as.data.frame(predDNN)
training$predDNNSp<-elephant$predict

## Confusion Matrix for Traning (Statistics)
trainingConf<-confusionMatrix(data = training$predDNNSp, reference = training$species,
                              mode = "everything")

## Predict the classes for testing
predDNN<- h2o.predict(dnnModel, as.h2o(testing))
giraffe<-as.data.frame(predDNN)
testing$predDNNSp<-giraffe$predict

## Confusion Matrix for Traning (Statistics)
confusionMatrix(data = testing$predDNNSp, reference = testing$species,
                mode = "everything")

h2o.shutdown()

##############################################################################
## Get the Accuracy vs DNN Units Graph--- based on this 10 Units are enough ####
##############################################################################
##Create Data Set
accuUnitsTable<-data.frame()

## Set the range of Units
nnUnitsList<-seq(10,100,5)

for (i in nnUnitsList){
  ## Partition the data
  inTrain<-createDataPartition( y = dat1$species,
                                p = .75,
                                list = FALSE)
  
  training <- dat1[inTrain,]
  testing <- dat1[-inTrain,]
  
  
  ## Check partition of data
  prop.table(table(training$species))
  prop.table(table(testing$species))
  
  ##Fit Deep neural network
  dnnModel <- h2o.deeplearning(x = c(49:78), y = c("species"), training_frame = as.h2o(training),
                               hidden = c(i,i,i), 
                               epochs = 100, ## times that data set is iterated
                               ##rho = 5e-4, ## decay of the learning rate
                               l1 = 0.0, ## regularization
                               train_samples_per_iteration = -2, ## 0:  one epoch, -1:  all available data (e.g., replicated training data), -2:automatic
                               variable_importances = TRUE, 
                               score_each_iteration = TRUE)
  
  ## Predict the classes for training
  predDNN<- h2o.predict(dnnModel, as.h2o(training))
  elephant<-as.data.frame(predDNN)
  training$predDNNSp<-elephant$predict
  
  ## Confusion Matrix for Traning (Statistics)
  trainConf<-confusionMatrix(data = training$predDNNSp, reference = training$species,
                             mode = "everything")
  
  trainingAccu<-mean(trainConf$byClass[,"Balanced Accuracy"])
  
  ## Predict the classes for testing
  predDNN<- h2o.predict(dnnModel, as.h2o(testing))
  giraffe<-as.data.frame(predDNN)
  testing$predDNNSp<-giraffe$predict
  
  ## Confusion Matrix for Traning (Statistics)
  testConf<-confusionMatrix(data = testing$predDNNSp, reference = testing$species,
                            mode = "everything")
  testingAccu<-mean(testConf$byClass[,"Balanced Accuracy"])
  
  ## Place Error values on Error Table
  accuTable<-data.frame(unitsNN = i, TrainingAccuracy = trainingAccu, TestingAccuracy = testingAccu)
  accuUnitsTable<-rbind(accuUnitsTable,accuTable)
  
}


## Graph the Training and Testing Errors vs the Units in the DNN
ggplot()+
  geom_point(data = accuUnitsTable, aes(x = unitsNN, y = TrainingAccuracy), color = "red", size = 2)+
  stat_smooth(data = accuUnitsTable, aes(x = unitsNN, y = TrainingAccuracy), color = "red")+
  geom_point(data = accuUnitsTable, aes(x = unitsNN, y = TestingAccuracy), color = "darkgreen", size = 2)+
  stat_smooth(data = accuUnitsTable, aes(x = unitsNN, y = TestingAccuracy), color = "darkgreen")
##############################################################################
## FIT Multiclass logistic regression to data #####
##############################################################################
## Get the names of the vars and interactions to fit the model
penguin<-names(training[,c(49:78)])

varNames = as.character()
garlic = 0
onion = 1
for (i in penguin){
  garlic = garlic + 1
  onion = onion + 1
  interName = paste(penguin[garlic],penguin[onion],sep = ":")
  
  varNames = paste(i,varNames, sep = " + ")
  varNames = paste(varNames,interName,   sep = " + ")
}

## Get the names of the vars and NO interactions to fit the model
penguin<-names(training[,c(49:78)])

varNames = as.character()
garlic = 0
onion = 1
for (i in penguin){
  varNames = paste(i,varNames, sep = " + ")
}


## Fit the MULTINOMIAL LOGICSTIC MODELS with interactions
mlogisticModel<-multinom(species~ProcCoord30 + ProcCoord29 + ProcCoord28 + ProcCoord27 + ProcCoord26 + ProcCoord25 + ProcCoord24 + ProcCoord23 + ProcCoord22 + ProcCoord21 + ProcCoord20 + ProcCoord19 + ProcCoord18 + ProcCoord17 + ProcCoord16 + ProcCoord15 + ProcCoord14 + ProcCoord13 + ProcCoord12 + ProcCoord11 + ProcCoord10 + ProcCoord9 + ProcCoord8 + ProcCoord7 + ProcCoord6 + ProcCoord5 + ProcCoord4 + ProcCoord3 + ProcCoord2 + ProcCoord1 +  + ProcCoord1:ProcCoord2 + ProcCoord2:ProcCoord3 + ProcCoord3:ProcCoord4 + ProcCoord4:ProcCoord5 + ProcCoord5:ProcCoord6 + ProcCoord6:ProcCoord7 + ProcCoord7:ProcCoord8 + ProcCoord8:ProcCoord9 + ProcCoord9:ProcCoord10 + ProcCoord10:ProcCoord11 + ProcCoord11:ProcCoord12 + ProcCoord12:ProcCoord13 + ProcCoord13:ProcCoord14 + ProcCoord14:ProcCoord15 + ProcCoord15:ProcCoord16 + ProcCoord16:ProcCoord17 + ProcCoord17:ProcCoord18 + ProcCoord18:ProcCoord19 + ProcCoord19:ProcCoord20 + ProcCoord20:ProcCoord21 + ProcCoord21:ProcCoord22 + ProcCoord22:ProcCoord23 + ProcCoord23:ProcCoord24 + ProcCoord24:ProcCoord25 + ProcCoord25:ProcCoord26 + ProcCoord26:ProcCoord27 + ProcCoord27:ProcCoord28 + ProcCoord28:ProcCoord29 + ProcCoord29:ProcCoord30,
                         data = training,
                         maxit = 800)

## Fit the MULTINOMIAL LOGICSTIC MODELS with NO interactions
mlogisticModel<-multinom(species~ProcCoord30 + ProcCoord29 + ProcCoord28 + ProcCoord27 + ProcCoord26 + ProcCoord25 + ProcCoord24 + ProcCoord23 + ProcCoord22 + ProcCoord21 + ProcCoord20 + ProcCoord19 + ProcCoord18 + ProcCoord17 + ProcCoord16 + ProcCoord15 + ProcCoord14 + ProcCoord13 + ProcCoord12 + ProcCoord11 + ProcCoord10 + ProcCoord9 + ProcCoord8 + ProcCoord7 + ProcCoord6 + ProcCoord5 + ProcCoord4 + ProcCoord3 + ProcCoord2 + ProcCoord1,
                         data = training,
                         maxit = 800)


## Predict the training error
training$predMLModel<-predict(object = mlogisticModel, newdata = training[,c(49:78)])
## Confusion Matrix for Traning (Statistics)
confusionMatrix(data = training$predMLModel, reference = training$species,
                mode = "everything")

## Predict the testing results
testing$predMLModel<-predict(object = mlogisticModel, newdata = testing[,c(49:78)])
## Confusion Matrix for Testing (Statistics)
testingResults<-confusionMatrix(data = testing$predMLModel, reference = testing$species,
                                mode = "everything")
testingResults

## In my case this model was not very promising, so there was no need to proceed
## but using as example the previous models, one can continue the code to evaluate the parameters

##############################################################################
##############################################################################
## One Vs All Kernel Logistic Regression #####
##############################################################################
## Set the predicting function
predicKLR<-function (klogreg, newData) 
{
  K = kernlab::kernelMult(klogreg$kernel, newData, klogreg$data, klogreg$alpha)
  pi = 1/(1 + exp(-as.vector(K)))
  return(pi)
}

## New Copy of dataSet that will get modified afterwards, to avoid changing dat1
datKLR<-dat1
for (i in c(41:48)){
  datKLR[[i]]<-as.factor(datKLR[[i]])  
}
str(datKLR)

modelsKLR<-list()
## Set the parameters for the KLR model
sigmaKLR<-5200
tolKLR=10e-6
maxiterKLR = 800

## The one vs All strategy requires that I repeat this process for each species
## Again since it has to be done a few times, the k-fold testing would be the best
## but to save time and processing power, I'll just use the simple test of partition
## into testing and training
## The error between the different species should represent the error between species
## that is what I assume.
for (i in levels(datSepsis$species)){
  ## Partition the data
  inTrain<-createDataPartition( y = datKLR$species,
                                p = .75,
                                list = FALSE)
  
  training <- datKLR[inTrain,]
  testing <- datKLR[-inTrain,]
  
  ## Check partition of data
  prop.table(table(training$species))
  prop.table(table(testing$species))
  
  ## Set the xMatrix for the Kernel Model
  xData = as.matrix(training[,c(49:78)])
  
  ## Structure data in CVST format
  dFrame = constructData(x=xData, y=(training[[i]]))
  ## Build the base learner
  klr = constructKlogRegLearner()
  p = list(kernel="rbfdot", sigma=sigmaKLR, lambda=.1/nrow(xData), tol=tolKLR, maxiter=maxiterKLR)
  modelsKLR[[i]] = klr$learn(data = dFrame, params =  p)
}

## Random Data for comparing model performance
testing$predYRandom<-sample(dat1$species, nrow(testing), replace=TRUE)
## Obtain the Confusion Matrix
testConfMRandom<-confusionMatrix(data = testing$predYRandom, reference = testing$species,
                                 mode = "everything")

## Predict Testing Data
## Set the xMatrix for the Kernel Model
xData = as.matrix(testing[,c(49:78)])
for (i in levels(datSepsis$species)){
  testing[paste(i,"Pred")] = predicKLR(klogreg = modelsKLR[[i]], newData = xData)
}

predictedFlies<-testing[,c(79:86)]
predTestKLRSp<-factor(max.col(predictedFlies))
## Change the number values to species
predTestKLRSp<-factor(predTestKLRSp, levels = seq(1,nlevels(testing$species)), 
                      labels = levels(testing$species))
testing$predKLRSp<-predTestKLRSp
## Confusion Matrix for testing (Statistics)
testingResults<-confusionMatrix(data = testing$predKLRSp, reference = testing$species,
                                mode = "everything")

testingResults$table
mean(testingResults$byClass[,"F1"])
##############################################################################
## One Vs All Kernel Logistic Regression sigma = 5200 optimal #####
##############################################################################
klrSigmaData = data.frame()

sigmaList<-seq(4000,6000,100)

for (i in sigmaList){
  modelsKLR<-list()
  sigmaKLR<-i
  tolKLR=10e-6
  maxiterKLR = 800
  
  for (i in levels(datSepsis$species)){
    ## Partition the data
    inTrain<-createDataPartition( y = datKLR$species,
                                  p = .75,
                                  list = FALSE)
    
    training <- datKLR[inTrain,]
    testing <- datKLR[-inTrain,]
    
    ## Check partition of data
    prop.table(table(training$species))
    prop.table(table(testing$species))
    
    ## Set the xMatrix for the Kernel Model
    xData = as.matrix(training[,c(49:78)])
    
    ## Structure data in CVST format
    dFrame = constructData(x=xData, y=(training[[i]]))
    ## Build the base learner
    klr = constructKlogRegLearner()
    p = list(kernel="rbfdot", sigma=sigmaKLR, lambda=.1/nrow(xData), tol=tolKLR, maxiter=maxiterKLR)
    modelsKLR[[i]] = klr$learn(data = dFrame, params =  p)
  }
  
  ## Predict Training Data
  ## Set the xMatrix for the Kernel Model
  xData = as.matrix(training[,c(49:78)])
  for (i in levels(datSepsis$species)){
    training[paste(i,"Pred")] = predicKLR(klogreg = modelsKLR[[i]], newData = xData)
  }
  
  predictedFlies<-training[,c(79:86)]
  predTrainKLRSp<-factor(max.col(predictedFlies))
  ## Change the number values to species
  predTrainKLRSp<-factor(predTrainKLRSp,levels = seq(1,nlevels(training$species)), 
                         labels = levels(training$species))
  training$predKLRSp<-predTrainKLRSp
  ## Confusion Matrix for training (Statistics)
  trainingResults<-confusionMatrix(data = training$predKLRSp, reference = training$species,
                                   mode = "everything")
  
  trainingResults$table
  trainingF1 = mean(trainingResults$byClass[,"F1"])
  
  ## Predict Testing Data
  ## Set the xMatrix for the Kernel Model
  xData = as.matrix(testing[,c(49:78)])
  for (i in levels(datSepsis$species)){
    testing[paste(i,"Pred")] = predicKLR(klogreg = modelsKLR[[i]], newData = xData)
  }
  
  predictedFlies<-testing[,c(79:86)]
  predTestKLRSp<-factor(max.col(predictedFlies))
  ## Change the number values to species
  predTestKLRSp<-factor(predTestKLRSp, levels = seq(1,nlevels(testing$species)), 
                        labels = levels(testing$species))
  testing$predKLRSp<-predTestKLRSp
  ## Confusion Matrix for testing (Statistics)
  testingResults<-confusionMatrix(data = testing$predKLRSp, reference = testing$species,
                                  mode = "everything")
  
  testingResults$table
  testingF1 = mean(testingResults$byClass[,"F1"])
  
  
  klrSigmaData1 <-data.frame (sigma = sigmaKLR, trainF1 = trainingF1, testF1 = testingF1)
  
  klrSigmaData = rbind(klrSigmaData,klrSigmaData1)
}

## Graph the Training and Testing Errors vs the Units
ggplot()+
  geom_point(data = klrSigmaData, aes(x = sigma, y = trainF1), color = "red", size = 2)+
  stat_smooth(data = klrSigmaData, aes(x = sigma, y = trainF1), color = "red")+
  geom_point(data = klrSigmaData, aes(x = sigma, y = testF1), color = "darkgreen", size = 2)+
  stat_smooth(data = klrSigmaData, aes(x = sigma, y = testF1), color = "darkgreen")