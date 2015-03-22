 
library(ggplot2)
library(caret)
library(rattle)

testing <- read.csv("pml-testing.csv",na.strings = c("NA", "na", "NULL",
                                                     "null", ""))
training <- read.csv("pml-training.csv",na.strings = c("NA", "na", "NULL",
                                                      "null", ""))

itrain <- createDataPartition(training$classe,p=.99,list=FALSE)

train_test <- training[-itrain,]
train_train <- training[itrain,]


ii <- nearZeroVar(train_train,freqCut=70/30,uniqueCut=20)
t1 <- train_train[,-ii]
t1 <- t1[,colSums(is.na(t1))<1]
t1 <- t1[,c(-1,-2,-3,-4,-5,-6,-7)]


fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 20)

fit <-train(classe ~ . ,data=t1, method="rf",ntree=50)

ps <- predict(fit,train_test)

confusionMatrix(ps,train_test$classe)


p_sub <- predict(fit,testing)
submiss <- as.character(p_sub)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(submiss)
