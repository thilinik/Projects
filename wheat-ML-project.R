library(MASS)
library(nnet)

##load the source file
source("C:\\Users\\Toshiba\\Google Drive\\jobs\\ML\\unsupervised\\wheat-ML-functions.R")


##load the data set
Wheat.data <-  read.csv("C:\\Users\\Toshiba\\Google Drive\\SFU\\stat\\fall15\\Stat852\\datasets\\wheat.csv",header=TRUE,sep=",")
head(Wheat.data)

##summarize the dataset
summary(Wheat.data)

##convert 'class' to numeric

Wheat.data <- manipulate_data(Wheat.data)



##Split the data

set.seed(67982193)


summary<- data.frame(loop=c(1:20), LDA_tr_error=NA, LDA_test_error=NA, 
                     QDA_tr_error=NA,QDA_test_error=NA,logistic_tr_error=NA,
                     logistic_test_error=NA)

for(i in 1:20)
{
  
  
  
  set1 <- split_datasets(Wheat.data)[[1]]
  set2 <- split_datasets(Wheat.data)[[2]]
  
  ##LDA

lda_pred <- lda_predictions(set1,set2)


lda_misclassifications <- evaluate_models(set1,set2,lda_pred[[1]],lda_pred[[2]])

summary[i,2] <- lda_misclassifications[[1]]
summary[i,3] <- lda_misclassifications[[2]]

##QDA
qda_pred <-  Qda_predictions(set1,set2)


Qda_misclassifications <- evaluate_models(set1,set2,qda_pred[[1]],qda_pred[[2]])

summary[i,4] <- Qda_misclassifications[[1]]
summary[i,5] <- Qda_misclassifications[[2]]


##Logistic
logistic_pred <- logistic_predictions(set1,set2)

logistic_misclassifications <- evaluate_models(set1,set2,logistic_pred[[1]],logistic_pred[[2]])

summary[i,6] <- logistic_misclassifications[[1]]
summary[i,7] <- logistic_misclassifications[[2]]

}



a <- rnorm(100)
b <- rnorm(100)

library(ggplot2)
library(tidyverse)

df <- tibble(a=a,b=b)

df %>% ggplot(aes(a,b))+geom_point()
