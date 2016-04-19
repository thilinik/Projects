##load the libraries
library(leaps)
library(glmnet)
library(BMA)
library(glmulti)
library(rpart)
library(rpart.plot)
library(mgcv)

source("C:\\Users\\Toshiba\\Google Drive\\jobs\\ML\\supervised\\ML-abalone-functions.R")

##load the data set
abalone <-  read.csv("C:\\Users\\Toshiba\\Google Drive\\SFU\\stat\\fall15\\Stat852\\datasets\\abalone.csv", header=TRUE, sep=",", na.strings=" ")

##summary statistics of the dataset
summary(abalone)

##cleans the dataset
abalone <- clean_data(abalone)

##manipulates the datasets
abalone_datasets <- manipulate_data(abalone)

##dataset for the analysis
abalone_new <- abalone_datasets[[1]]

##dataset used for trees
abalone_trees <- abalone_datasets[[2]]

##create a summary which saves performance of the models at each iteration
summary<- data.frame(loop=c(1:20), OLS_full.MSE=NA, OLS_full=NA, 
                     Allsub.BIC.MSE=NA,Allsub.BIC=NA,
                     stepwise.MSE=NA, stepwise=NA,
                     LASSO.lse.MSE=NA,LASSO.lse=NA,
                     LASSO.min.MSE=NA,LASSO.min=NA, 
                     BMA.MSE=NA, BMA=NA,
                     GAM.MSE=NA, GAM=NA,
                     PPR.MSE=NA, PPR=NA,
                     fulltree.MSE=NA, fulltree=NA,
                     pruned.MSE=NA,pruned=NA)


##set the seed
set.seed(29003092)



for(i in 1:20)
{
  
  print(i)  
  
  ##performs split_datasets()
  split.data <- split_datasets(abalone_new,abalone_trees)
  
  
  ##1.performs OLS_regression()
  ols <- OLS_regression(split.data[[1]],split.data[[2]])

  
  ##evaluates the model
  performances <- evaluate_models(split.data[[1]],split.data[[2]],ols[[1]],ols[[2]])
  summary[i,2] <- performances[1]
  summary[i,3] <- performances[2]
  
 
  ##2. performs all subsets using BIC
  allsub.BIC <-allsubsets_BIC(split.data[[1]],split.data[[2]])
  
  ##evaluates the model
  performances <- evaluate_models(split.data[[1]],split.data[[2]],allsub.BIC[[1]],allsub.BIC[[2]])
  summary[i,4] <- performances[1]
  summary[i,5] <- performances[2]
  
  ##3. performs stepwise regression
  stepwise <- perform_stepwise(split.data[[1]],split.data[[2]])
  ##evaluates the model
  performances <- evaluate_models(split.data[[1]],split.data[[2]],stepwise[[1]],stepwise[[2]])
  summary[i,6] <- performances[1]
  summary[i,7] <- performances[2]
  
  ##4. performs LASSO using +1SE
  LASSO_lse <- LASSO_1se(split.data[[1]],split.data[[2]])
  ##evaluates the model
  performances <- evaluate_models(split.data[[1]],split.data[[2]],LASSO_lse[[1]],LASSO_lse[[2]])
  summary[i,8] <- performances[1]
  summary[i,9] <- performances[2]
  
  ##5. performs LASSO using min.
  LASSO_min <- perform_LASSO_min(split.data[[1]],split.data[[2]],LASSO_lse[[3]])
  
  ##evaluates the model
  performances <- evaluate_models(split.data[[1]],split.data[[2]],LASSO_min[[1]],LASSO_min[[2]])
  summary[i,10] <- performances[1]
  summary[i,11] <- performances[2]
  
  ##6. performs Bayesian model averaging
  BMAvg <- BMA(split.data[[1]],split.data[[2]])
  ##evaluates the model
  performances <- evaluate_models(split.data[[1]],split.data[[2]],BMAvg[[1]],BMAvg[[2]])
  summary[i,12] <- performances[1]
  summary[i,13] <- performances[2]
  
  ##7. performs gen. additive models
  Gen.additive.mod <- GAM(split.data[[1]],split.data[[2]])
  ##evaluates the model
  performances <- evaluate_models(split.data[[1]],split.data[[2]],Gen.additive.mod[[1]],Gen.additive.mod[[2]])
  summary[i,14] <- performances[1]
  summary[i,15] <- performances[2]
  
  ##8. performs projection pursuit regression
  proj.pursuit <- PPR(split.data[[1]],split.data[[2]])
  ##evaluates the model
  performances <- evaluate_models(split.data[[1]],split.data[[2]],proj.pursuit[[1]],proj.pursuit[[2]])
  summary[i,16] <- performances[1]
  summary[i,17] <- performances[2]
  
  ##9. performs full and pruned regression trees
  full.pruned.trees <- regression_trees(split.data[[3]],split.data[[4]])
  ##evaluates the model- under full tree
  performances <- evaluate_models(split.data[[3]],split.data[[4]],full.pruned.trees[[1]],full.pruned.trees[[2]])
  summary[i,18] <- performances[1]
  summary[i, 19] <- performances[2]
  
  ##evaluates the model- under pruned tree
  performances <- evaluate_models(split.data[[3]],split.data[[4]],full.pruned.trees[[3]],full.pruned.trees[[4]])
  summary[i,20] <- performances[1]
  summary[i,21] <- performances[2]

}


boxplot(sqrt(summary[,c(3,5,7,9,11,13,15,17,19,21)]),las=2,main="Box-plots of root-MSPE"
        ,cex.axis=0.7,cex.main=0.8,cex.lab=0.7,ylab='Root-MSPE',xlab='method')


