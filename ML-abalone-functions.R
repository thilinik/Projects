##function: cleans the dataset
##inputs : dataset
##outputs : cleaned dataset
clean_data <- function(data)
{
  data <- data[(data$Height > 0),]
  
  return(data)
}  

##function: creates dummies
##inputs : dataset
##outputs : dataset with dummies for 'sex'
create_dummies<- function(data)
{
  
  data$male <- ifelse(data$Sex=='M',1,0)
  data$female <- ifelse(data$Sex=='F',1,0)
  
  return(data)
}

##function: Manipulates data
##inputs : dataset
##outputs : manipulated dataset
manipulate_data<- function(data)
{
  
  data$Sex <- as.factor(data$Sex)
  data <- create_dummies(data)
  
  
  data_new <- data.frame(Rings=data[,9],data[,c(2:8)],data[,c(10:11)])
  data_trees <- data.frame(Rings=data[,9],data[,c(1:8)])
  
  return(list(data_new,data_trees))
  
}  


##function: splits the datasets
##inputs : datasets
##outputs : training and testing of each dataset
split_datasets<- function(data, data_trees)
{
  random_unif <- runif(n = nrow(data))
  data$set <- ifelse(random_unif <= 0.75, 1,2)
  
  aba_training <- data[data$set==1,]
  aba_test <- data[data$set==2,]
  
  aba_training.trees <- data_trees[data$set==1,]
  aba_test.trees <- data_trees[data$set==2,]
  
  return(list(aba_training,aba_test,aba_training.trees,aba_test.trees))
}


##function : performs OLS regression
##input : dataset
##output : predicted values of training and testing
OLS_regression <- function(data.training, data.test)
{
  ols<- lm(data = data.training[,c(1:10)], data.training[,1]~.)
  ols.tr.pred <- cbind(1,as.matrix(data.training[,c(2:10)])) %*% ols$coef 
  ols.test.pred <- cbind(1,as.matrix(data.test[,c(2:10)])) %*% ols$coef
  
  return(list(ols.tr.pred,ols.test.pred))
}


##function: variable selection using all subsets BIC
##inputs :  training and testing datasets
##outputs : predicted values for training and testing sets
allsubsets_BIC <- function(data.training, data.test)
{
  allsub.bic=regsubsets(x=data.training[,c(2:10)], y=data.training[,1], nbest=1,nvmax=9)
  min.bic.position1 <- which(summary(allsub.bic)$bic==min(summary(allsub.bic)$bic))
  best.mod1 <- summary(allsub.bic)$outmat[min.bic.position1,]
  terms1<-names(which(best.mod1=='*'))
  allsub.imp.x <- data.training[,terms1]
  
  allsub.fit <- lsfit(x = allsub.imp.x, y = data.training[,1])
  allsub.tr.pred <- cbind(1,as.matrix(allsub.imp.x)) %*% allsub.fit$coef 
  allsub.test.pred <- cbind(1,as.matrix(data.test[,terms1])) %*% allsub.fit$coef 
  
  return(list(allsub.tr.pred,allsub.test.pred))
}  

##function: variable selection using stepwise regression
##inputs :  training and testing datasets
##outputs : predicted values for training and testing sets
perform_stepwise <- function(data.training, data.test)
{
  initial.step <- lm(data=data.training[,c(1:10)],formula=Rings~ 1)
  final.step <- lm(data=data.training[,c(1:10)], formula=Rings~.)
  stepwise <- step(object=initial.step, scope=list(upper=final.step), trace=0,
                   k = log(nrow(data.training)))
  step.pred.tr <- predict(stepwise, data.training[,c(2:10)])
  step.pred.test <- predict(stepwise, data.test[,c(2:10)])
  
  return(list(step.pred.tr,step.pred.test))  
}

##function: variable selection using LASSO +1SE
##inputs :  training and testing datasets
##outputs : predicted values for training, testing sets and lasso model object
LASSO_1se <- function(data.training,data.test)
{
  las.mod.lse <- cv.glmnet(y=data.training[,1],x=as.matrix(data.training[,c(2:10)])
                           ,family = "gaussian")  
  
  las.tr.pred <- predict(las.mod.lse, newx = as.matrix(data.training[,c(2:10)]))
  las.test.pred <- predict(las.mod.lse, newx = as.matrix(data.test[,c(2:10)]))
  
  return(list(las.tr.pred,las.test.pred,las.mod.lse))
  
}

##function: variable selection using LASSO min.
##inputs :  training, testing datasets and LASSO +1se object
##outputs : predicted values for training and testing sets
perform_LASSO_min<- function(data.training,data.test,las.mod.lse)
{
  lasso.min <- glmnet(y=data.training[,1], x= as.matrix(data.training[,c(2:10)]), family="gaussian")
  lam.min <- las.mod.lse$lambda.min
  las.mod.min <- coef(lasso.min,s = lam.min)
  
  las.min.tr <-cbind(1,as.matrix(data.training[,c(2:10)])) %*% las.mod.min
  las.min.test <-cbind(1,as.matrix(data.test[,c(2:10)])) %*% las.mod.min
  
  return(list(las.min.tr,las.min.test))  
  
}

##function: variable selection using Bayesian model averaging
##inputs :  training and testing datasets
##outputs : predicted values for training and testing sets
BMA <- function(data.training,data.test)
{
  bma.tr <- bicreg(x = as.matrix(data.training[,c(2:10)]), y=data.training[,1]
                   , strict = FALSE, OR = 80)
  bma.coef <- bma.tr$postmean
  
  bma.tr <-cbind(1,as.matrix(data.training[,c(2:10)])) %*% bma.coef
  bma.test <-cbind(1,as.matrix(data.test[,c(2:10)])) %*% bma.coef
  
  return(list(bma.tr,bma.test))
  
}

##function: performs generalized additive models
##inputs :  training and testing datasets
##outputs : predicted values for training and testing sets
GAM <- function(data.training,data.test)
{
  gam.full <- gam(data = data.training[,c(1:11)], Rings~s(Length)+s(Diameter)+s(Height)
                  +s(Whole.weight)+s(Shucked.weight)+s(Viscera.weight)+s(Shell.weight)
                  +male+female,family=gaussian(link=identity))
  gam.pred.tr <- predict(gam.full,data.training[,c(2:10)])
  gam.pred.test <- predict(gam.full,data.test[,c(2:10)])
  
  return(list(gam.pred.tr,gam.pred.test))
}

##function: performs projection pursuit regression
##inputs :  training and testing datasets
##outputs : predicted values for training and testing sets
PPR<- function(data.training,data.test)
{
  ppr <- ppr(data=data.training[,c(1:10)], Rings~., nterms=2, optlevel=3) 
  
  ppr.pred.tr <- predict(ppr, data.training[,c(2:10)])
  ppr.pred.test <- predict(ppr, data.test[,c(2:10)])
  
  return(list(ppr.pred.tr,ppr.pred.test))
}

##function: grows full and pruned regression trees
##inputs :  training and testing datasets
##outputs : predicted values for training and testing sets
regression_trees <- function(data.training.trees,data.test.trees)
{
  full.tree <- rpart(data=data.training.trees, Rings~., method="anova", cp=0)
  full.cp <- full.tree$cptable 
  full.pred.tr <- predict(full.tree, data.training.trees[,c(2:9)])
  full.pred.test <- predict(full.tree, data.test.trees[,c(2:9)])
  
  minrow <- which.min(full.cp[,4])
  se.row <- min(which(full.cp[,4] < full.cp[minrow,4]+full.cp[minrow,5]))
  cplow.1se <- full.cp[se.row,1]
  cpup.1se <- ifelse(se.row==1, yes=1, no=full.cp[se.row-1,1])
  cp.1se <- sqrt(cplow.1se*cpup.1se)
  
  prune.1se <- prune(full.tree, cp=cp.1se)
  prune.pred.tr <- predict(prune.1se, data.training.trees[,c(2:9)])
  prune.pred.test <- predict(prune.1se, data.test.trees[,c(2:9)])
  
  return(list(full.pred.tr,full.pred.test,prune.pred.tr,prune.pred.test))
}


##function: evaluate the models
##inputs : datasets, fitted values
##outputs : MSE, MSPE
evaluate_models <- function(data.training,data.test,training.pred,test.pred)
{
  mse <- sum((data.training[,1]-training.pred)^2)/(nrow(data.training)-10)
  mspe <- mean((data.test[,1]-test.pred)^2)
  
  return (c(mse,mspe))
  
}



