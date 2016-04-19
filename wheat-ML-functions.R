manipulate_data<- function(data)
{
  
  #Wheat.data$typenum <- as.numeric(Wheat.data$type)
  data$classnum <- as.numeric(data$class)
  
  return(data)
}

split_datasets<- function(data)
{
  perm <- sample(x=nrow(data))
  
  ##remove both the id and class
  set1 <- data[which(perm <= 200),-c(1,2)]
  set2 <- data[which(perm > 200),-c(1,2)]
  
  
  return(list(set1,set2))
}


lda_predictions <- function(data.training, data.test)
{
  lda.fit <- lda(x=data.training[,-6], grouping=data.training$type)
  
  #predictions
  lda.pred.train <- predict(lda.fit, newdata=data.training[,-6])$class
  lda.pred.valid <- predict(lda.fit, newdata=data.test[,-6])$class
  
  return(list(lda.pred.train,lda.pred.valid))
  
}

Qda_predictions <- function(data.training, data.test)
{
  qda.fit <- qda(x=data.training[,-6], grouping=data.training$type)
  ##predict
  qda.pred.train <- predict(qda.fit, newdata=data.training[,-6])$class
  qda.pred.valid <- predict(qda.fit, newdata=data.test[,-6])$class
  
  return(list(qda.pred.train,qda.pred.valid))
  
}
##rescale data first
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  return(x1)
}


logistic_predictions <- function(data.training, data.test)
{
  set1.rescale <- data.frame(cbind(rescale(data.training[,-6], data.training[,-6]), type=data.training$type))
  set2.rescale <- data.frame(cbind(rescale(data.test[,-6], data.training[,-6]), type=data.test$type))
  
  mod.fit <- multinom(data=set1.rescale, formula=type ~ ., maxit=100, trace=TRUE)
  ##predict
  log.pred.train <- predict(mod.fit, newdata=set1.rescale[,-7])
  log.pred.valid <- predict(mod.fit, newdata=set2.rescale[,-7])
  
  return(list(log.pred.train,log.pred.valid))
  
}



evaluate_models <- function(data.training,data.test,training.pred,test.pred)
{
  
  ##misclassification 
  
  misclass.train <- mean(ifelse(training.pred == data.training$type, yes=0, no=1))
  misclass.test<- mean(ifelse(test.pred == data.test$type, yes=0, no=1))
  
  return(list(misclass.train,misclass.test))
}