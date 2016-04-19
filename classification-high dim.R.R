
#####Penalized(L2) logistic regression#################

######download "CMA" and "limma" from bioconductor################################
##################################################################################
source("https://bioconductor.org/biocLite.R")
biocLite("CMA")
biocLite("limma")

########load the libraries###############################################
#########################################################################

library(CMA)
library(limma)
library(pamr)
library(plsgenomics)


######1. load datasets-SRBCT################################################################
#     info: downoaded from http://statweb.stanford.edu/~tibs/ElemStatLearn/                #
#                                                                                          #  
#           SBRCT gene expression data. 2308 genes(p), 63 training samples(n1),            #
#           25 test samples(n2)                                                            # 
#           One gene per row, one sample per column                                        #
#           Cancer classes are labelled as 1:"EWS",2:"RMS",3:"NB" and 4:"BL"               #
############################################################################################
set.seed(27611)
xtrain <- read.table("C:\\Users\\Toshiba\\Google Drive\\SFU\\stat\\fall15\\Stat852\\Project2\\analysis\\xtrain.txt")
ytrain <- read.table("C:\\Users\\Toshiba\\Google Drive\\SFU\\stat\\fall15\\Stat852\\Project2\\analysis\\ytrain.txt")
ytrain <- as.factor(unlist(ytrain))
xtrain <- t(as.matrix(xtrain,ncol=63))

xtest <- read.table("C:\\Users\\Toshiba\\Google Drive\\SFU\\stat\\fall15\\Stat852\\Project2\\analysis\\xtest.txt")
ytest <- read.table("C:\\Users\\Toshiba\\Google Drive\\SFU\\stat\\fall15\\Stat852\\Project2\\analysis\\ytest.txt")
ytest = as.factor(unlist(ytest))
xtest <- t(as.matrix(xtest,ncol=63))

data.train <- cbind(ytrain,xtrain)
colnames(data.train)[1] <- 'y'

######2. Normalize X variables ########################################################
#######################################################################################
xnorm<- scale(xtrain)

dim(xnorm)
mean_train <- apply(xnorm,2,mean)
sd_train <- apply(xnorm,2,sd)
xtest_norm<- scale(xtest)
mean_test <- apply(xtest_norm,2,mean)
sd_test <- apply(xtest_norm,2,sd)


######3. Create the learning sets##########################################################
#                   Using CMA:::GenerateLearningsets                                      #
#                                                                                         #  
#     Function: GenerateLearningsets(n, y, method = c("LOOCV", "CV", "MCCV", "bootstrap"),#
#                       fold = NULL, niter = NULL, ntrain = NULL, strat = FALSE)          #
#     Output  : An object of class 'learningsets'                                         #
#                                                                                         #
###########################################################################################

fiveCV10iter <- GenerateLearningsets(y = ytrain, method ="CV", fold = 5, niter = 10, strat = TRUE)
learn_matrix <-fiveCV10iter@learnmatrix

######3. Choose lambda##########################################################
#                   Using CMA:::tune                                      #
#                                                                                         #  
#     Function: tune(X, y, f, learningsets, genesel, genesellist = list(), nbgene, classifier, fold = 3, strat = FALSE          #
#     Output  : Returns the best lambda that gives the min.cv error                                         #
#                                                                                         #
###########################################################################################

tune_plr <- CMA:::tune(X = xnorm, y = ytrain, learningsets =
                         fiveCV10iter, classifier = plrCMA, grids =list(lambda=10^{-10:8}),fold=5)

cv_errors <- matrix(unlist(tune_plr@tuneres),ncol=50)
cv_errors2 <- data.frame(cv_errors, lam=tune_plr@hypergrid)
data_plot <- data.frame(lamda=tune_plr@hypergrid, cv_err=cv_errors2[,50])
plot(x=log(data_plot$lambda),y=data_plot$cv_err,type='b')


######4. Feature selection##########################################################
#                   Using CMA:::GeneSelection                                     #
#                                                                                         #  
#     Function: GeneSelection(X, y, f, learningsets, method = c("t.test", "welch.test", "wilcox.test", "f.test", "kru        #
#     Output  : used 'rfe'(recursive feature elimination) and 'kruskal'(UR)
#               Provides top 10 variables for the 1st iteration     
#                                                                                         #
###########################################################################################


genesel_kru <- GeneSelection(X = xnorm, y = ytrain, learningsets
                             = fiveCV10iter, method = "kruskal.test",scheme = "multiclass")
genesel.rfe <- GeneSelection(X = xnorm, y = ytrain,learningsets
                             = fiveCV10iter, method = "rfe" ,scheme="one-vs-all",trace=TRUE)

tab_krus <- toplist(genesel_kru,show=F)

tab.rfe <- toplist(genesel.rfe,show = F)
tab_rfe_imp <- data.frame(v1=tab.rfe[[1]][,1],v2=tab.rfe[[2]][,1],v3=tab.rfe[[3]][,1], v4=tab.rfe[[4]][,1])

##common features selected by both the methods
##var 246 1954  545 1389 2050  554 1003
common_var=intersect(x=unlist(tab_rfe_imp),tab_krus[,1])


######5. Fitting the model##########################################################
#                   Using CMA:::Classification                                     #
#                                                                                         #  
#     Function: classification(X, y, f, learningsets, genesel, genesellist = list(), nbgene, classifier, tuneres        #
#     Output  : Performs parameter tuning, variable selection and classification
#                    
#                                                                                         #
###########################################################################################


class_plr_rfe<- classification(X = xnorm, y = ytrain, learningsets
                            = fiveCV10iter, classifier = plrCMA, genesel = genesel.rfe,
                            nbgene = 10,tuneres=tune_plr)


class_plr_kruskal <- classification(X = xnorm, y = ytrain, learningsets
                                    = fiveCV10iter, classifier = plrCMA, genesel = genesel_kru,
                                    nbgene = 10,tuneres=tune_plr)


class_SVM_kruskal <- classification(X = xnorm, y = ytrain, learningsets
                                      = fiveCV10iter, classifier = svmCMA, genesel = genesel_kru,
                                      nbgene = 10)
class_SVM_rfe <- classification(X = xnorm, y = ytrain, learningsets
                                    = fiveCV10iter, classifier = svmCMA, genesel = genesel_kru,
                                    nbgene = 10)


##for the 1st iteration, predicted classes of the test set.
ftable(class_plr_rfe[[42]])
ftable(class_plr_tune2[[1]])
ftable(class_plr_kruskal[[50]])
ftable(class_plr_kruskal_2[[1]])
ftable(class_SVM_kruskal[[1]])

######6. Prediction##########################################################
#                   Using CMA:::Prediction                                     #
#                                                                                         #  
#     Function: prediction(X.tr,y.tr,X.new,f,classifier,genesel,models=F,nbgene,tuneres,...)        #
#     Output  : Performs parameter tuning, variable selection and classification
#               then predicts labels for new observations    
#                                                                                         #
###########################################################################################



predict.test_rfe <- prediction(X.tr = xnorm,y.tr = ytrain,X.new =xtest_norm,classifier = "plrCMA",
                           gensel=genesel.rfe,nbgene = 5)

predict.test_krus <- prediction(X.tr = xnorm, y.tr = ytrain, X.new =xtest_norm, 
                               classifier = "plrCMA", genesel = genesel_kru, nbgene = 5)
                               
predict.test_rfe_svm <- prediction(X.tr = xnorm,y.tr = ytrain,X.new =xtest_norm,classifier = "svmCMA",
                               gensel=genesel.rfe,nbgene = 5)

predict.test_krus_svm <- prediction(X.tr = xnorm, y.tr = ytrain, X.new =xtest_norm, 
                                classifier = "svmCMA", genesel = genesel_kru, nbgene = 5)
##proves what we are doing is correct, when omitted the feature selection, misclassification rates are higher
predict.test <- prediction(X.tr = xnorm,y.tr = ytrain,X.new =xtest_norm,classifier = "plrCMA")


non_missing_index <- which(!is.na(ytest))
comparisons <- data.frame(y=ytest[non_missing_index],
                          PLR_RFE=show(predict.test_rfe)[non_missing_index],
                          PLR_UR=show(predict.test_krus)[non_missing_index],
                          SVM_RFE=show(predict.test_rfe_svm)[non_missing_index],
                          SVM_UR=show(predict.test_krus_svm)[non_missing_index])


summary <- data.frame(Method=c("PLR-RFE","PLR-UR","SVM-RFE","SVM-UR"),test_error=NA)

for(i in 1:4)
{
summary[i,2]<- mean(ifelse(comparisons$y == comparisons[,(i+1)], yes=0, no=1))
}

##confusion matrix
PLR_UR_confusion <- table(comparisons$y, comparisons$PLR_UR, dnn=c("Obs","Pred"))
SVM_UR_confusion <- table(comparisons$y, comparisons$SVM_UR, dnn=c("Obs","Pred"))



