rm(list = ls())
if(T){
  library(rnhanesdata)
  library(tidyverse)
  library(magrittr)
  library(data.table)
  library(randomForest)
  library(caret)
  library(keras)
  library(tensorflow)
}


##########################################################################################################################################
# logistic: decease in 10 years or not
# 1 for alive, 0 for deceased

rm(list = ls())
load(file = "analyticData.rda")
analyticData$permth = analyticData$mortstat * analyticData$permth_exm
analyticData$permth = ifelse(analyticData$permth %>% is.na(),1,ifelse(analyticData$permth >= 0,0,NA)) %>% as.factor()
analyticData = analyticData %>% select(-mortstat,-permth_exm)
table(analyticData$permth)

y = analyticData %>% select(-SEQN)
set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
fit = glm(permth ~ ., family = "binomial", data = y, subset = trainIdx)

yPred =  (predict(fit, y[!trainIdx,], type = "response") > 0.5) * 1

ytest = y[!trainIdx, ]
ptab = table(yPred, ytest[,"permth"])
ptab
sum(diag(ptab)) / sum(ptab)

# [1] 0.7072581
# over-fitting problem
##########################################################################################################################################
# logistic ridge regression
# https://stackoverflow.com/questions/30565457/getting-glmnet-coefficients-at-best-lambda
# https://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
rm(list = ls())
load(file = "analyticData.rda")
analyticData$permth = analyticData$mortstat * analyticData$permth_exm
analyticData$permth = ifelse(analyticData$permth %>% is.na(),1,ifelse(analyticData$permth >= 0,0,NA)) %>% as.factor()
analyticData = analyticData %>% select(-mortstat,-permth_exm)
table(analyticData$permth)

analyticData = analyticData %>% na.omit()
set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(analyticData)[1], replace = TRUE, prob = c(.7, .3))
library(glmnet)
y = analyticData[trainIdx,] %>% select(permth) %>% as.matrix() %>% as.numeric()
x = analyticData[trainIdx,] %>% select(-SEQN,-permth) %>% as.matrix()

fit = glmnet(x,y,family = 'binomial',
             lambda=cv.glmnet(as.matrix(x), y)$lambda.1se)
#lambda.1se

# coef(fit)
# plot(fit,xvar = 'lambda')
# cv.glmmod <- cv.glmnet(x, y %>% unclass, alpha=1)
# plot(cv.glmmod)
yPred = ( predict(fit, newx = analyticData[!trainIdx,] %>% select(-SEQN,-permth)  %>% as.matrix(),
                  s = 'lambda.1se',
                  type = 'response') > 0.5) * 1

ytest = analyticData[!trainIdx, ] %>% select(permth)
ptab = table(ytest = ytest[,"permth"], ytru = factor(yPred,levels = levels(ytest[,"permth"])))
ptab
sum(diag(ptab)) / sum(ptab)




##########################################################################################################################################
# glm pc scores
load(file = "analyticData.rda")
analyticData = analyticData %>% select(-permth_exm)
# NA: alive
# 1: deceased
analyticData$mortstat = ifelse(analyticData$mortstat %>% is.na,1,0)
# 1: alive, 0 : deceased

# which(sapply(analyticData, class) != "numeric")
# analyticData = sapply(analyticData, as.numeric) %>% as.data.frame()

pca = prcomp(analyticData %>% select(-SEQN,-mortstat) %>% na.omit() ,
             center = T,
             scale. = T)
if(F){
  save(pca,file='pca.rda')
}
rm(list = ls())
load(file = 'pca.rda')
screeplot(pca)

if(F){
  pcscore = data.frame(SEQN = analyticData %>% na.omit %>% select(SEQN),
                       pca$x,
                       mortstat = analyticData %>% na.omit %>% select(mortstat))
  save(pcscore, file = "pcscore.rda")
}
rm(list = ls())
load(file = "pcscore.rda")

# first 3 PCs
y = pcscore[,c(2:10,which(colnames(pcscore) == 'mortstat'))]
y$mortstat = as.factor(y$mortstat)
set.seed(100)
# trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
trainIdx = sample(dim(y)[1],0.7*dim(y)[1])
fit = glm(mortstat ~ ., family = "binomial", data = y, subset = trainIdx)
summary(fit)

yPred =  (predict(fit, y[-trainIdx,], type = "response") > 0.5) * 1

ytest = y[-trainIdx, ]
ptab = table(ytest[,"mortstat"], yPred %>% factor(levels = levels(ytest[,"mortstat"] )))
ptab
sum(diag(ptab)) / sum(ptab)


# > ptab
# 
#      0    1
# 0    0  168
# 1    0 1072
# > sum(diag(ptab)) / sum(ptab)
# [1] 0.8645161

##########################################################################################################################################
# glm pc scores
load(file = 'pcscore.rda')
load(file = 'pca.rda')
screeplot(pca)
#first three PCs
y = pcscore[,c(1:4,which(colnames(pcscore) == 'mortstat'))] %>% select(-SEQN) 
set.seed(100)
trainIdx = sample(nrow(y),0.7*nrow(y))
fit = glm(mortstat ~ ., family = "binomial", data = y, subset = trainIdx)
summary(fit)
yPred =  (predict(fit, y[-trainIdx,], type = "response") > 0.5) * 1

ytest = y[-trainIdx, ]
ptab = table(yPred, ytest[,"mortstat"])
ptab
sum(diag(ptab)) / sum(ptab)












