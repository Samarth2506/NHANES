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

load(file = "analyticData.rda")
analyticData = analyticData %>% select(-permth_exm)
# NA: alive
# 1: deceased
analyticData$mortstat = ifelse(analyticData$mortstat %>% is.na,1,0)
# which(sapply(analyticData, class) != "numeric")
# analyticData = sapply(analyticData, as.numeric) %>% as.data.frame()

pca = prcomp(analyticData %>% select(-SEQN,-mortstat) %>% na.omit() ,
             center = T,
             scale. = T)

# first 3 PCs
y = data.frame(pca$x[,1:3],mortstat = analyticData %>% na.omit %>% select(mortstat))
y$mortstat = as.factor(y$mortstat)
set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
fit = glm(mortstat ~ ., family = "binomial", data = y, subset = trainIdx)
yPred =  (predict(fit, y[!trainIdx,], type = "response") > 0.5) * 1

ytest = y[!trainIdx, ]
ptab = table(ytest[,"mortstat"], yPred %>% factor(levels = levels(ytest[,"mortstat"] )))
ptab
sum(diag(ptab)) / sum(ptab)
