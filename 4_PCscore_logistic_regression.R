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
y = pcscore[,c(2:4,which(colnames(pcscore) == 'mortstat'))]
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














