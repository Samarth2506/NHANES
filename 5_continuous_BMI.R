########################################################################
# BMI target
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
load(file = 'analyticData.rda')
analyticData = analyticData %>% select(-mortstat,-permth_exm) %>%
  inner_join(Covariate_D %>% select(SEQN,BMI),by = "SEQN")

analyticData$'log(BMI+1)' = log(analyticData$BMI+1)


y = analyticData %>% select(-SEQN)
# if(F){
#   save(y,file = 'BMI.rda')
# }
# rm(list = ls())
# load(file = 'BMI.rda')

set.seed(100)
BMI = y$BMI 
y = y %>% select(-BMI)
trainIdx = sample(nrow(y),0.7*nrow(y))
# trainIdx = sample(c(T,F),dim(y)[1],c(0.7,0.3),replace = T)
fit = lm( y$`log(BMI+1)` ~  ., data = y, subset = trainIdx)
# summary(fit)
yPred = exp(predict(fit,y[-trainIdx,]))-1
result = cbind(yPred,yTrue = BMI[-trainIdx]) %>% na.omit() %>% as.data.frame()
modelmse = mean(summary(fit)$residuals^2)
modelmse

# mean(
#   apply(result,MARGIN = 1,FUN = function(i){abs(i[1]-i[2])<=5})
# )

mean((result[,1]-result[,2])^2)

library(ggplot2)
ggplot(data=result , aes(x = 1:dim(result)[1])) + 
  geom_line(aes(y = yPred),color = 'red') + 
  geom_line(aes(y = yTrue),color = 'blue')
# library(plotly)
# ggplotly(ggplot(data=result , aes(x = 1:dim(result)[1])) + geom_line(aes(y = yPred),color = 'red') + geom_line(aes(y = yTrue),color = 'blue'))

#########################################################################
### ridge regression on BMI

rm(list = ls())
load(file = "analyticData.rda")
analyticData = analyticData %>% select(-mortstat,-permth_exm)
analyticData = analyticData %>% inner_join(Covariate_D[,c('SEQN','BMI')],by = "SEQN")

analyticData = analyticData %>% na.omit()
set.seed(100)
trainIdx = sample(dim(analyticData)[1],0.8*dim(analyticData)[1])


ytrain = analyticData$BMI[trainIdx]
xtrain = analyticData[trainIdx,grep("MIN",colnames(analyticData))] %>% as.matrix()



library(glmnet)
fit <- glmnet(xtrain, ytrain, lambda = 
                cv.glmnet(xtrain, ytrain)$lambda.min)
ypred = predict(fit, newx = analyticData[-trainIdx,grep("MIN",colnames(analyticData))] %>% as.matrix(),
                s = 'lambda.min')

result = cbind(ypred,ytrue = analyticData$BMI[-trainIdx]) %>% na.omit() %>% as.data.frame()
mean((result[,1]-result[,2])^2)

library(ggplot2)
ggplot(data=result , aes(x = 1:dim(result)[1])) + 
  geom_line(aes(y = ypred),color = 'red') + 
  geom_line(aes(y = ytrue),color = 'blue')




