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
fit = lm( log(BMI+1) ~  ., data = y, subset = trainIdx)
# summary(fit)

yPred = exp(predict(fit,y[-trainIdx,]))-1
result = cbind(yPred,yTrue = BMI[-trainIdx]) %>% na.omit() %>% as.data.frame()

library(dvmisc)
get_mse(fit)



library(ggplot2)
ggplot(data=result , aes(x = 1:dim(result)[1])) + geom_line(aes(y = yPred),color = 'red') + geom_line(aes(y = yTrue),color = 'blue')
# library(plotly)
# ggplotly(ggplot(data=result , aes(x = 1:dim(result)[1])) + geom_line(aes(y = yPred),color = 'red') + geom_line(aes(y = yTrue),color = 'blue'))
MSE = mean((result[,1]-result[,2])^2)
MSE
       