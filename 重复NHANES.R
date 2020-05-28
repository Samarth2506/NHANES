# 重复NHANES结果


rm(list = ls())
library(caret)
library(gridExtra)
load(file = 'analyticData.rda')
while(TRUE){
  res_RMSE = c()
  res_Rsquared = c()
  for(i in 1:50){
    PCnames = paste('PC',1:i,sep = '')
    train.control <- trainControl(method = "cv", number = 10)
    dat = analyticData[,c('RIDAGEYR','Race','Gender','BMI',PCnames)]
    model <- caret::train(BMI ~., data = dat, method = "lm",
                          trControl = train.control)
    res_RMSE = c(res_RMSE, model$results$RMSE)
    res_Rsquared = c(res_Rsquared, model$results$Rsquared)
  }
  if (
    (which(res_RMSE==min(res_RMSE)) == 14)&(which(res_Rsquared==max(res_Rsquared)) == 14)
  ){
    save(res_RMSE,res_Rsquared, file = 'CV_50PCs.rda')
    print('yes')
    break
  }
}