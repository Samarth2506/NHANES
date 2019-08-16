##########################################################################################################################################
# re-encode time-series 7*1440 matrix to 1*(1440*7) matrix

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
  library(data.table)
}



keep_inx <- exclude_accel(act = PAXINTEN_D, flags = Flags_D)
accel_good_D <- PAXINTEN_D[keep_inx,] 
flags_good_D <- Flags_D[keep_inx,]


MINdata = data.frame(accel_good_D[,1:5],accel_good_D[,6:dim(accel_good_D)[2]] * flags_good_D[,6:dim(accel_good_D)[2]]) %>% 
  select(-c(PAXCAL,PAXSTAT,SDDSRVYR)) %>% 
  reshape(idvar = "SEQN", timevar = "WEEKDAY",direction = "wide") %>% na.omit()

analyticData = MINdata  %>% inner_join(Mortality_2015_D %>% select(SEQN,mortstat,permth_exm), by = "SEQN") %>% 
  filter(mortstat != "NA") %>%
  mutate(mortstat = ifelse(mortstat == 0,yes = NA, no = 1)) # re-encode mortstat for further processing: multiply with time
# NA: alive, 1: deceased
analyticData[1:10,1:10]
analyticData[1:5,(dim(analyticData)[2]-15):dim(analyticData)[2]]

# re-encode permth 
if(T){
  permth =  ((analyticData$mortstat * analyticData$permth_exm) %/% 12)
  table(permth, useNA = "ifany")
  permth[permth < 5 ] <- 0
  permth[permth >= 5] <- 1
  permth[is.na(permth)] <- 2
  # 0 for 0~5 years, 1 for 5~10 years, 2 for alive
  analyticData$permth = permth %>% as.factor()
  analyticData = analyticData %>% select(-mortstat,-permth_exm)
  table(analyticData$permth)
  analyticData[1:5,(dim(analyticData)[2]-10):dim(analyticData)[2]]
}

if(F){
  save(analyticData,file = "10080.rda")
}
y = analyticData$permth %>% as.matrix()
x = analyticData %>% select(-SEQN) %>% as.matrix()

set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(x)[1], replace = TRUE, prob = c(.7, .3))

ytrain = y[trainIdx, ]
xtrain = x[trainIdx, ]

# %>% scale()
# mns = attr(xtrain, "scaled:center")
# sds = attr(xtrain, "scaled:scale")

xtest = x[!trainIdx, ]   
# %>% scale(center = mns, scale = sds)
ytest = y[!trainIdx, ]

table(ytrain)
table(ytest)
ytrain <- to_categorical(ytrain,3)
ytest <- to_categorical(ytest, 3)

model <- keras_model_sequential() %>% 
  layer_dense(units = 2^8, activation = 'relu', input_shape = dim(xtrain)[2]) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 2^4, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 3, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  xtrain, ytrain, 
  epochs = 30, 
  batch_size = 128,
  validation_split = 0.2
)

plot(history)

model %>% evaluate(xtest, ytest)

# $loss
# [1] 2.210287
# 
# $acc
# [1] 0.8628692


# model %>% predict_classes(xtest)

table(ytest = y[!trainIdx, ],model %>% predict_classes(xtest))

# ytest   2
#     0  26
#     1  39
#     2 409
