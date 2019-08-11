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

keep_inx <- exclude_accel(act = PAXINTEN_C, flags = Flags_C)
accel_good_C <- PAXINTEN_C[keep_inx,] 
flags_good_C <- Flags_C[keep_inx,]

# SEQN_inx = (accel_good_C %>% group_by(SEQN) %>% 
#               filter(length(WEEKDAY) == 7)  %>% as.data.frame 
#             %>% select(SEQN) %>% unique())$SEQN



# 4878 patients assumed alive, 732 assumed deceased, 4512 assumed NA

# %>% filter(mortstat != "NA")
mortality_good_C = Mortality_2011_C  %>%
  select(SEQN,mortstat,causeavl,ucod_leading,diabetes_mcod,hyperten_mcod,permth_exm)

covariate_good_C = Covariate_C %>% select(SEQN,RIDAGEYR,
                                          BMI,BMI_cat,Race,Gender,Diabetes,CHF,CHD,Cancer,
                                          Stroke,EducationAdult,MobilityProblem,DrinkStatus,DrinksPerWeek,SmokeCigs)

# dot multiply / sumup over min
MINdata = data.frame(accel_good_C[,1:5],accel_good_C[,6:dim(accel_good_C)[2]] * flags_good_C[,6:dim(accel_good_C)[2]]) %>% 
  select(-c(PAXCAL,PAXSTAT,SDDSRVYR)) %>% 
  # filter(SEQN %in% SEQN_inx) %>% as.data.frame() %>%
  select(-WEEKDAY) %>% group_by(SEQN) %>% summarise_each(funs(mean)) %>% as.data.frame()

# 0: Assumed alive 
# 1: Assumed deceased
# NA: Under age 18, not available for public release or ineligible for mortality follow-up
analyticData = MINdata  %>% inner_join(mortality_good_C %>% select(SEQN,mortstat,permth_exm), by = "SEQN") %>% 
  filter(mortstat != "NA") %>%
  mutate(mortstat = ifelse(mortstat == 0,yes = NA, no = 1)) # re-encode mortstat for further processing: multiply with time
# NA: alive
# 1: deceased
if(F){
  save(analyticData,file = "analyticData.rda")
  load(file = "analyticData.rda")
}

analyticData[1:10,1:10]
analyticData[1:5,(dim(analyticData)[2]-15):dim(analyticData)[2]]


##########################################################################################################################################
# spectrum: spectral analysis
rm(list = ls())
load(file = "analyticData.rda")
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
  analyticData[1:10,1:10]
  analyticData[1:5,(dim(analyticData)[2]-15):dim(analyticData)[2]]

}

# temp = analyticData[1,] %>% select(-SEQN,-permth) %>% as.numeric() %>% na.omit()
# spectrum(temp)
# plot(spectrum(temp),log = "no")
# spectrum(temp)$spec
dim( analyticData %>% select(-SEQN,-permth) )

y1 = apply(analyticData %>% select(-SEQN,-permth) , 1 ,FUN = function(i){
  data = i  %>% as.numeric() %>% na.omit %>% spectrum
  return(data$spec)
})

y1 <- data.frame(matrix(unlist(y1), nrow=length(y1), byrow=T))

dim(y1)

y2 = data.frame(SEQN = analyticData %>%  select(SEQN),
                 y1,
               permth = analyticData %>%  select(permth))

y2[1:5,1:5]
y2[1:5,(dim(y2)[2]-5):dim(y2)[2]]







y = y2$permth %>% as.matrix()
table(y)
x = y2 %>% select(-permth,-SEQN) %>% as.matrix()


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

ytrain <- to_categorical(ytrain,3)
ytest <- to_categorical(ytest, 3)


model <- keras_model_sequential() %>% 
  layer_dense(units = 2^8, activation = 'relu', input_shape = dim(xtrain)[2]) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 2^7, activation = 'relu') %>%
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

# model %>% predict_classes(xtest)

table(ytest = y[!trainIdx, ],model %>% predict_classes(xtest))


##########################################################################################################################################
# logistic: decease in 10 years or not
# 1 for alive, 0 for deceased
rm(list = ls())
load(file = "analyticData.rda")
analyticData$permth = analyticData$mortstat * analyticData$permth_exm
analyticData$permth = ifelse(analyticData$permth %>% is.na(),1,ifelse(analyticData$permth >= 0,0,NA)) %>% as.factor()
analyticData = analyticData %>% select(-mortstat,-permth_exm)

y = analyticData
set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
fit = glm(permth ~ ., family = "binomial", data = y, subset = trainIdx)
yPred =  (predict(fit, y[!trainIdx,], type = "response") > 0.5) * 1

ytest = y[!trainIdx, ]
ptab = table(yPred, ytest[,"permth"])
sum(diag(ptab)) / sum(ptab)
# [1] 0.7058333


##########################################################################################################################################

