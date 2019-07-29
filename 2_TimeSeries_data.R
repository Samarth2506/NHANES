rm(list = ls())
if(T){
  library(rnhanesdata)
  library(tidyverse)
  library(magrittr)
  library(data.table)
  library(randomForest)
  library(caret)
}
# The monitors were programmed to begin recording activity information for successive 1 minute intervals (epochs) beginning at 
# 12:01 a.m. the day after the health examination.
########################################################################
# data preprocess/ get valid data
########################################################################

keep_inx <- exclude_accel(act = PAXINTEN_C, flags = Flags_C)
accel_good_C <- PAXINTEN_C[keep_inx,] %>% na.omit()
flags_good_C <- Flags_C[keep_inx,] %>% na.omit()




complete_ind = MINdata %>% group_by(SEQN) %>% 
  summarise(weeklength = n()) %>% as.data.frame() %>%
  filter(weeklength == 7) %>% select(SEQN)


mortality_good_C = Mortality_2011_C %>% filter(eligstat == 1,mortstat == 1) %>% 
  mutate(permth = (permth_int %/% 12) %>% as.factor()) %>% 
  select(SEQN,causeavl,ucod_leading,diabetes_mcod,hyperten_mcod,permth)

covariate_good_C = Covariate_C %>% select(SEQN,SDMVPSU,SDMVSTRA,WTINT2YR,WTMEC2YR,RIDAGEYR,
                                          BMI,BMI_cat,Race,Gender,Diabetes,CHF,CHD,Cancer,
                                          Stroke,EducationAdult,MobilityProblem,DrinkStatus,DrinksPerWeek,SmokeCigs)

# ranges of SDDSRVYR are 3 3 in all data frame we use i.e. 2003-2004 wave
# sum(accel_good_C[,1:5] != flags_good_C[,1:5]) = 0 
# non-wearable and wearble

# plot(y = accel_good_C[1,6:1439],x = 1:1434,type = "l")
# lines(x=1:1434,y = MINdata[1,6:1439],col = "red")



# raw
if(F){
MINdata = accel_good_C %>% 
  select(-c(PAXCAL,PAXSTAT,SDDSRVYR)) %>% 
  group_by(SEQN) %>% filter(length(WEEKDAY) == 7) %>% as.data.frame() %>% select(-WEEKDAY)# complete 7 days
}

# dot multiply matrix between accel and flags
MINdata = data.frame(accel_good_C[,1:5],accel_good_C[,6:dim(accel_good_C)[2]] * flags_good_C[,6:dim(accel_good_C)[2]]) %>% 
  select(-c(PAXCAL,PAXSTAT,SDDSRVYR)) %>% 
  group_by(SEQN) %>% filter(length(WEEKDAY) == 7) %>% as.data.frame() %>% select(-WEEKDAY) %>% # complete 7 days
  group_by(SEQN) %>% 
  summarise_all(funs(sum)) %>% as.data.frame()  # sum up over days

if(F){
MINdata = data.frame(
  SEQN = MINdata$SEQN,
  apply(MINdata[,-1], MARGIN = 1, function(i) i/sum(i)) %>% t()
) # compositional data
}


MINdata[1:10,1:10]
MINdata[1:5,(dim(MINdata)[2]-20):dim(MINdata)[2]]

# just acctivity
if(F){
analyticData = MINdata %>%
  inner_join(mortality_good_C %>% select(SEQN,permth),by = "SEQN")

}

# covariate included
analyticData = MINdata %>% 
  inner_join(covariate_good_C, by = "SEQN") %>% 
  inner_join(mortality_good_C, by = "SEQN")

# manually encode character-levels factor
analyticData <- as.data.frame(lapply(analyticData, function (x) if (is.factor(x)) unclass(x) %>% as.factor() else x)) 
analyticData <- as.data.frame(lapply(analyticData, function (x) if (is.factor(x)) unclass(x) %>% as.numeric() else x)) 
analyticData = analyticData %>% na.omit()


analyticData[1:10,1:10]
analyticData[1:5,(dim(analyticData)[2]-20):dim(analyticData)[2]]

# MINdata.imputed = rfImpute(permth ~ . ,data = MINdata,iter = 6)
# MINdata2 <- as.data.frame(lapply(MINdata, function (x) if (is.character(x)) {as.numeric(as.character(x))} else x))



################################################################################################################################################
# DL

y = analyticData$permth%>% as.matrix()
x = analyticData %>% select(-permth,-SEQN) %>% as.matrix()


set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(x)[1], replace = TRUE, prob = c(.7, .3))
ytrain = y[trainIdx, ]
xtrain = x[trainIdx, ] %>% scale()

mns = attr(xtrain, "scaled:center")
sds = attr(xtrain, "scaled:scale")

xtest = x[!trainIdx, ]  %>% scale(center = mns, scale = sds)
ytest = y[!trainIdx, ]

ytrain <- to_categorical(ytrain, 10)
ytest <- to_categorical(ytest, 10)


model <- keras_model_sequential() %>% 
  layer_dense(units = 2^10, activation = 'relu', input_shape = dim(xtrain)[2]) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 2^5, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  # layer_dense(units = 2^5, activation = 'relu') %>%
  # layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10, activation = 'softmax')

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

model %>% predict_classes(xtest)

# ptab = table(model %>% predict_classes(xtest),
#              ytest)
# ptab
# sum(diag(ptab)) / sum(ptab)


################################################################################################################################################
# ML


y = analyticData %>% na.omit() %>% select(-SEQN)
y <- as.data.frame(lapply(y, function (x) if (is.factor(x)) unclass(x) %>% as.factor() else x)) 
y[1:10,1440:1460]

set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
traindata = y[trainIdx,]
testdata = y[!trainIdx,]

traindata$permth <- factor(traindata$permth) %>% droplevels()
testdata$permth <- factor(testdata$permth) %>% droplevels()

model = train(permth ~ ., data = traindata,
              method = "svmLinear")

pred = model %>% predict(testdata)
ptab = table(testdata$permth,pred)
print(paste('Accuracy',sum(diag(ptab))/sum(ptab)))

# svmLinear [1] "Accuracy 0.126984126984127"
# ordinalRF [1] "Accuracy 0.111111111111111"


################################################################################################################################################
# PCA

# dot multiply matrix between accel and flags
MINdata = data.frame(accel_good_C[,1:5],accel_good_C[,6:dim(accel_good_C)[2]] * flags_good_C[,6:dim(accel_good_C)[2]]) %>% 
  select(-c(PAXCAL,PAXSTAT,SDDSRVYR)) %>% 
  group_by(SEQN) %>% filter(length(WEEKDAY) == 7) %>% as.data.frame() %>% select(-WEEKDAY) %>% # complete 7 days
  group_by(SEQN) %>% 
  summarise_all(funs(sum)) %>% as.data.frame()  # sum up over days
analyticData = MINdata %>% 
  inner_join(covariate_good_C, by = "SEQN") %>% 
  inner_join(mortality_good_C, by = "SEQN") 

# manually encode character-levels factor
analyticData <- as.data.frame(lapply(analyticData, function (x) if (is.factor(x)) unclass(x) %>% as.numeric() else x)) 
analyticData = analyticData %>% na.omit()

analyticData[1:10,1:10]
analyticData[1:5,(dim(analyticData)[2]-20):dim(analyticData)[2]]

plot(prcomp(analyticData %>% select(-SEQN,-permth)))

analyticData2 = prcomp(analyticData %>% select(-SEQN,-permth))$x %>% as.data.frame() %>%
  mutate(permth = analyticData$permth %>% as.factor())
analyticData2[1:5,1:5]
analyticData2[1:5,200:204]


if(T){
  y = analyticData2$permth %>% as.matrix()
  x = analyticData2 %>% select(-permth) %>% as.matrix()
  
  
  set.seed(100)
  trainIdx = sample(c(TRUE, FALSE), dim(x)[1], replace = TRUE, prob = c(.7, .3))
  ytrain = y[trainIdx, ]
  xtrain = x[trainIdx, ] 
  # %>% scale()
  
  mns = attr(xtrain, "scaled:center")
  sds = attr(xtrain, "scaled:scale")
  
  xtest = x[!trainIdx, ]  
  # %>% scale(center = mns, scale = sds)
  ytest = y[!trainIdx, ] 
  
  ytrain <- to_categorical(ytrain, 10)
  ytest <- to_categorical(ytest, 10)
  
  
  model <- keras_model_sequential() %>% 
    layer_dense(units = 2^10, activation = 'relu', input_shape = dim(xtrain)[2]) %>% 
    layer_dropout(rate = 0.4) %>% 
    layer_dense(units = 2^5, activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 10, activation = 'softmax')
  
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
  
  model %>% predict_classes(xtest)
  
}

if(T){
  y = analyticData2 %>% na.omit()
  y[1:5,1:5]
  y[1:5,200:204]
  set.seed(100)
  trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
  traindata = y[trainIdx,]
  testdata = y[!trainIdx,]
  
  traindata$permth <- factor(traindata$permth) %>% droplevels()
  testdata$permth <- factor(testdata$permth) %>% droplevels()
  
  plot(prcomp(analyticData %>% select(-SEQN,-permth)))
  model = train(permth ~ ., data = traindata,
                method = "svmLinear")
  # note, choose first 3 PC resulting in lower ACC
  pred = model %>% predict(testdata)
  ptab = table(testdata$permth,pred)
  print(paste('Accuracy',sum(diag(ptab))/sum(ptab)))
}









################################################################################################################################################
t = 60
if(T){
HOURdata = MINData %>% select(-c(SEQN,WEEKDAY)) %>% 
  apply(MARGIN = 1, FUN = function(i){
  colSums(matrix(i, nrow = t)) # hour-level
}) %>% t() 
HOURdata = data.frame(MINdata[,1:2],HOURdata) 
HOURdata = HOURdata %>% group_by(SEQN) %>% filter(length(WEEKDAY) == 7) %>% as.data.frame()# 7 days
HOURdata = split(HOURdata,HOURdata$SEQN) %>% lapply(FUN = function(i){
  colMeans(i[,grep("X",colnames(i))])
}) %>% do.call(what = "rbind")
HOURdata = data.frame(SEQN = as.integer(rownames(HOURdata)),HOURdata)
HOURdata2 = left_join(HOURdata,Covariate_C,by = "SEQN") %>% 
  inner_join(mortality_good_C %>% select(SEQN,permth),by = "SEQN")
########################################################################
  # rfImpute()
########################################################################

y = HOURdata2 %>% select(-SEQN) %>% na.omit()
y <- as.data.frame(lapply(y, function (x) if (is.factor(x)) factor(x) else x)) 

# y.imputed = rfImpute(permth ~ ., data = y,iter = 20)
fit = randomForest(permth ~ ., data = y,ntree = 4000,
                   importance = T)
varImpPlot(fit)
}


MINnames = colnames(MINdata)[3:dim(MINdata)[2]]

WEEKdata = split(MINdata,MINdata$SEQN) %>% lapply(FUN = function(i){
    WEEKstats = apply(i[,MINnames],1,function(r){
      c(sum(r),
        max(r),
        min(r),
        sd(r))
    })
    data.frame(i[,1:2],WEEKlength = length(i[,"WEEKDAY"]),
               WEEKsum = WEEKstats[1,],
               WEEKmax = WEEKstats[2,],
               WEEKmin = WEEKstats[3,],
               WEEKsd  = WEEKstats[4,]
               )
}) %>% 
  do.call(what = "rbind")

# save(WEEKdata, file = "WEEKdata.rda")


########################################################################
# try diff stats features to select time period i.e. week
# RESULTS
# WEEKsum: 1,5,4,2,3,6,7
# WEEKmax: 4,6,3,2,5,7,1
# WEEKmin: all 0
# WEEKsd: 3,1,2,6,5,7,4
########################################################################

feature = "WEEKsum"
if(T){
  WEEKdata2 = WEEKdata %>% # only 7 weeks complete data
    filter(WEEKlength == 7) %>% select(SEQN,WEEKDAY,feature) %>%
    spread(key = WEEKDAY,value = feature)
  # colnames(WEEKdata2) = paste0("Week",colnames(WEEKdata2))
  WEEKdata2 = inner_join(data.frame(SEQN = Mortality_2011_C$SEQN,
                                    permth = Mortality_2011_C$permth_int %/% 12),
                         WEEKdata2,
                         by = "SEQN")
  WEEKdata2$permth = as.factor(WEEKdata2$permth)
  colnames(WEEKdata2) = c("SEQN","permth","Week1","Week2","Week3","Week4","Week5","Week6","Week7" )
}

y = WEEKdata2 %>% na.omit() %>%
  select(-SEQN)
# y.imputed = rfImpute(permth ~ ., data = y,iter = 20)
fit = randomForest(permth ~ ., data = y,ntree = 500,
                   importance = T)
# importance(fit)
varImpPlot(fit,main = feature)




# mortality
mortality_good_C = Mortality_2011_C %>% filter(eligstat == 1,mortstat == 1) %>% 
  mutate(permth = as.factor(permth_int %/% 12))

# health info
WEEKind = MINdata %>% group_by(SEQN) %>% summarise(WEEKlength = n()) %>% filter(WEEKlength==7)
WEEKind = WEEKind$SEQN
dataset = inner_join(MINdata %>% filter(SEQN %in% WEEKind),
                     Covariate_C %>% na.omit(),
                     by = "SEQN") %>%
  inner_join(mortality_good_C,by ="SEQN")
dataset = dataset[,-grep("mortsrce",colnames(dataset))]  %>% na.omit() %>% select(-c(permth_exm,permth_int))


y = dataset  %>% select(-SEQN) 
  # filter(WEEKDAY != 7)
# from RF results

# relevel - level 9 sometimes not included


view(y[1:10,1440:1470])
y = apply(dataset,MARGIN = 2,FUN = function(i){
  if(class(i) == "factor"| class(i) == "character"){as.numeric(as.factor(i))} else{return(i)}
})
y = prcomp(y %>% as.data.frame %>% select(-c(SEQN,permth)))

if(F){
  set.seed(111)
  trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
}
traindata = y[trainIdx,]
testdata = y[!trainIdx,]

# a = lapply(y, FUN = function(i){class(i)})
library(caret)
model = train(permth ~ ., data = traindata,
              method = "multinom")
pred = predict(model,testdata)
# results = data.frame(test = testdata$permth, prediction = pred)
table(testdata$permth,pred)
print(paste('Accuracy',sum(diag(table(testdata$permth_int,pred)))/sum(table(testdata$permth_int,pred))))




#### can-do idea ######

process Covariate_C
https://stackoverflow.com/questions/30052042/filter-factor-levels-in-r-using-dplyr

#### trash code#######


> dim(Covariate_C)
[1] 10122    23