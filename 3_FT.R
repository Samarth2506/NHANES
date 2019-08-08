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

SEQN_inx = (accel_good_C %>% group_by(SEQN) %>% 
              filter(length(WEEKDAY) == 7)  %>% as.data.frame 
            %>% select(SEQN) %>% unique())$SEQN



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
  select(-WEEKDAY) %>% group_by(SEQN) %>% summarise_each(funs(sum)) %>% as.data.frame()

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
rm(list = ls())
load(file = "analyticData.rda")
permth =  ((analyticData$mortstat * analyticData$permth_exm) %/% 12)
table(permth, useNA = "ifany")
permth[permth < 5 ] <- 0
permth[permth >= 5] <- 1
permth[is.na(permth)] <- 2
# 0 for 0~5 years, 1 for 5~10 years, 2 for alive
analyticData$permth = permth %>% as.factor()
analyticData = analyticData %>% select(-mortstat,-permth_exm)
unique(analyticData$permth)
analyticData[1:10,1:10]
analyticData[1:5,(dim(analyticData)[2]-15):dim(analyticData)[2]]

temp = analyticData[1,] %>% select(-SEQN,-permth) %>% as.numeric() %>% fft()
spectrum(temp)

if(F){
  xs <- seq(-2*pi,2*pi,pi/100)
  wave.1 <- sin(3*xs)
  wave.2 <- sin(10*xs)
  wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
  spectrum(wave.3)
  wave.3 %>% fft()
}

if(F){
  plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
    plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
    
    # TODO: why this scaling is necessary?
    plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
    
    plot(plot.data, t="h", lwd=2, main="", 
         xlab="Frequency (Hz)", ylab="Strength", 
         xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
  }
  
}
plot.frequency.spectrum(temp)


a = apply(analyticData,1,FUN = function(i){
  data = i %>% select(-SEQN,-permth) %>% as.numeric()
  fft(data)
}) %>% t()

a = data.frame(a,permth = analyticData$permth)

y = a$permth %>% as.matrix()
x = a %>% select(-permth)
trainIdx = sample(c(TRUE, FALSE), dim(x)[1], replace = TRUE, prob = c(.7, .3))
ytrain = y[trainIdx, ]
xtrain = x[trainIdx, ]

xtest = x[!trainIdx, ]
ytest = y[!trainIdx, ]

ytrain <- to_categorical(ytrain,3)
ytest <- to_categorical(ytest, 3)


model <- keras_model_sequential() %>% 
  layer_dense(units = 128, activation = 'relu', input_shape = dim(xtrain)[2]) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 64, activation = 'relu') %>%
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

model %>% predict_classes(xtest)








##########################################################################################################################################

