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
  select(-WEEKDAY) %>% group_by(SEQN) %>% summarise_all(funs(sum)) %>% as.data.frame()


# 0: Assumed alive 
# 1: Assumed deceased
# NA: Under age 18, not available for public release or ineligible for mortality follow-up
analyticData = MINdata  %>% inner_join(mortality_good_C %>% select(SEQN,mortstat,permth_exm), by = "SEQN") %>% 
  filter(mortstat != "NA") %>%
  mutate(mortstat = ifelse(mortstat == 0,yes = NA, no = 1)) # re-encode mortstat for further processing

permth =  ((analyticData$mortstat * analyticData$permth_exm) %/% 12)
permth[permth < 5 ] <- 0
permth[permth >= 5] <- 1
permth[is.na(permth)] <- 2
# 0 for 0~5 years, 1 for 5~10 years, 2 for alive
analyticData$permth = permth

unique(analyticData$permth)
analyticData[1:10,1:10]
analyticData[1:5,(dim(analyticData)[2]-20):dim(analyticData)[2]]




##########################################################################################################################################
y = analyticData$permth%>% as.matrix()
x = analyticData %>% select(-permth,-SEQN,-mortstat,-permth_exm) %>% as.matrix()
set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(x)[1], replace = TRUE, prob = c(.7, .3))
ytrain = y[trainIdx, ]
xtrain = x[trainIdx, ] %>% scale()

mns = attr(xtrain, "scaled:center")
sds = attr(xtrain, "scaled:scale")

xtest = x[!trainIdx, ]  %>% scale(center = mns, scale = sds)
ytest = y[!trainIdx, ]

ytrain <- to_categorical(ytrain,3)
ytest <- to_categorical(ytest, 3)


model <- keras_model_sequential() %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = dim(xtrain)[2]) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
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