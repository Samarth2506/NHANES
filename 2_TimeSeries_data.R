rm(list = ls())
if(T){
  library(rnhanesdata)
  library(tidyverse)
  library(magrittr)
  library(data.table)
  library(randomForest)
}

########################################################################
# data preprocess/ get valid data
########################################################################

keep_inx <- exclude_accel(act = PAXINTEN_C, flags = Flags_C)
accel_good_C <- PAXINTEN_C[keep_inx,] %>% na.omit()
flags_good_C <- Flags_C[keep_inx,] %>% na.omit()

# ranges of SDDSRVYR are 3 3 in all data frame we use i.e. 2003-2004 wave
# sum(accel_good_C[,1:5] != flags_good_C[,1:5]) = 0 
# non-wearable and wearble
MINdata = data.frame(accel_good_C[,1:5],accel_good_C[,6:dim(accel_good_C)[2]] * flags_good_C[,6:dim(accel_good_C)[2]]) %>% 
  select(-c(PAXCAL,PAXSTAT,SDDSRVYR))

# week level data
MINnames = colnames(MINdata)[3:dim(MINdata)[2]]
# WEEKdata = MINdata %>% group_by(SEQN,WEEKDAY) %>%
#   summarise()

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

# level 9 sometimes not included


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