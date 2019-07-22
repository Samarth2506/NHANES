rm(list = ls())
if(T){
  library(rnhanesdata)
  library(tidyverse)
  library(magrittr)
  library(data.table)
}

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
WEEKdata = split(MINdata,MINdata$SEQN) %>% lapply(FUN = function(i){
    WEEKstats = apply(i[,MINnames],1,function(r){
      sum(r)
    })
    data.frame(i[,1:2],WEEKlength = length(i[,"WEEKDAY"]),WEEKstats = WEEKstats)
  # WEEKmean = rowSums(i[,MINnames])/length(MINnames)
  # data.frame(SEQN = unique(i[,"SEQN"]),
  #            WeekLengths = length(rownames(i)),
  #            Mean = mean(WEEKmean), #averaging mean of week-level activity
  #            Sd = sd(WEEKmean), 
  #            Max = max(WEEKmean), 
  #            Min = min(WEEKmean))
}) %>% 
  do.call(what = "rbind")


WEEKdata2 = WEEKdata %>% # only 7 weeks complete data
  filter(WEEKlength == 7) %>% 
  spread(key = WEEKDAY,value = WEEKstats)
# colnames(WEEKdata2) = paste0("Week",colnames(WEEKdata2))
WEEKdata2 = inner_join(data.frame(SEQN = Mortality_2011_C$SEQN,
                                  permth = Mortality_2011_C$permth_int %/% 12),
                       WEEKdata2,
                       by = "SEQN")





WEEKdata2$permth = as.factor(WEEKdata2$permth)
colnames(WEEKdata2) = c("SEQN","permth","WEEKlength","Week1","Week2","Week3","Week4","Week5","Week6","Week7" )
y = WEEKdata2 %>% na.omit() %>%
  select(-c(SEQN,WEEKlength))
library(randomForest)
# y.imputed = rfImpute(permth ~ ., data = y,iter = 20)
fit = randomForest(permth ~ ., data = y,ntree = 1000,
                   importance = T)
importance(fit)
varImpPlot(fit)
# mortality

mortality_good_C = Mortality_2011_C %>% filter(eligstat == 1,mortstat == 1) %>% 
  mutate(permth = as.factor(permth_int %/% 12))

# health info
dataset = inner_join(WEEKdata,Covariate_C %>% na.omit(),by = "SEQN") %>%
  inner_join(mortality_good_C,by ="SEQN")


dataset = dataset[,-grep("mortsrce",colnames(dataset))] %>% select(-SEQN) %>% mutate()



set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
traindata = y[trainIdx,]
testdata = y[!trainIdx,]




#### can-do ######

process Covariate_C
https://stackoverflow.com/questions/30052042/filter-factor-levels-in-r-using-dplyr

#### trash code#######


> dim(Covariate_C)
[1] 10122    23