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
WEEKdata = split(MINdata[,MINnames],MINdata$SEQN) %>% lapply(FUN = function(i){
  a = rowSums(i)
  data.frame(WeekLengths = length(rownames(i)),Mean = mean(a), Sd = sd(a), Max = max(a), Min = min(a))
}) %>% 
  do.call(what = "rbind")

# only 7 weeks complete data
WEEKdata = WEEKdata %>% filter(WeekLengths == 7)



#### trash code#######


> dim(Covariate_C)
[1] 10122    23