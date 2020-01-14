library(rnhanesdata)
library(tidyverse)


rm(list = ls())

keep_inx <- exclude_accel(act = PAXINTEN_D, flags = Flags_D)
accel_good_D <- PAXINTEN_D[keep_inx,] 
flags_good_D <- Flags_D[keep_inx,]


SEQN_7days = data.frame(SEQN = accel_good_D[,'SEQN'],accel_good_D[,MIN_name] * flags_good_D[,MIN_name]) %>% 
  group_by(SEQN) %>% summarise(num = n()) %>% filter(num==7) %>% select(SEQN)

MIN_name = grep('MIN',colnames(accel_good_D),value = T)
MINdata = data.frame(SEQN = accel_good_D[,'SEQN'],accel_good_D[,MIN_name] * flags_good_D[,MIN_name]) %>%
  group_by(SEQN) %>% summarise_each(funs(mean)) %>% 
  filter(SEQN %in% SEQN_7days$SEQN) %>% na.omit()



if(T){
  pca_model = prcomp(MINdata %>% select(-SEQN),
                     center = T,
                     scale. = T)
  save(pca_model,file= 'pca_model.rda')
}
load(file = 'pca_model.rda')
analyticData = data.frame(SEQN=MINdata$SEQN,
                          pca_model$x) %>% 
  inner_join(Covariate_D %>% select(SEQN,Race,Gender,RIDAGEYR,BMI),by = 'SEQN') %>% 
  na.omit()
