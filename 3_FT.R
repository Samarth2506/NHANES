rm(list = ls())
if(T){
  library(rnhanesdata)
  library(tidyverse)
  library(magrittr)
  library(data.table)
  library(randomForest)
  library(caret)
  library(keras)
}

keep_inx <- exclude_accel(act = PAXINTEN_C, flags = Flags_C)
accel_good_C <- PAXINTEN_C[keep_inx,] 
flags_good_C <- Flags_C[keep_inx,]

SEQN_inx = (accel_good_C %>% group_by(SEQN) %>% 
              filter(length(WEEKDAY) == 7)  %>% as.data.frame 
            %>% select(SEQN) %>% unique())$SEQN



# 4878 patients assumed alive, 732 assumed deceased, 4512 assumed NA

mortality_temp = Mortality_2011_C %>% filter(mortstat == 0) %>% 
  select(SEQN,causeavl,ucod_leading,diabetes_mcod,hyperten_mcod,permth_exm)

mortality_good_C = Mortality_2011_C %>% filter(mortstat != "NA") %>%
  select(SEQN,causeavl,ucod_leading,diabetes_mcod,hyperten_mcod,permth_exm)

covariate_good_C = Covariate_C %>% select(SEQN,RIDAGEYR,
                                          BMI,BMI_cat,Race,Gender,Diabetes,CHF,CHD,Cancer,
                                          Stroke,EducationAdult,MobilityProblem,DrinkStatus,DrinksPerWeek,SmokeCigs)


MINdata.flags = data.frame(accel_good_C[,1:5],accel_good_C[,6:dim(accel_good_C)[2]] * flags_good_C[,6:dim(accel_good_C)[2]]) %>% 
  select(-c(PAXCAL,PAXSTAT,SDDSRVYR)) %>% 
  # filter(SEQN %in% SEQN_inx) %>% as.data.frame() %>%
  select(-WEEKDAY) %>% group_by(SEQN) %>% summarise_all(funs(sum)) %>% as.data.frame()

analyticData = MINdata.flags %>%
  inner_join(covariate_good_C %>% select(SEQN,RIDAGEYR),by = "SEQN") %>%
  inner_join(mortality_good_C %>% select(SEQN,permth_exm),by = "SEQN") 

unique(analyticData$permth_exm)
analyticData[1:10,1:10]
analyticData[1:5,(dim(analyticData)[2]-20):dim(analyticData)[2]]
