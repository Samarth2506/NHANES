if(T){
  rm(list = ls())
  library(rnhanesdata)
  library(tidyverse)
  library(magrittr)
}

keep_inx <- exclude_accel(act = PAXINTEN_C, flags = Flags_C)
accel_good_C <- PAXINTEN_C[keep_inx,]
flags_good_C <- Flags_C[keep_inx,]

#reweight_accel()

# Covariate_C %>% na.omit()

data_C = inner_join(Mortality_2011_C[!is.na(Mortality_2011_C$permth_int),] %>% select(c("SEQN","permth_int")),
                    accel_good_C[,grep(c("SEQN|WEEKDAY|MIN"),colnames(accel_good_C))],
                    by = "SEQN") %>% na.omit()

# library(zoo)
# data_C = data_C %>% t() %>% na.locf() %>% t() #autofill the missing value from the previous left row data


#mean of signal
temp = by(data_C, INDICES = data_C$SEQN, FUN = function(i){
  colMeans(i %>% select(-c(SEQN,WEEKDAY,permth_int)))
}) %>%
  do.call(what = "rbind") #convert to data frame

temp2 = data.frame(SEQN = rownames(do.call(rbind,temp)) %>% as.integer(), do.call(rbind,temp))
temp3 = left_join(unique(data_C[,c("SEQN","permth_int")]),temp2,by = "SEQN") %>% na.omit()

# signal and mortality
temp4 = Covariate_C %>% select(SEQN,WTMEC2YR,RIDAGEYR,BMI,BMI_cat, Race, Gender,
                       Diabetes,CHF,CHD,Cancer,Stroke,MobilityProblem,DrinkStatus,DrinksPerWeek,SmokeCigs) %>%
  na.omit()

# health info
temp5 = left_join(temp4,temp3,by = "SEQN") %>% na.omit() %>% as.data.frame() %>% 
  filter(Stroke != "Don't know")
#convert factor to numeric
temp5 = apply(temp5,MARGIN = 2,FUN = function(i){
  as.numeric(as.factor(i))
})

#final dataframe
temp6 = temp5  %>% as.data.frame() %>% select(-c(SEQN,permth_int))


pca_C = prcomp(temp6,scale. = T)


# screeplot(pca_C)
library(factoextra)
fviz_screeplot(pca_C,addlabels = TRUE)



# prediction
y = data.frame(permth_int = as.integer(temp5[,"permth_int"]/12) %>% as.factor(),
               pca_C$x)
trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
traindata = y[trainIdx,]
testdata = y[!trainIdx,]
library(caret)
model = train(permth_int ~ PC1+PC2+PC3, data = traindata,
              method = "svmLinear")
pred = predict(model,testdata)
results = data.frame(test = testdata$permth_int, prediction = pred)
table(testdata$permth_int,pred)
print(paste('Accuracy',table(results$test == results$prediction)["TRUE"]/length(results$test)))

