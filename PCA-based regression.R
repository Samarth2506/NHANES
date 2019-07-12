if(T){
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


# mean of signal by day
temp = by(data_C, INDICES = data_C$SEQN, FUN = function(i){
  colMeans(i %>% select(-c(SEQN,WEEKDAY,permth_int)))
}) %>%
  do.call(what = "rbind") #convert to data frame

temp = data.frame(SEQN = rownames(temp) %>% as.integer(), temp)
# save(temp2,file = "mean.rda")

# added mortality
temp = left_join(unique(data_C[,c("SEQN","permth_int")]),temp,by = "SEQN") %>% na.omit()

# added health info
healthinfo = Covariate_C %>% select(SEQN,WTMEC2YR,RIDAGEYR,BMI,BMI_cat, Race, Gender,
                       Diabetes,CHF,CHD,Cancer,Stroke,MobilityProblem,DrinkStatus,DrinksPerWeek,SmokeCigs) %>%
  na.omit()

temp2 = left_join(healthinfo,temp,by = "SEQN") %>% na.omit() %>% as.data.frame() %>% 
  filter(Stroke != "Don't know")

#convert factor to numeric
temp3 = lapply(temp2,FUN = function(i){
  if(class(i) == "factor"){as.numeric(as.factor(i))} else{return(i)}
}) %>% do.call(what = "cbind")



pca_C = prcomp(temp3 %>% as.data.frame %>% select(-c(SEQN,permth_int)),scale. = T)


# screeplot(pca_C)
library(factoextra)
fviz_screeplot(pca_C,addlabels = TRUE)



# prediction

# only wave count
if(F){
y = data.frame(permth_int = as.integer(temp[,"permth_int"]/12) %>% as.factor(),
               temp %>% select(-SEQN,-permth_int))

# > table(testdata$permth_int,pred)
# pred
#     0   1   2   3   4   5   6   7   8   9
# 0   0   1   1   0   1   0   0   7   9   0
# 1   2   1   0   0   1   1   0   7   4   0
# 2   0   1   1   0   0   1   0  13   2   0
# 3   0   2   0   0   2   0   0   9   5   0
# 4   0   2   1   1   0   0   0  12  11   0
# 5   0   1   0   0   0   0   0  12   4   0
# 6   0   1   2   1   1   0   0   9   5   0
# 7   4   8  11   7  12   9  11 298 215   4
# 8   4   3   3  10  11   8   9 272 233   3
# 9   0   0   1   0   0   0   0  11   4   0

}

# all data including health info 
y = data.frame(permth_int = as.integer(temp3[,"permth_int"]/12) %>% as.factor(),
               pca_C$x)

# > table(testdata$permth_int,pred)
# pred
#     0   1   2   3   4   5   6   7   8   9
# 0   0   0   1   0   0   0   0   7   5   0
# 1   0   0   0   0   0   1   0   9   6   0
# 2   0   1   1   0   1   0   1  11   3   0
# 3   0   0   0   0   1   0   1   8   6   0
# 4   0   0   2   0   3   1   0   7  12   0
# 5   0   2   2   0   1   1   0   8   8   0
# 6   0   0   1   0   0   0   0  14   6   0
# 7   1   5   4   2   7   6   2 234 208   0
# 8   0   4   4   0   6   3   3 235 211   0
# 9   0   0   0   0   0   0   0   9   2   0

set.seed(100)
trainIdx = sample(c(TRUE, FALSE), dim(y)[1], replace = TRUE, prob = c(.7, .3))
traindata = y[trainIdx,]
testdata = y[!trainIdx,]

library(caret)
model = train(permth_int ~ ., data = traindata,
              method = "svmLinear")
pred = predict(model,testdata)
results = data.frame(test = testdata$permth_int, prediction = pred)
table(testdata$permth_int,pred)
print(paste('Accuracy',table(results$test == results$prediction)["TRUE"]/length(results$test)))

