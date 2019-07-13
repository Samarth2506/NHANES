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

# added mortality
temp = left_join(unique(data_C[,c("SEQN","permth_int")]),temp,by = "SEQN") %>% na.omit()

if(F){
  save(temp,file = 'temp.rda')
}

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
# 0   0   1   0   1   4   0   0   5   8   0
# 1   0   4   1   0   0   0   0   4   3   0
# 2   0   2   1   0   2   2   0  11   9   0
# 3   0   2   0   1   1   1   0   2   9   0
# 4   0   0   1   0   0   0   0   8   9   0
# 5   0   1   0   0   1   0   0  10   4   0
# 6   0   0   0   0   0   0   1   8  10   0
# 7   2  18   8  10   9  12  16 248 260   5
# 8   2   8   5   9  10  13  11 247 224   1
# 9   0   0   1   0   0   0   0   4   6   0

}

# all data including health info 
y = data.frame(permth_int = as.integer(temp3[,"permth_int"]/12) %>% as.factor(),
               pca_C$x)

# > table(testdata$permth_int,pred)
# pred
#     0   1   2   3   4   5   6   7   8   9
# 0   0   1   0   0   0   0   0   7   2   0
# 1   0   1   0   0   0   0   0   9   4   0
# 2   0   1   0   1   2   0   1  12   8   0
# 3   1   0   0   0   0   0   0  11   3   0
# 4   0   0   1   0   0   1   0  11   8   0
# 5   0   0   1   1   0   2   0   6   8   0
# 6   0   0   0   1   0   0   0   6   7   0
# 7   1   6   5   1   8   5   1 212 209   0
# 8   2   6   3   3   7   5   5 220 228   0

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




# just temp2 i.e. raw data
y = data.frame(permth_int = as.integer(temp2$permth_int/12) %>% as.factor(), temp2 %>% select(-permth_int))
# > table(testdata$permth_int,pred)
# pred
#     0   1   2   3   4   5   6   7   8   9
# 0   0   1   0   0   0   0   0   6   3   0
# 1   2   1   0   0   1   0   0   7   3   0
# 2   0   2   0   2   1   0   0  11   9   0
# 3   2   1   0   0   1   0   0   6   5   0
# 4   0   2   0   0   0   1   0  10   8   0
# 5   1   1   0   1   2   1   0   5   7   0
# 6   0   0   1   0   0   0   0   5   8   0
# 7   6   9   4   6  14   5   9 220 172   3
# 8   6   3   3   6  20   5  15 223 192   6
# 9   1   0   0   0   0   0   0   4   4   0
