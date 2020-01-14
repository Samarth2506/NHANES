library(rnhanesdata)
library(tidyverse)


rm(list = ls())


###################################################
# preprocess

# select 'good' data
keep_inx <- exclude_accel(act = PAXINTEN_D, flags = Flags_D)
accel_good_D <- PAXINTEN_D[keep_inx,] 
flags_good_D <- Flags_D[keep_inx,]


# participants' ID wearing monitor for 7 consecutive days
SEQN_7days = data.frame(SEQN = accel_good_D[,'SEQN'],accel_good_D[,MIN_name] * flags_good_D[,MIN_name]) %>% 
  group_by(SEQN) %>% summarise(num = n()) %>% filter(num==7) %>% select(SEQN)


# dot multiply bwtween flag matrix and activity count matrix 
# average over days
MIN_name = grep('MIN',colnames(accel_good_D),value = T)
MINdata = data.frame(SEQN = accel_good_D[,'SEQN'],accel_good_D[,MIN_name] * flags_good_D[,MIN_name]) %>%
  group_by(SEQN) %>% summarise_each(funs(mean)) %>% 
  filter(SEQN %in% SEQN_7days$SEQN) %>% na.omit()


# pincipal component analysis
if(F){
  pca_model = prcomp(MINdata %>% select(-SEQN),
                     center = T,
                     scale. = T)
  save(pca_model,file= 'pca_model.rda')
}
load(file = 'pca_model.rda')

# include participants' correspondent covariates with PC loadings 
analyticData = data.frame(SEQN=MINdata$SEQN,
                          pca_model$x) %>% 
  inner_join(Covariate_D %>% select(SEQN,Race,Gender,RIDAGEYR,BMI),by = 'SEQN') %>% 
  na.omit()

# > quantile(analyticData$BMI,probs = 0.05)
#      5% 
# 16.238 
# > quantile(analyticData$BMI,probs = 0.95)
#    95% 
# 37.563
# omit outliers
analyticData = analyticData %>% filter(15 <BMI & BMI< 40)
save(analyticData, file = 'analyticData.rda')

###################################################

rm(list = ls())
load(file = 'analyticData.rda')
Y = analyticData
PCnames = paste('PC',1:5,sep = '')
y = analyticData[,c('RIDAGEYR','Race','Gender','BMI',PCnames)] %>% na.omit()
colnames(y)
if(F){
  fit = randomForest(BMI ~ ., data = y,ntree = 501,
                     importance = T)
  save(fit,file = 'rffit.rda')
}
load(file = 'rffit.rda')
varImpPlot(fit)

importance(fit)

y_cor <- as.data.frame(
  lapply(y, function (x) if (is.factor(x)) unclass(x) %>% as.numeric  else x))

res_cor = cor(y_cor)
library(corrplot)
corrplot(res_cor,
         metho = 'color',
         type = 'upper',
         order = 'hclust',
         addCoef.col = "black",
         diag = F)







