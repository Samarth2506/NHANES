# rnhanesdata

> Source 
> https://github.com/andrew-leroux/rnhanesdata 
> https://wwwn.cdc.gov/Nchs/Nhanes/

**Timeline**

# July

> Some typical ML and DL method to predict conditional survival analysis: **Maxima of Pr(years left to live|assuming die in 10 years)** 
>
> 
>
> For next, add a new category representing patients not dying in 10 years in neural network: **Maxima of Pr(years left to live)**
>

# August

> **New dataset 2005~2006** 
>
> manipulate between mortstat and permth_exm to get target category
>
> **Fourier transform**  
>
> get spectrum and feed into neural network
> Spectrum()$spec
>
> 
>
> High ACC but plausible
>
> `0: 0~5 years 1: 5~10 years 2: alive`
>
> ```R
> ytest    2
> 0   88
> 1   82
> 2 1072
> 
> $loss
> [1] 2.206181
> 
> $acc
> [1] 0.863124
> ```
>
> 
>
> **PCA**
>
> Raw patient data
>
> | patient idx | mortality | Age  |
> | :---------: | :-------: | :--: |
> |      1      |     0     |  65  |
> |      2      |     1     |  54  |
>
> Re-encode to the following format and feed into PCA (add Gender/Covariate stuff)
>
> | patient idx | year idx | mortality | Age  |
> | :---------: | :------: | :-------: | :--: |
> |      1      |    1     |     0     |  65  |
> |      1      |    2     |     0     |  65  |
> |      1      |    3     |     0     |  65  |
> |      1      |    4     |     0     |  65  |
> |      2      |    1     |     0     |  54  |
> |      2      |    2     |     1     |  54  |
>
> 
>
> **Logistic regression based on average over days**
>
> ```R
> > ptab
>   
> yPred   0   1
>  0  51 245
>  1 118 826
> > sum(diag(ptab)) / sum(ptab)
> [1] 0.7072581
> ```
>
> over-fitting problem 
>
> **Logistic regression based on PC scores**
>
> ```R
> > summary(fit)
> 
> Call:
> glm(formula = mortstat ~ ., family = "binomial", data = y, subset = trainIdx)
> 
> Deviance Residuals: 
>     Min       1Q   Median       3Q      Max  
> -5.2579   0.1785   0.3708   0.5720   1.2018  
> 
> Coefficients:
>              Estimate Std. Error z value Pr(>|z|)    
> (Intercept)  2.522431   0.089855  28.072  < 2e-16 ***
> PC1          0.096047   0.006827  14.069  < 2e-16 ***
> PC2         -0.054783   0.010307  -5.315 1.07e-07 ***
> PC3         -0.068555   0.010050  -6.822 9.00e-12 ***
> PC4         -0.039612   0.017162  -2.308  0.02099 *  
> PC5          0.012553   0.015650   0.802  0.42248    
> PC6          0.012767   0.020076   0.636  0.52480    
> PC7          0.054179   0.021027   2.577  0.00998 ** 
> PC8         -0.011252   0.021716  -0.518  0.60437    
> PC9         -0.027085   0.020222  -1.339  0.18045    
> ---
> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> (Dispersion parameter for binomial family taken to be 1)
> 
>     Null deviance: 2236.5  on 2924  degrees of freedom
> Residual deviance: 1908.2  on 2915  degrees of freedom
> AIC: 1928.2
> 
> Number of Fisher Scoring iterations: 6
> ```
>
>
> ```R
> > ptab
>    
>        0    1
>   0    0  171
>   1    2 1081
> > sum(diag(ptab)) / sum(ptab)
> [1] 0.8620415
> ```
>
> 
>
> 
>
> **logistic ridge regression**
>
> glmnet
>
> prevent overfitting and underfitting
>
> lambda.1se
>
> ```R
> > ptab
>   ytru
> ytest    0    1
>  0    0  168
>  1    0 1072
> > sum(diag(ptab)) / sum(ptab)
> [1] 0.8645161
> ```
>
> 
>
> lambda.min
>
> ```R
> > ptab
>   ytru
> ytest    0    1
>  0    0  168
>  1    0 1072
> > sum(diag(ptab)) / sum(ptab)
> [1] 0.8645161
> ```
>
> 
>
> >**convert 7 * 1440 matrix  to 1 * 10080 data note its 7 days complete data**
> >
> >**prob: data doesn't line up so might not work** 
>
> 
>
> **pick BMI as target**
>
> visualization of linear regression in ggplot2
>
> check reports

# September

> **Reselect features: treat activities and covariates separately because they are not in same units**
>
> | PCs  | Age,gender,etc.  |
> | ---- | ---------------- |
> |      | Maybe PCA again? |
>
> Just checked the code and found that's exactly what I did before but didn't include Covariate_D
>
> 
>
> Summary of model?
>
> Regularization? Ridge? Lasso?
>
> BMI threshold
>
> Double check results: Same SEQN for each patient
>
> 
>
> **PC scores vs. BMI**
>
> 
>
> ```R
> load(file = 'pca.rda')
> temp = pca$rotation
> plot(( 1 : nrow(temp)) / 60, temp[,1],type = 'l')
> plot(( 1 : nrow(temp)) / 60, temp[,2],type = 'l')
> plot(( 1 : nrow(temp)) / 60, temp[,3],type = 'l')
> plot(( 1 : nrow(temp)) / 60, temp[,4],type = 'l')
> ```
>
> 
>
> ```R
> rm(list = ls())
> load(file ='BMI.rda')
> data = pcscore  %>%
> left_join(Covariate_D[,c('SEQN','RIDAGEYR','Gender','Race','BMI_cat')],by = 'SEQN')
> data = data %>% select(c(1:5),BMI,Race)
> 
> 
> library(ggplot2)
> 
> ggplot(data)+
> aes(x = BMI,y = PC1, colour = Race, group = Race) + 
> geom_line()
> 
> ggplot(data)+
> aes(x = BMI,y = PC3, colour = Race, group = Race) + 
> geom_line()
> ```
>
> 
>
> ```R
> Call:
> lm(formula = log ~ . - BMI_cat, data = y, subset = trainidx)
> 
> Residuals:
> Min       1Q   Median       3Q      Max 
> -0.72461 -0.13707 -0.00772  0.12928  0.60376 
> 
> Coefficients:
>         Estimate Std. Error t value Pr(>|t|)    
> (Intercept)  3.2984489  0.0132763 248.446  < 2e-16 ***
> PC1         -0.0011646  0.0003092  -3.767 0.000169 ***
> PC2          0.0005029  0.0003108   1.618 0.105734    
> PC3          0.0010938  0.0004029   2.715 0.006671 ** 
> PC4         -0.0003430  0.0005964  -0.575 0.565271    
> PC5          0.0010227  0.0006836   1.496 0.134747    
> RIDAGEYR     0.0007831  0.0002173   3.603 0.000319 ***
> Gender2      0.0102633  0.0078261   1.311 0.189821    
> Race2        0.0278941  0.0100144   2.785 0.005381 ** 
> Race3        0.0146841  0.0219751   0.668 0.504050    
> Race4        0.0518277  0.0094653   5.476 4.74e-08 ***
> Race5       -0.0129902  0.0187947  -0.691 0.489520    
> ---
> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> Residual standard error: 0.1987 on 2869 degrees of freedom
> Multiple R-squared:  0.03121,	Adjusted R-squared:  0.0275 
> F-statistic: 8.403 on 11 and 2869 DF,  p-value: 1.007e-14
> 
> [1] 35.86605
> ```
>
> 
>
> **Smooth data before PCA: Random forest importance plot**
>
> can be over-smoothed if the %IncMSE != IncNodePurity
>
> Cross-validation
>
> **Nutrition info included / literature**

# October

> ![random_forest_before_smooth](https://github.com/LuchaoQi/rnhanesdata/blob/master/reports/random_forest_before_smooth.png?raw=true)
>
> ![residuals](https://github.com/LuchaoQi/rnhanesdata/blob/master/reports/residuals.png?raw=true)
>
> **interaction term**
>
> ```R
> fit = lm(BMI~. + PC1:RIDAGEYR + PC1:Race + PC1:Gender + RIDAGEYR:Race,
>    data = y, subset = trainidx)
> fit2 = lm(BMI ~ .,data = y, subset = trainidx)
> anova(fit,fit2)
> ```
>
> ```R
> Analysis of Variance Table
> 
> Model 1: BMI ~ RIDAGEYR + Race + Gender + PC1 + PC2 + PC3 + PC4 + PC5 + 
> PC1:RIDAGEYR + PC1:Race + PC1:Gender + RIDAGEYR:Race
> Model 2: BMI ~ RIDAGEYR + Race + Gender + PC1 + PC2 + PC3 + PC4 + PC5
> Res.Df    RSS  Df Sum of Sq      F    Pr(>F)    
> 1   4454 203.52                                   
> 2   4464 215.53 -10   -12.008 26.279 < 2.2e-16 ***
> ---
> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> Call:
> lm(formula = BMI ~ . + PC1:RIDAGEYR + PC1:Race + PC1:Gender + 
> RIDAGEYR:Race, data = y, subset = trainidx)
> 
> Residuals:
> Min       1Q   Median       3Q      Max 
> -0.60949 -0.14959 -0.01866  0.12842  0.87540 
> 
> Coefficients:
>                           Estimate Std. Error t value Pr(>|t|)    
> (Intercept)                    3.146e+00  1.212e-02 259.539  < 2e-16 ***
> RIDAGEYR                       3.695e-03  2.682e-04  13.776  < 2e-16 ***
> RaceMexican American           8.251e-04  1.630e-02   0.051  0.95964    
> RaceOther Hispanic            -8.465e-03  3.752e-02  -0.226  0.82150    
> RaceBlack                      9.035e-03  1.670e-02   0.541  0.58859    
> RaceOther                     -1.328e-01  3.038e-02  -4.371 1.26e-05 ***
> GenderFemale                  -7.938e-03  6.697e-03  -1.185  0.23602    
> PC1                            1.041e-02  7.142e-04  14.574  < 2e-16 ***
> PC2                           -1.079e-05  2.881e-04  -0.037  0.97012    
> PC3                            5.749e-04  3.781e-04   1.520  0.12847    
> PC4                           -4.402e-04  5.619e-04  -0.783  0.43343    
> PC5                           -1.392e-03  5.739e-04  -2.426  0.01530 *  
> RIDAGEYR:PC1                  -2.069e-04  1.483e-05 -13.951  < 2e-16 ***
> RaceMexican American:PC1      -2.196e-03  7.718e-04  -2.845  0.00446 ** 
> RaceOther Hispanic:PC1        -4.713e-03  1.725e-03  -2.732  0.00633 ** 
> RaceBlack:PC1                 -1.336e-03  7.724e-04  -1.729  0.08386 .  
> RaceOther:PC1                 -3.649e-03  1.414e-03  -2.581  0.00989 ** 
> GenderFemale:PC1               3.020e-03  5.483e-04   5.508 3.84e-08 ***
> RIDAGEYR:RaceMexican American  1.148e-03  4.289e-04   2.677  0.00745 ** 
> RIDAGEYR:RaceOther Hispanic    1.367e-03  1.041e-03   1.314  0.18906    
> RIDAGEYR:RaceBlack             1.486e-03  4.267e-04   3.483  0.00050 ***
> RIDAGEYR:RaceOther             2.671e-03  8.929e-04   2.991  0.00279 ** 
> ---
> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> Residual standard error: 0.2138 on 4454 degrees of freedom
> Multiple R-squared:  0.2971,	Adjusted R-squared:  0.2938 
> F-statistic: 89.67 on 21 and 4454 DF,  p-value: < 2.2e-16
> 
> # MSE
> [1] 42.30455
> 
> ```
>
> 
>
> **No interaction**
>
> ```R
> 
> Call:
> lm(formula = BMI ~ ., data = y, subset = trainidx)
> 
> Residuals:
>     Min      1Q  Median      3Q     Max 
> -0.6736 -0.1545 -0.0176  0.1339  0.8491 
> 
> Coefficients:
>                        Estimate Std. Error t value Pr(>|t|)    
> (Intercept)           3.135e+00  9.875e-03 317.428  < 2e-16 ***
> RIDAGEYR              3.138e-03  1.786e-04  17.571  < 2e-16 ***
> RaceMexican American  4.421e-02  8.669e-03   5.100 3.54e-07 ***
> RaceOther Hispanic    4.600e-02  1.979e-02   2.325  0.02013 *  
> RaceBlack             5.592e-02  8.497e-03   6.581 5.21e-11 ***
> RaceOther            -5.031e-02  1.588e-02  -3.168  0.00154 ** 
> GenderFemale         -4.393e-03  6.862e-03  -0.640  0.52208    
> PC1                   5.745e-03  3.143e-04  18.278  < 2e-16 ***
> PC2                  -1.165e-05  2.957e-04  -0.039  0.96856    
> PC3                   2.126e-03  3.731e-04   5.699 1.28e-08 ***
> PC4                  -2.462e-03  5.560e-04  -4.428 9.74e-06 ***
> PC5                  -2.430e-03  5.816e-04  -4.179 2.99e-05 ***
> ---
> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> Residual standard error: 0.2197 on 4464 degrees of freedom
> Multiple R-squared:  0.2557,	Adjusted R-squared:  0.2538 
> F-statistic: 139.4 on 11 and 4464 DF,  p-value: < 2.2e-16
> 
> [1] 43.46444
> ```
>
> 
>
> **Try different window size**
>
> ```{r}
> if(F){
> err = rep(0,10)
> for (d in 1:10){
> MINdata_smooth = MINdata[,MIN_name] %>%
> apply(MARGIN = 2, function(i) runmean(i,k=d))
> MINdata_smooth = data.frame(SEQN = MINdata$SEQN,MINdata_smooth)
> pca_smooth = prcomp(MINdata_smooth %>% select(-SEQN),
>               center = T,
>               scale. = T)
> 
> y = data.frame(SEQN= MINdata_smooth$SEQN,pca_smooth$x[,1:5]) %>% 
> left_join(Covariate_D[,c('RIDAGEYR','Race','Gender','BMI','SEQN')],by = 'SEQN')
> y = y[,c('RIDAGEYR','Race','Gender','BMI','PC1','PC2','PC3','PC4','PC5')] %>% na.omit()
> 
> yTrue = y$BMI
> 
> y$BMI = log(y$BMI+1)
> 
> fit = lm(BMI ~ .,data = y, subset = trainidx)
> summary(fit)
> 
> yPred = exp(predict(fit,y[-trainidx,]))-1
> yTrue = yTrue[-trainidx]
> result = cbind(yPred,
>             yTrue) %>% as.data.frame() %>% na.omit()
> 
> 
> err[d] = mean((result[,1]-result[,2])^2)
> }
> err
> }
> # > err
> #  [1] 47.93726 49.89128 50.71071 51.17472 51.32988 51.30029 51.16800 51.16298 51.10647
> # [10] 51.17049
> ```
