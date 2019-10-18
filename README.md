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
> 
>
> ```R
>fit = lm(BMI ~ .,data = y, subset = trainidx)
> fit2 = lm(BMI~. + RIDAGEYR:PC1,data = y, subset = trainidx)
>anova(fit,fit2)
> ```
>
> ```R
>> anova(fit,fit2)
> Analysis of Variance Table
>
> Model 1: BMI ~ RIDAGEYR + Race + Gender + PC1 + PC2 + PC3 + PC4 + PC5
>Model 2: BMI ~ RIDAGEYR + Race + Gender + PC1 + PC2 + PC3 + PC4 + PC5 + 
>     RIDAGEYR:PC1
>  Res.Df    RSS Df Sum of Sq     F    Pr(>F)    
> 1   4464 215.53                                 
>2   4463 206.29  1     9.235 199.8 < 2.2e-16 ***
> ---
>Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> ```
> 
> 
