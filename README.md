# rnhanesdata

> Source 
> https://github.com/andrew-leroux/rnhanesdata 
> https://wwwn.cdc.gov/Nchs/Nhanes/

**Timeline**

### July 

> Some typical ML and DL method to predict conditional survival analysis: **Maxima of Pr(years left to live|assuming die in 10 years)** 
>
> 
>
> For next, add a new category representing patients not dying in 10 years in neural network: **Maxima of Pr(years left to live)**
>

### August

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
>     0  51 245
>     1 118 826
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
> -5.1519   0.1886   0.3789   0.5665   1.1851  
> 
> Coefficients:
>              Estimate Std. Error z value Pr(>|z|)    
> (Intercept)  2.485661   0.087662  28.355  < 2e-16 ***
> PC1          0.094724   0.006817  13.895  < 2e-16 ***
> PC2         -0.055732   0.010354  -5.383 7.33e-08 ***
> PC3         -0.064101   0.009514  -6.737 1.61e-11 ***
> ---
> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> 
> (Dispersion parameter for binomial family taken to be 1)
> 
>     Null deviance: 2236.5  on 2924  degrees of freedom
> Residual deviance: 1921.7  on 2921  degrees of freedom
> AIC: 1929.7
> 
> Number of Fisher Scoring iterations: 6
> ```
>
>
> ```R
> > ptab
>      
> yPred    0    1
>     0    0    1
>     1  171 1082
> > sum(diag(ptab)) / sum(ptab)
> [1] 0.8628389
> ```
>
> 
>
> 
>
> **logistic ridge regression**
>
> prevent overfitting and underfitting
>
> lambda.1se
>
> ```R
> > ptab
>      ytru
> ytest    0    1
>     0    0  168
>     1    0 1072
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
>      ytru
> ytest    0    1
>     0    0  168
>     1    0 1072
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
