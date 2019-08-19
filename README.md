# rnhanesdata

> Source 
> https://github.com/andrew-leroux/rnhanesdata 
> https://wwwn.cdc.gov/Nchs/Nhanes/
>
> Timeline
>
> ---
>
> ### July 
>
> Some typical ML and DL method to predict conditional survival analysis: **Maxima of Pr(years left to live|assuming die in 10 years)** . 
>
> For next, add a new category representing patients not dying in 10 years in neural network: **Maxima of Pr(years left to live)**
>
> ---
>
> ### August
>
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
> High ACC but not sure if it's plausible
>
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
>
> 
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
> | patient idx | week idx | mortality | Age  |
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
> 
>
> 
>
> **Logistic regression based on average over days**
>
> [1] 0.7072581
>
> over-fitting problem 
>
> 
>
> **convert 7 * 1440 matrix  to 1 * 10080 data**
>
> **note its 7 days complete data**
>
> **feed10080 into network**
>
> $loss
> [1] 2.210287
>
> $acc
> [1] 0.8628692
>
> ytest   2
>  0  26
>  1  39
>  2 409
>
> **PCA based DL: feed in rotation**
>
> $loss
> [1] 1.2907
>
> $acc
> [1] 0.8605898
>
> ytest   0   1   2
>  0   1   0  21
>  1   3   2  26
>  2   0   2 318
>
> **Spectrum DL**
>
> $loss
> [1] 2.290239
>
> $acc
> [1] 0.8579088
>
> ytest   2
>  0  22
>  1  31
>  2 320
