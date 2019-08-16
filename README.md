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
> * High ACC but not sure if it's plausible
>
>   ytest    2
>       0   88
>       1   82
>       2 1072
>
> * $loss
>   [1] 2.206181
>
>   $acc
>   [1] 0.863124
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
> | patient idx | mortality | Age  |
> | :---------: | :-------: | :--: |
> |      1      |     0     |  65  |
> |      2      |     0     |  65  |
> |      3      |     0     |  65  |
> |      4      |     0     |  65  |
> |      1      |     0     |  54  |
> |      2      |     1     |  54  |
>
> 
>
> 
>
> 
>
> **Logistic regression**
>
> * [1] 0.7072581
> * over-fitting problem 
