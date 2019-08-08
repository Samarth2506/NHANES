# rnhanesdata

> Source
> https://github.com/andrew-leroux/rnhanesdata
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
> **Category target** 
>
> manipulate between mortstat and permth_exm to get target category
>
> **Fourier transform**  
>
> get spectrum and feed into neural network
> Spectrum()$spec
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
> **Logistic regression**
