# Fit_YinSigmoid
R script for fitting phenotypic data to the [Yin](https://doi.org/10.1093/aob/mcg029) function based on thermal time, using nls

## Files
- [Yin_function.R](Yin_function.R): the script for fitting
- [Temp_2017.txt](Temp_2017.txt): example temperature input file
- [Plantheight.txt](Plantheight.txt): example phenotype input file

## Basic features
- read temperature and phenotype data
- wrangle data for fitting
- fit the yin model to the data using [nls](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/nls#:~:text=The%20nls%20function%20uses%20a,(eps)%20%3E%200%20) function.
- generate overview plots of fits in pdf format
