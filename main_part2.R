setwd("/Users/iseebiglietti/Desktop/M1 APE/Econ 2/HW/econ2_HW")

library(foreign)
library(uroot)
library(forecast)
library(dynlm)
library(vars)
library(urca)
library(fUnitRoots)
library(portes)
library(pdfetch)
library(rwebstat)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(highcharter)
library(gap)
library(mFilter)

###############################
#### Multivariate Analysis ####
###############################


### Selection of the lag order of the multivariate model

### Estimation of the multivariate model, and quality checks of the estimation

### Granger non-causality tests

### Forecasts: in-sample and out-of-sample, for all variables

### Orthogonalized impulse response functions, for all variables (use Cholesky method, and explain how you interpret the ordering 
### of the variables)