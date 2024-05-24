setwd("/Users/iseebiglietti/Desktop/M1 APE/Econ 2/HW/econ2_HW")

library(foreign)
library(uroot)
library(forecast)
library(dynlm)
library(vars)
library(urca)
library(fUnitRoots)
library(portes)
library(fUnitRoots)
library(pdfetch)
library(rwebstat)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(highcharter)
library(gap)
library(mFilter)

#############################
#### Univariate Analysis ####
#############################






### Data presentation: source and plots for each time series
read.csv2("RAW/GDP.csv")
read.csv2("RAW/LRUN64TTUSQ156S.csv")

# first plot (Raw plot). What do you see ? 
plot(qfr, type="s", col=c("red","green","black"), lwd=2, lty=1:3) + legend(x=1950, y=500, legend=c("Import","Export","GDP"),col=c("red","green","black"), lwd=2, lty=1:3)
#ou bien 
qfr_df = cbind(time(qfr), qfr) # you have to put time as a column
    colnames(qfr_df) = c("Time", colnames(qfr))
    qfr_df = as.data.frame(qfr_df) # you have to put your data in a dataframe
    baseplot = ggplot(qfr_df) + theme_bw() + 
      geom_line(aes(x=Time, y=gdp)) + 
      ggtitle("French real GDP") + ylab("")
    baseplot
baseplot + theme_wsj()
baseplot + theme_economist()
#ou bien
highchart(type = "stock") %>%
      hc_add_series(qfr[,"import"]/qfr[,"gdp"], name="Share of Imports") %>%
      hc_add_series(qfr[,"export"]/qfr[,"gdp"], name="Share of Exports") %>%
      hc_title(
        text = "France in globalisation",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)  ) %>%
      hc_rangeSelector(enabled = FALSE) %>%
      hc_rangeSelector(
        verticalAlign = "bottom",
        selected = 4) %>%
      hc_legend(enabled = TRUE)
#ou bien
    highchart(type = "stock") %>%
      hc_add_series((qfr[,"export"]-qfr[,"import"])/qfr[,"gdp"], name="Pct of GdP") %>%
      hc_title(
        text = "France balance of trade",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)  ) %>%
      hc_rangeSelector(enabled = FALSE) %>%
      hc_rangeSelector(
        verticalAlign = "bottom",
        selected = 4) %>%
      hc_legend(enabled = TRUE)
# linear trend
time = 1:nrow(lqfr)
reg_lin = lm(lgdp ~ time)
summary(reg_lin)
str(reg_lin)
# quadratic trend
time2 = time^2
reg_quad = lm(lgdp ~ time + time2)
summary(reg_quad)
#ou bien trend with breaks 

# transformation (dif log gdp and idk for unemployment rate)
lag1 = window(lag(lgdp, k=-1), start=1948, end=2019)






### Unit root and stationarity tests for each time series

# unit root function
#ADF-test
# With 1 lag ?
adfTest(lgdp, type="ct", lags=4) # we can't reject H0 (UR)
adfTest(dlgdp, type="c", lags=3) # we reject H0 --> stationarized TS
#PP-test
summary(ur.pp(lgdp, model="trend", type="Z-tau", use.lag=4)) # we don't reject H0 (UR)
summary(ur.pp(dlgdp, model="constant", type="Z-tau", use.lag=3)) # we reject the UR assumption at 1% --> stationarized TS.
#ERS-test (DF-GLS)
summary(ur.ers(lgdp, model="trend", type="DF-GLS", lag.max=4)) # we don't reject H0 (UR)
summary(ur.ers(dlgdp, model="constant", type="DF-GLS", lag.max=3)) # we reject H0 (UR) at 1% --> stationarized TS
#KPSS
summary(ur.kpss(lgdp, type="tau", lags="long")) # 15 lags
# we reject H0 (stationarity around a trend) at 1%
summary(ur.kpss(dlgdp, type="mu", lags="long")) # 15 lags
# we reject H0 (stationarity around a constant) at 1%
summary(ur.kpss(dlgdp, type="tau", lags="long")) # 15 lags
# we don't reject H0 (stationarity around a trend) even at 10%
summary(ur.kpss(d2lgdp, type="mu", lags="long")) 
# we don't reject H0 (stationarity around a constant), even at 10% --> stationarized TS


# stat test (filters)
lgdp_hp1600 = hpfilter(lgdp, type="lambda", freq=1600)
plot(lgdp_hp1600) # the cycle component clearly looks like a business cycle

lgdp_bk = bkfilter(lgdp, pl=6, pu=32)
str(lgdp_bk)
plot(cbind(lgdp_hp1600$cycle,lgdp_bk$cycle), type="s", col=c("black","blue"))
legend(x=1980, y=-0.03, legend=c("HP filter","BK filter"),
       col=c("black","blue"), lwd=2)

lgdp_cf = cffilter(lgdp, pl=6, pu=32, root=TRUE)
plot(cbind(lgdp_hp1600$cycle,lgdp_bk$cycle,lgdp_cf$cycle), type="s", col=c("black","blue","red"))
legend(x=1980, y=-0.03, legend=c("HP filter","BK filter","CF filter"),
       col=c("black","blue","red"), lwd=2)
# stat test for tranformed function






### Identification of the ARMA or ARIMA process for two time series
Acf(d_ln_wpi, lag.max=20)
Pacf(d_ln_wpi, lag.max=40)
# - For a stationary TS, the autocorrelation function decreases in an exponential manner. 
# - For a non-stationary TS, the autocorrelation function decreases linearly (and very slowly).

# ARMA diagnosis
#-%-------------%-
# 1. Significance of the last coefficients for the AR and MA parts, and significance of the constant.
#       --> Student tests (or Fisher test, as it should actually be a joint test)
# 2. Residuals WN --> no autocorrelation. 
#       --> Portmanteau tests: Box-Pierce, Ljung-Box, ...
#       Better with LB!
# 3. It would also be better to have normally distributed residuals.
#       --> Tests: Kolmogorov-Smirnov, Shapiro-Wilk, ...
# 4. Choice among the ARMA models, and between the best AR, MA and ARMA models: 
#       smallest information criteria. 
#       --> Many possible ICs: AIC, BIC, HQ, ...
#       Better with BIC or HQ.
# ...
#ARMA diagnosis
arma33 = Arima(gr_gdp_s, order=c(3,0,3))
plot(cbind(arma33$x,arma33$fitted), plot.type="s", col=c("black","red")) # poor prediction
1 - sum(arma33$residuals^2)/sum((arma33$x - mean(arma33$x))^2) # R2 0.16
#WN residuals?
tsdiag(arma33, gof.lag=40) # very clearly, yes
# p value is very high for every h! So yeah, eps are residuals
#Normally distributed residuals?
hist(arma33$residuals, breaks=20) 
qqnorm(arma33$residuals); qqline(arma33$residuals)
shapiro.test(arma33$residuals)
ks.test(arma33$residuals, rnorm)
jarque.bera.test(arma33$residuals)
# Bayesian Information Criterion / Schwarz IC = k*ln(n) - 2*ln(L)
ar3$bic # -1529.589
ma3$bic # -1524.86
arma33$bic # -1522.604
#minimize the criterion --> AR
# Akaike Information Criterion = 2*k - 2*ln(L) (to be modified for small sample)
ar3$aic # -1546
ma3$aic # -1542
arma33$aic # -1550






### Forecasts: in-sample and out-of-sample, for two time series
#For a ARMA(3,3)
arma_s = Arima(gr_gdp_s, order=c(3,0,3))
# in sample forecast
plot(cbind(gr_gdp_s, arma_s$fitted), plot.type="s", col=c("black","red")) 
legend(x=1980, y=0.06, legend=c("QoQ rate","In-sample forecast ARMA(3,3)"),
       col=c("black","red"), lwd=2)
# out-of-sample forecast
forecast_arma_oos = forecast(object=gr_gdp_s, model=arma_s, h=35)
forecast_arma_oos
plot(forecast_arma_oos)
lines(gr_gdp, col="black") # good forecast, at the good level
#Comparison
# comparison of all three in-sample forecasts
plot(cbind(gr_gdp_s, ar3_s$fitted,ma3_s$fitted,arma_s$fitted), plot.type="s", 
     col=c("black","red","blue","green")) 
legend(x=1990, y=0.06, legend=c("QoQ rate","IS Forecast AR(3)","IS Forecast MA(3)","IS Forecast ARMA(3,3)"),
       col=c("black","red","blue","green"), lwd=2)
# comparison of all three out-of-sample forecasts
f1=ts(c(gr_gdp_s,forecast_hl_s$mean), start=1949.25, frequency=4)
f2=ts(c(gr_gdp_s,forecast_ma_oos$mean), start=1949.25, frequency=4)
f3=ts(c(gr_gdp_s,forecast_arma_oos$mean), start=1949.25, frequency=4)
plot(cbind(f1, f2, f3), 
     plot.type="s", col=c("red","blue","green"))
lines(gr_gdp, col="black")
legend(x=1990, y=0.06, legend=c("QoQ rate","OOS Forecast AR(3)","OOS Forecast MA(3)","OOS Forecast ARMA(3,3)"),
       col=c("black","red","blue","green"), lwd=2)







###############################
#### Multivariate Analysis ####
###############################


### Selection of the lag order of the multivariate model

### Estimation of the multivariate model, and quality checks of the estimation

### Granger non-causality tests

### Forecasts: in-sample and out-of-sample, for all variables

### Orthogonalized impulse response functions, for all variables (use Cholesky method, and explain how you interpret the ordering 
### of the variables)

