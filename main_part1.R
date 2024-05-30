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

#############################
#############################
#### UNIVARIATE ANALYSIS ####
#############################
#############################





##########################################################
#Data presentation: source and plots for each time series#
##########################################################

GDP <- read.csv2("RAW/GDP.csv", sep = ",")
urate <- read.csv2("RAW/LRUN64TTUSQ156S.csv", sep = ",")

# change the date format
GDP$DATE <- as.Date(GDP$DATE, format = "%Y-%m-%d")
urate$DATE <- as.Date(urate$DATE, format = "%Y-%m-%d")

# separate month and year
GDP$YEAR <- format(GDP$DATE, "%Y")
GDP$MONTH <- format(GDP$DATE, "%m")
urate$YEAR <- format(urate$DATE, "%Y")
urate$MONTH <- format(urate$DATE, "%m")

# combine year and month in the correct way for the following plots
GDP$DATE <- as.numeric(paste0(GDP$YEAR, ".", GDP$MONTH))
urate$DATE <- as.numeric(paste0(urate$YEAR, ".", urate$MONTH))

# transform the measure of interest in the correct format
GDP$GDP <- as.numeric(GDP$GDP)
urate$LRUN64TTUSQ156S <- as.numeric(urate$LRUN64TTUSQ156S)
urate <- urate %>%
  rename(rate = LRUN64TTUSQ156S)



###### First plot (Raw plot). What do you see ? 

GDP_1 <- ggplot(GDP, aes(x = DATE, y = GDP)) +
  geom_line()

ggsave("OUTPUT/GDP_1.png", plot = GDP_1, width = 8, height = 6)
#ou bien
GDP_1 <- highchart(type = "stock") %>%
      hc_add_series(GDP[,"GDP"], name="Value of GDP") %>%
      hc_title(
        text = "US GDP",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)  ) %>%
      hc_rangeSelector(enabled = FALSE) %>%
      hc_rangeSelector(
        verticalAlign = "bottom",
        selected = 4) %>%
      hc_legend(enabled = TRUE)

export_hc(GDP_1, filename = "OUTPUT/GDP_1.png")

urate_1 <- ggplot(urate, aes(x = DATE, y = rate)) +
  geom_line()

ggsave("OUTPUT/urate_1.png", plot = urate_1, width = 8, height = 6)
#ou bien
urate_1 <- highchart(type = "stock") %>%
      hc_add_series(urate[,"rate"], name="Unemployment rate") %>%
      hc_title(
        text = "US unemployment rate",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)  ) %>%
      hc_rangeSelector(enabled = FALSE) %>%
      hc_rangeSelector(
        verticalAlign = "bottom",
        selected = 4) %>%
      hc_legend(enabled = TRUE)

export_hc(GDP_1, filename = "OUTPUT/urate_1.png")

#####################################################################################    ICI PROBLEMES 
###### Transformations (where d stands for 1 differenciation and l for log transformation)
lGDP <- GDP %>% mutate(GDP = log(GDP))
dGDP <- GDP %>% mutate(GDP = GDP - lag(GDP))
dlGDP <- GDP %>% mutate(GDP = log(GDP) - lag(log(GDP)))
ddGDP <- GDP %>% mutate(GDP = GDP - lag(GDP) - lag(GDP - lag(GDP)))
ddlGDP <- GDP %>% mutate(GDP = log(GDP) - lag(log(GDP)) - lag(log(GDP) - lag(log(GDP))))

drate <- urate %>% mutate(rate = rate - lag(rate))
ddrate <- urate %>% mutate(rate = rate - lag(rate) - lag(rate - lag(rate)))

lGDP_plot <- ggplot(lGDP, aes(x = DATE, y = lGDP)) +
  geom_line()
ggsave("OUTPUT/lGDP.png", plot = lGDP_plot, width = 8, height = 6)

dGDP_plot <- ggplot(dGDP, aes(x = DATE, y = dGDP)) +
  geom_line()
ggsave("OUTPUT/dGDP.png", plot = dGDP_plot, width = 8, height = 6)

dlGDP_plot <- ggplot(dlGDP, aes(x = DATE, y = dlGDP)) +
  geom_line()
ggsave("OUTPUT/dlGDP.png", plot = dlGDP_plot, width = 8, height = 6)

ddGDP_plot <- ggplot(ddGDP, aes(x = DATE, y = ddGDP)) +
  geom_line()
ggsave("OUTPUT/ddGDP.png", plot = ddGDP_plot, width = 8, height = 6)

ddlGDP_plot <- ggplot(ddlGDP, aes(x = DATE, y = ddlGDP)) +
  geom_line()
ggsave("OUTPUT/ddlGDP.png", plot = ddlGDP_plot, width = 8, height = 6)


drate_plot <- ggplot(drate, aes(x = DATE, y = drate)) +
  geom_line()
ggsave("OUTPUT/drate.png", plot = drate_plot, width = 8, height = 6)

ddrate_plot <- ggplot(ddrate, aes(x = DATE, y = ddrate)) +
  geom_line()
ggsave("OUTPUT/ddrate.png", plot = ddrate_plot, width = 8, height = 6)



# we take rate, lGDP and dlGDP ! :) #




#######################################################
#Unit root and stationarity tests for each time series#
#######################################################

###### Unit root function
#IC test to get the number of lags
VARselect(urate) # ? lags
VARselect(lGDP) # ? lags
VARselect(dlGDP) # ? lags

#ADF-test
adfTest(urate, type="ct", lags=4)
adfTest(lGDP, type="ct", lags=4) # we can't reject H0 (UR)
adfTest(dlGDP, type="c", lags=3) # we reject H0 --> stationarized TS
#PP-test
summary(ur.pp(urate, model="trend", type="Z-tau", use.lag=4))
summary(ur.pp(lGDP, model="trend", type="Z-tau", use.lag=4)) # we don't reject H0 (UR)
summary(ur.pp(dlGDP, model="constant", type="Z-tau", use.lag=3)) # we reject the UR assumption at 1% --> stationarized TS.
#ERS-test (DF-GLS)
summary(ur.ers(urate, model="trend", type="DF-GLS", lag.max=4))
summary(ur.ers(lGDP, model="trend", type="DF-GLS", lag.max=4)) # we don't reject H0 (UR)
summary(ur.ers(dlGDP, model="constant", type="DF-GLS", lag.max=3)) # we reject H0 (UR) at 1% --> stationarized TS
#KPSS
summary(ur.kpss(urate, type="mu", lags="long")) # 15 lags
# we reject H0 (stationarity around a constant) at 1%
summary(ur.kpss(lGDP, type="tau", lags="long")) # 15 lags
# we reject H0 (stationarity around a trend) at 1%
summary(ur.kpss(dlGDP, type="mu", lags="long")) # 15 lags
# we reject H0 (stationarity around a constant) at 1%
summary(ur.kpss(dlGDP, type="tau", lags="long")) # 15 lags
# we don't reject H0 (stationarity around a trend) even at 10%

#dlGDP is stationary with lGDP isn't ! Take dlGDP and rate from now on !!





########################################################################################### NOT DONE FROM NOW
#################################################################
#Identification of the ARMA or ARIMA process for two time series#
#################################################################

#ACF of an MA(q)=0 after q lags
#PACF of an AR(p)=0 after p lags

Acf(dlGDP, lag.max=20) #q = ?
Pacf(dlGDP, lag.max=40) #p = ?
Acf(urate, lag.max=20) #q = ?
Pacf(urate, lag.max=40) #p = ?
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

#MA, AR, ARMA diagnosis
dlGDP_ar3 = Arima(dlGDP, order=c(3,0,0))
summary(dlGDP_ar3)
dlGDP_ma3 = Arima(dlGDP, order=c(0,0,3))
summary(dlGDP_ma3)
dlGDP_arma33 = Arima(dlGDP, order=c(3,0,3))
summary(dlGDP_arma33)
plot(cbind(dlGDP_arma33$x,dlGDP_arma33$fitted), plot.type="s", col=c("black","red")) # poor prediction
1 - sum(dlGDP_arma33$residuals^2)/sum((dlGDP_arma33$x - mean(dlGDP_arma33$x))^2) # R2 ?

rate_ar3 = Arima(urate, order=c(3,0,0))
summary(rate_ar3)
rate_ma3 = Arima(urate, order=c(0,0,3))
summary(rate_ma3)
rate_arma33 = Arima(urate, order=c(3,0,3))
summary(rate_arma33)
plot(cbind(rate_arma33$x,rate_arma33$fitted), plot.type="s", col=c("black","red")) # poor prediction
1 - sum(rate_arma33$residuals^2)/sum((rate_arma33$x - mean(rate_arma33$x))^2) # R2 ?

#WN residuals? (autocorrelation tests)
Box.test(dlGDP_ar3$residuals,lag = 30, type = "Ljung-Box") # No AC
tsdiag(dlGDP_ar3, gof.lag = 40) # True for all lag selection
Box.test(dlGDP_ma3$residuals,lag = 30, type = "Ljung-Box") # No AC
tsdiag(dlGDP_ma3, gof.lag = 40) # True for all lag selection
Box.test(dlGDP_arma33$residuals,lag = 30, type = "Ljung-Box") # No AC
tsdiag(dlGDP_arma33, gof.lag=40) # very clearly, yes
# p value is very high for every h! So yeah, epsilons are residuals

Box.test(rate_ar3$residuals,lag = 30, type = "Ljung-Box") # No AC
tsdiag(rate_ar3, gof.lag = 40) # True for all lag selection
Box.test(rate_ma3$residuals,lag = 30, type = "Ljung-Box") # No AC
tsdiag(rate_ma3, gof.lag = 40) # True for all lag selection
Box.test(rate_arma33$residuals,lag = 30, type = "Ljung-Box") # No AC
tsdiag(rate_arma33, gof.lag=40) # very clearly, yes
# p value is very high for every h! So yeah, epsilons are residuals

#Normally distributed residuals?
hist(dlGDP_ar3$residuals, breaks=20) 
qqnorm(dlGDP_ar3$residuals); qqline(dlGDP_ar3$residuals)
shapiro.test(dlGDP_ar3$residuals)
ks.test(dlGDP_ar3$residuals, rnorm)
jarque.bera.test(dlGDP_ar3$residuals)

hist(dlGDP_ma3$residuals, breaks=20) 
qqnorm(dlGDP_ma3$residuals); qqline(dlGDP_ma3$residuals)
shapiro.test(dlGDP_ma3$residuals)
ks.test(dlGDP_ma3$residuals, rnorm)
jarque.bera.test(dlGDP_ma3$residuals)

hist(dlGDP_arma33$residuals, breaks=20) 
qqnorm(dlGDP_arma33$residuals); qqline(dlGDP_arma33$residuals)
shapiro.test(dlGDP_arma33$residuals)
ks.test(dlGDP_arma33$residuals, rnorm)
jarque.bera.test(dlGDP_arma33$residuals)


hist(rate_ar3$residuals, breaks=20) 
qqnorm(rate_ar3$residuals); qqline(rate_ar3$residuals)
shapiro.test(rate_ar3$residuals)
ks.test(rate_ar3$residuals, rnorm)
jarque.bera.test(rate_ar3$residuals)

hist(rate_ma3$residuals, breaks=20) 
qqnorm(rate_ma3$residuals); qqline(rate_ma3$residuals)
shapiro.test(rate_ma3$residuals)
ks.test(rate_ma3$residuals, rnorm)
jarque.bera.test(rate_ma3$residuals)

hist(rate_arma3$residuals, breaks=20) 
qqnorm(rate_arma3$residuals); qqline(rate_arma3$residuals)
shapiro.test(rate_arma3$residuals)
ks.test(rate_arma3$residuals, rnorm)
jarque.bera.test(rate_arma3$residuals)

# Bayesian Information Criterion / Schwarz IC = k*ln(n) - 2*ln(L)
dlGDP_ar3$bic # -1529.589
dlGDP_ma3$bic # -1524.86
dlGDP_arma33$bic # -1522.604
#minimize the criterion --> AR
rate_ar3$bic # -1529.589
rate_ma3$bic # -1524.86
rate_arma33$bic # -1522.604
#minimize the criterion --> AR

# Akaike Information Criterion = 2*k - 2*ln(L) (to be modified for small sample)
dlGDP_ar3$aic # -1546
dlGDP_ma3$aic # -1542
dlGDP_arma33$aic # -1550
#minimize the criterion --> ARMA
rate_ar3$aic # -1546
rate_ma3$aic # -1542
rate_arma33$aic # -1550
#minimize the criterion --> ARMA

# This implies that we should choose the AR(1), as it has the minimum BIC
# and the ARMA(1,1) is not SS at the MA component. Finally, all three models
# have no serial correlation and non normal errors.

# We could choose the MA(3) or the AR(2), as they both have a similar BIC and the
# same properties. However, I will use the former because it has a slightly lower
# BIC, a slightly better chance of having normal errors, a lower AIC and is the 
# result of the process of taking out unsignificant coefficients from the 
# ARMA(2,3).


#############################################################
#Forecasts: in-sample and out-of-sample, for two time series#
#############################################################

#For a ARMA(3,3) (celui que tu choisis en fait)
# in sample forecast
plot(cbind(dlGDP, dlGDP_arma33$fitted), plot.type="s", col=c("black","red")) 
plot(cbind(urate, rate_arma33$fitted), plot.type="s", col=c("black","red")) 

# out-of-sample forecast
forecast_arma_dlGDP = forecast(object=dlGDP, model=dlGDP_arma33, h=35)
forecast_arma_dlGDP
plot(forecast_arma_dlGDP)
lines(dlGDP, col="black") # good forecast, at the good level

forecast_arma_rate = forecast(object=urate, model=rate_arma33, h=35)
forecast_arma_rate
plot(forecast_arma_rate)
lines(urate, col="black") # good forecast, at the good level

#Comparison
# comparison of all three in-sample forecasts
plot(cbind(dlGDP, dlGDP_ar3$fitted,dlGDP_ma3$fitted,dlGDP_arma33$fitted), plot.type="s", 
     col=c("black","red","blue","green")) 
legend(x=1971.01, y=0.06, legend=c("Differentiated Growth","Forecast AR(3)","Forecast MA(3)","Forecast ARMA(3,3)"),
       col=c("black","red","blue","green"), lwd=2)

plot(cbind(urate, rate_ar3$fitted,rate_ma3$fitted,rate_arma33$fitted), plot.type="s", 
     col=c("black","red","blue","green")) 
legend(x=1971.01, y=0.06, legend=c("Unemployment Rate","Forecast AR(3)","Forecast MA(3)","Forecast ARMA(3,3)"),
       col=c("black","red","blue","green"), lwd=2)

# comparison of all three out-of-sample forecasts
forecast_ar_dlGDP = forecast(object=dlGDP, model=dlGDP_ar3, h=35)
forecast_ma_dlGDP = forecast(object=dlGDP, model=dlGDP_ma3, h=35)
forecast_ar_rate = forecast(object=urate, model=rate_ar3, h=35)
forecast_ma_rate = forecast(object=urate, model=rate_ma3, h=35)

f1=ts(c(dlGDP,forecast_ar_dlGDP$mean), start=1971.01, frequency=4)
f2=ts(c(dlGDP,forecast_ma_dlGDP$mean), start=1971.01, frequency=4)
f3=ts(c(dlGDP,forecast_arma_dlGDP$mean), start=1971.01, frequency=4)
plot(cbind(f1, f2, f3), 
     plot.type="s", col=c("red","blue","green"))
legend(x=1971.01, y=0.06, legend=c("Differentiated Growth","Forecast AR(3)","Forecast MA(3)","Forecast ARMA(3,3)"),
       col=c("black","red","blue","green"), lwd=2)

f4=ts(c(urate,forecast_ar_rate$mean), start=1971.01, frequency=4)
f5=ts(c(urate,forecast_ma_rate$mean), start=1971.01, frequency=4)
f6=ts(c(urate,forecast_arma_rate$mean), start=1971.01, frequency=4)
plot(cbind(f4, f5, f6), 
     plot.type="s", col=c("red","blue","green"))
legend(x=1971.01, y=0.06, legend=c("Unemployment Rate","Forecast AR(3)","Forecast MA(3)","Forecast ARMA(3,3)"),
       col=c("black","red","blue","green"), lwd=2)

