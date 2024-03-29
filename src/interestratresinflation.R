library(tidyverse)
library(fredr)
library(fpp2)
library(dplyr)
library(mFilter)
library(pls)
library(forecast)
library(lmtest)
library(sandwich)
library(hash)
library(car)
library(tseries)
library(dynlm)
library(plotly)
library(MASS)
library(robustbase) 
library(olsrr)
library(readtext)
library(readtext)

source("src/helpers.R")
source("src/data.R")

# to do:
# get firm inventories


code = readLines("./FRED/fred-api-key.txt")
fredr_set_key(code)


data = data_pipeline()
data$COVID = data[, 'date'] %>% sapply(FUN = covid_dummy)
dim(data)
# Note: SP500_DIFF is in log differences

data.ts = ts(dplyr::select(data, -c("date")), start = c(2014, 10), freq=12)
# Since data starts in August 2015 for Bitcoin and interest rates
data.ts = window(data.ts, start = c(2015,08))
dim(data.ts)

# Plot inflation rate vs change in real interest rates
autoplot(data.ts[, c("log_inflation", "REAL_INTEREST_3M_DIFF")], facets = TRUE)

# Plot inflation rate vs change in  interest rates
autoplot(data.ts[, c("log_inflation", "TREASURY3M_DIFF")], facets = TRUE)

# We see positive correlation with inflation and yields as an increase in inflation will cause investors to 
# require higher yield to compensate for inflation.

# Plot time series of some of the variables
autoplot(data.ts[, "average_log_return"])
autoplot(diff(log(data.ts[, "Stock_Index"])))
autoplot(diff(log(data.ts[, "OIL_WTI"])))
autoplot(data.ts[, "REAL_INTEREST_1Y_DIFF"])
autoplot(data.ts[, "CONSUMPTION_DIFF"])
autoplot(data.ts[, "FEDFUNDS_DIFF"])


# Look at linear correlation
cor_matrix = data.ts %>% cor()

# Look at cross correlations
print(ccf(data$TREASURY1Y_DIFF, data$log_inflation, lag.max= 20))
print(ccf(data$TREASURY3M_DIFF, data$log_inflation), lag.max= 20)
print(ccf(data$TREASURY3M_DIFF, data$SP500_DIFF, lag.max= 20))


ggplot(data=data, aes(x=TREASURY1Y_DIFF, y=log_inflation)) +
  geom_point(aes(color=year))
ggplotly()

ggplot(data=data, aes(x=TREASURY1Y_DIFF, y=SP500_DIFF)) +
  geom_point(aes(color=year))  +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
ggplotly()


ggplot(data=data, aes(x=REAL_INTEREST_1Y_DIFF, y=SP500_DIFF)) +
  geom_point(aes(color=year))  +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
ggplotly()


# Scatter plot of the 3 month real interest rate vs log return of S&P500
ggplot(data=data, aes(x=REAL_INTEREST_3M_DIFF, y=SP500_DIFF)) +
  geom_point(aes(color=year))  +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
ggplotly()


# Notice that when we lag the interest rate, the correlation switches signsL
ggplot(data=data, aes(x=lag(REAL_INTEREST_3M_DIFF), y=SP500_DIFF)) +
  geom_point(aes(color=year))  +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
ggplotly()


# remove extreme outlier/high leverage point which was 2020-03-01 i.e. the covid shock
data_modified = data %>% dplyr::filter(TREASURY1Y_DIFF > -1.0)
data_modified.ts = ts(dplyr::select(data_modified, -c("date")), start = 2015, freq=12)
data_modified.ts %>% cor()

# 1Y
ggplot(data=data_modified, aes(x=TREASURY1Y_DIFF, y=SP500_DIFF)) +
  geom_point(aes(color=year)) +
  geom_smooth(method = "lm", se = FALSE)
ggplotly()

# 3M
ggplot(data=data_modified, aes(x=TREASURY3M_DIFF, y=SP500_DIFF)) +
  geom_point(aes(color=year)) +
  geom_smooth(method = "lm", se = FALSE)
ggplotly()


ggplot(data=data, aes(x=TREASURY3M_DIFF, y=Stock_Index_DIFF)) +
  geom_point(aes(color=year)) +
  geom_smooth(method = "lm", se = FALSE)
ggplotly()

ggplot(data=data, aes(x=TREASURY3M_DIFF, y=average_log_return)) +
  geom_point(aes(color=year)) +
  geom_smooth(method = "lm", se = FALSE)
ggplotly()

# Histograms
ggplot(data=data, aes(x=TREASURY3M_DIFF)) +
  geom_histogram()
ggplot(data=data, aes(x=Stock_Index_DIFF)) +
  geom_histogram()
ggplotly()

# Scatter of inflation vs difference in TREASURY1Y
ggplot(data=data, aes(x=log_inflation, y=TREASURY1Y_DIFF)) +
  geom_point(aes(color=year)) +
  geom_smooth(method = "lm", se = FALSE)
ggplotly()


# Linear models

# notice that the correlation between consumption and unemployment is high:
cor(data[,c("UNEMPLOYMENT_DIFF", "CONSUMPTION_DIFF")])
# so we will include only CONSUMPTION_DIFF in the model

# We control with using consumption growth, and inflation as a measure of economic performance
# This model would not satisfy strict exogeneity as a change in SP500 returns should not effect the FED RATE but SP500 may 
# respond to future forecasts of inflation and hence a change in FED RATE.
n_lags = 7
model = dynlm(SP500_DIFF ~  d(L(FEDFUNDS_DIFF, 0:7)) + L(FEDFUNDS_DIFF, 7) + CONSUMPTION_DIFF + log_inflation + SAVINGS_DIFF + INFLATION_EXPECTATIONS_5Y_DIFF, 
              data=data.ts)

summary(model)
ggAcf(residuals(model))
ggPacf(residuals(model))

# HAC standard errors
m = ceiling(0.75 * (dim(data.ts)[1])**(1/3))
nw = NeweyWest(model, lag = m, prewhite = F)
coeftest(model, vcov. = nw)

# We add log inflation, consumption_diff, savings as a measure of economic performance as inflation will be higher when consumption is high. 
# i.e high consumption growth causing output to be larger than potential output may cause inflation.
# Also note that in a market clearing environment Y(t) = C(t) in theory.
# REAL_INTEREST_3M_DIFF also must become statistically insignificant because log_inflation will already be large when interest rates decide 
# to increase in the same month so log_inflation absorbs the effect.
# Inflation may not always move the same direction as the S&P500 as if we have a negative supply shock, output will decrease,
# inflation will rise, and weak economic conditions may cause the S&P500 to fall due to weak consumer demand and investment.

residual_plot("FEDFUNDS_DIFF", model, data=data.ts,lags=6)
residual_plot("log_inflation", model, data=data.ts,lags=6)
residual_plot("CONSUMPTION_DIFF", model, data=data.ts,lags=6)


# F-test with HAC errors
res_model <- dynlm(SP500_DIFF ~ 1, data=data[7:dim(data)[1], ])
waldtest(model, 
         res_model, 
         vcov = NeweyWest(model, lag = m, prewhite = F))


res_model <- update(model, formula = . ~ . - CONSUMPTION_DIFF - log_inflation)
waldtest(model, 
         res_model, 
         vcov = NeweyWest(model, lag = m, prewhite = F))

model = dynlm(SP500_DIFF ~ REAL_INTEREST_3M_DIFF + d(L(REAL_INTEREST_3M_DIFF, 1:6)) + COVID + CONSUMPTION_DIFF, data=data.ts)
# C
summary(model)
# HAC consistent standard errors
m = ceiling(0.75 * (dim(data.ts)[1])**(1/3))
nw = NeweyWest(model, lag = m, prewhite = F)
coeftest(model, vcov. = nw)

#-----------------------------------------------------
#------------ VECTOR AUTOREGRESSIONs-----------------
#-----------------------------------------------------
library(vars)

interest_rate = "FEDFUNDS_DIFF"

TYPE = 'const'

var_data = data.ts[, c("log_inflation", "UNEMPLOYMENT_DIFF", interest_rate, "SP500_DIFF")]
print(ccf(var_data[, "log_inflation"], var_data[, interest_rate], lag.max=10))
autoplot(var_data, facets = TRUE, colour=TRUE)

VARselect(var_data, lag.max = 6,type=TYPE)[["selection"]]

var.model = VAR(var_data, p=2, type=TYPE)
serial.test(var.model, lags.pt = 10, type="PT.asymptotic")
summary(var.model)

# TREASURY1Y_DIFF Granger-cause's log_inflation but not the other way around
causality(var.model, "log_inflation")$Granger
causality(var.model, interest_rate)$Granger

# look at impulse response function 

impulse = irf(
  var.model, 
  impulse = interest_rate,
  response = "log_inflation",
  n.ahead = 10,
  ortho = FALSE,
  runs = 1000
  )
plot(impulse)

# Estimate structural var
a = diag(1, 4)
a[lower.tri(a)] = NA
a
svar = SVAR(var.model, Amat = a, max.iter = 20000)
svar
summary(svar)


# svar 2
var_data = data.ts[, c("log_inflation", interest_rate, "SP500_DIFF")]
print(ccf(var_data[, "log_inflation"], var_data[, interest_rate], lag.max=10))
autoplot(var_data, facets = TRUE, colour=TRUE)

VARselect(var_data, lag.max = 6,type=TYPE)[["selection"]]

var.model = VAR(var_data, p=1, type=TYPE)
serial.test(var.model, lags.pt = 10, type="PT.asymptotic")
summary(var.model)

a = diag(1, 3)
a[lower.tri(a)] = NA
a
svar = SVAR(var.model, Amat = a, max.iter = 10000)
svar
summary(svar)

