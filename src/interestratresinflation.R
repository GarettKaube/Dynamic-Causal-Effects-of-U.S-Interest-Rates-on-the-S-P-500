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

# Get data and create covid dummy variable
data = data_pipeline()
data$COVID = data[, 'date'] %>% sapply(FUN = covid_dummy)
dim(data)

# Note: SP500_DIFF is in log differences

data.ts = ts(dplyr::select(data, -c("date")), start = c(2015, 10), freq=12)
data.ts = window(data.ts, start = c(2015,08))
dim(data.ts)

# Plot inflation rate vs change in real interest rates
autoplot(data.ts[, c("inflation", "REAL_INTEREST_3M_DIFF")], facets = TRUE)

autoplot(data.ts[, c("NONE_FARM_PAYROLLS_DIFF")])

# Plot inflation rate vs change in  interest rates
autoplot(data.ts[, c("inflation", "TREASURY3M_DIFF")], facets = TRUE)

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
print(ccf(data$TREASURY1Y_DIFF, data$inflation, lag.max= 20))
print(ccf(data$TREASURY3M_DIFF, data$inflation), lag.max= 20)
print(ccf(data$TREASURY3M_DIFF, data$SP500_DIFF, lag.max= 20))



# Histograms
ggplot(data=data, aes(x=TREASURY3M_DIFF)) +
  geom_histogram()
ggplot(data=data, aes(x=Stock_Index_DIFF)) +
  geom_histogram()
ggplotly()



get_VIF = function(predictors) {
  # Initialize VIF data frame
  VIF = data.frame()
  for (i in 1:(length(predictors))) {
    target = predictors[i]
    
    # Get predictors
    predictors_modified = predictors[-c(i)]
    
    # Create regression formulas
    predictor_str = predictors_modified[1]
    predictors_modified = predictors_modified[-c(1)]
    for (pred in predictors_modified) {
      predictor_str = predictor_str %>% paste("+") %>% paste(pred)
    }
    
    formula = paste(target, "~") %>% paste(predictor_str) 
    print(paste("Model: ",formula))
    
    # Regress, get R-Squared and calculate Variance factors
    r2 = summary(lm({{formula}}, data = data))$r.squared
    VIF[1, paste(formula, "|")] =  1/(1-r2)
    
  }
  return(VIF)
}

predictors = c("CONSUMPTION_DIFF1", "log_inflation1", "SAVINGS_DIFF1", "FEDFUNDS_DIFF",
               "INFLATION_EXPECTATIONS_5Y_DIFF", "UNEMPLOYMENT_DIFF1", "NONE_FARM_PAYROLLS1", "producer_price_index_change1")
get_VIF(predictors)

# Linear models

# notice that the correlation between consumption and unemployment is high:
cor(data[,c("UNEMPLOYMENT_DIFF", "CONSUMPTION_DIFF")])
cor(data[,c("INFLATION_EXPECTATIONS_5Y_DIFF", "CONSUMPTION_DIFF")])
cor(data[,c("SP500_DIFF", "CONSUMPTION_DIFF")])
cor(data[,c("SP500_DIFF", "CONSUMPTION_DIFF", "FEDFUNDS_DIFF1", "log_inflation", "log_inflation1", "NONE_FARM_PAYROLLS1", "producer_price_index_change1")])
# so we will include only CONSUMPTION_DIFF in the model

# We control with using consumption growth, and inflation as a measure of economic performance
# This model would not satisfy strict exogeneity as a change in SP500 returns should not effect the FED RATE but SP500 may 
# respond to future forecasts of inflation and hence a change in FED RATE.

# We lag all the control variables since the stock market reacts to economic data from the previous period  

model = dynlm(
  SP500_DIFF ~  d(L(FEDFUNDS_DIFF, 0:4)) + L(FEDFUNDS_DIFF, 5) + 
    L(log_inflation_MOM) + L(CONSUMPTION_DIFF) + COVID + 
    L(producer_price_index_change) + L(NONE_FARM_PAYROLLS), 
  data=data.ts
)


# HAC standard errors
m = ceiling(0.75 * (dim(data.ts)[1])**(1/3))
nw = NeweyWest(model, lag = m, prewhite = F)
coeftest(model, vcov. = nw)

ggAcf(residuals(model))
ggPacf(residuals(model))

# We add log inflation, consumption_diff, savings as a measure of economic performance as inflation will be higher when consumption is high. 
# i.e high consumption growth causing output to be larger than potential output may cause inflation.
# Also note that in a market clearing environment Y(t) = C(t) in theory.
# REAL_INTEREST_3M_DIFF also must become statistically insignificant because inflation will already be large when interest rates decide 
# to increase in the same month so inflation absorbs the effect.
# Inflation may not always move the same direction as the S&P500 as if we have a negative supply shock, output will decrease,
# inflation will rise, and weak economic conditions may cause the S&P500 to fall due to weak consumer demand and investment.

residual_plot("FEDFUNDS_DIFF", model, data=data.ts,lags=8)
residual_plot("inflation", model, data=data.ts,lags=6)
residual_plot("CONSUMPTION_DIFF", model, data=data.ts,lags=6)


# F-test with HAC errors
res_model <- dynlm(SP500_DIFF ~ 1, data=data[7:dim(data)[1], ])
waldtest(model, 
         res_model, 
         vcov = NeweyWest(model, lag = m, prewhite = F))


res_model <- update(model, formula = . ~ . - CONSUMPTION_DIFF - inflation)
waldtest(model, 
         res_model, 
         vcov = NeweyWest(model, lag = m, prewhite = F))

# Test another model
model = dynlm(SP500_DIFF ~ REAL_INTEREST_3M_DIFF + d(L(REAL_INTEREST_3M_DIFF, 1:6)) + COVID + CONSUMPTION_DIFF, data=data.ts)
summary(model)
# HAC consistent standard errors
m = ceiling(0.75 * (dim(data.ts)[1])**(1/3))
nw = NeweyWest(model, lag = m, prewhite = F)
coeftest(model, vcov. = nw)

#VECTOR AUTOREGRESSIONS
library(vars)

interest_rate = "FEDFUNDS_DIFF"

TYPE = 'none'

var_data = data.ts[, c("log_inflation_MOM", "UNEMPLOYMENT_DIFF", interest_rate)]
var_data = window(var_data, start = c(2020, 9))
print(ccf(var_data[, "log_inflation"], var_data[, interest_rate], lag.max=10))
autoplot(var_data, facets = TRUE, colour=TRUE)
VARselect(var_data, lag.max = 6,type=TYPE)[["selection"]]

var.model = VAR(var_data, p=1, type=TYPE)
serial.test(var.model, lags.pt = 10, type="PT.asymptotic")
summary(var.model)

# TREASURY1Y_DIFF Granger-cause's inflation but not the other way around
causality(var.model, "log_inflation_MOM")$Granger
causality(var.model, interest_rate)$Granger

# look at impulse response functions 

# Interest rate on inflation
impulse = irf(
  var.model, 
  impulse = interest_rate,
  response = "log_inflation_MOM",
  n.ahead = 25,
  ortho = FALSE,
  runs = 1000
  )
plot(impulse)

# interest rate on log difference S&P 500
impulse = irf(
  var.model, 
  impulse = interest_rate,
  response = "SP500_DIFF",
  n.ahead = 25,
  ortho = FALSE,
  runs = 1000
)
plot(impulse)


# Estimate structural var
a = diag(1, 3)
a[lower.tri(a)] = NA
a
svar = SVAR(var.model, Amat = a, max.iter = 10000,)
svar

impulse = irf(
  svar, 
  impulse = interest_rate,
  response = "log_inflation_MOM",
  n.ahead = 10,
  ortho = TRUE,
  runs = 1000
)




