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
library(readtext)
library(vars)
library(ggcorrplot)

source("src/helpers.R")
source("src/data.R")

code = readLines("./FRED/fred-api-key.txt")
fredr_set_key(code)

# Data prep
data = data_pipeline()
row.names(data) = 1:dim(data)[1]
dim(data)
tail(data)
head(data,1)
data$anticipated = as.numeric(data$anticipated)
# Covid dummy variable
data$COVID = sapply(data$date, covid_dummy)
# Note: SP500_DIFF is in log differences, i.e log returns

data.ts = ts(dplyr::select(data, -c("date")), start = c(2002, data[1, "month"]), freq=12)
dim(data.ts)


# Look at cross correlation
print(ccf(data$FEDFUNDS_DIFF, data$log_inflation, lag.max= 20))


# Look at correlation between the variables
cor(data.ts[,c("UNEMPLOYMENT_DIFF", "CONSUMPTION_DIFF")])
cor(data.ts[,c("SP500_DIFF", "CONSUMPTION_DIFF")])
cor(data.ts[,c("SP500_DIFF", "OIL_WTI_DIFF")])
cor(data.ts[,c("log_inflation", "OIL_WTI_DIFF")])
cor(data.ts[,c("CONSUMPTION_DIFF", "NOMINAL_GDP_DIFF")])

vars = c("SP500_DIFF", "CONSUMPTION_DIFF", "FEDFUNDS_DIFF", 
         "log_inflation", "NONE_FARM_PAYROLLS_DIFF", 
         "VOLATILITY_INDEX_DIFF", "OIL_WTI_DIFF", "NOMINAL_GDP_DIFF", 
         "FEDFUNDS_FUTURES_DIFF" ,"inflation")

corr = cor(data.ts[, vars])
ggcorrplot(corr,hc.order = TRUE, outline.col = "white")

# notice that the correlation between consumption_diff and unemployment_diff is high
# so we will include only CONSUMPTION_DIFF in the model

#-----------------------------------------------------------------------
#------------------------------  Modeling ------------------------------
#-----------------------------------------------------------------------
# Naive models
model = dynlm(
  SP500_DIFF ~ FEDFUNDS_DIFF, 
  data=data.ts
)

# HAC standard errors for testing model coefficients
m = ceiling(0.75 * (dim(data.ts)[1])**(1/3))
nw = NeweyWest(model, lag = m, prewhite = F)
coeftest(model, vcov. = nw)

# Distributed lag model with no controls
model = dynlm(
  SP500_DIFF ~  L(d(FEDFUNDS_DIFF), 0:4) + L(FEDFUNDS_DIFF, 5), 
  data=data.ts
)

m = ceiling(0.75 * (dim(data.ts)[1])**(1/3))
nw = NeweyWest(model, lag = m, prewhite = F)
coeftest(model, vcov. = nw)

#------ Dynamic Linear Model---------
# Controls:
# - Surprise in consumption growth, inflation, and unemployment: 
#    Expected values will already be incorporated in the S&P500 returns
#    unless there is an unexpected change.
# - Volatility index:
#    Captures investors market sentiment.
# - Anticipated rate changes (anticipated):
#    A change in the FED rate may have no effect on returns if the change was anticipated

# The dynamic linear model model would not satisfy strict exogeneity meaning that a change in SP500 returns may indirectly 
# effect future federal funds rates through increased consumption.
# but SP500 may respond to future forecasts of inflation and a change in FED EFFECTIVE RATE.

# We use a VAR's residuals as a measure of surprise:

get_surprise_vars = function(data.ts) {
  TYPE = 'const'
  variables = c("log_inflation", 
                "CONSUMPTION_DIFF", 
                "UNEMPLOYMENT_DIFF", 
                "SAVINGS_DIFF",
                "TREASURY10Y_DIFF")
  var_data = data.ts[, variables]
  
  # Select lag that minimizes HQ (Hannan–Quinn information criterion)
  selection = VARselect(
    var_data, 
    lag.max=6, 
    type=TYPE
  )[["selection"]]
  
  var.model = VAR(var_data, p=selection[2], type=TYPE)

  # Join the residuals to the original data
  surprise = residuals(var.model)
  new_col_names = c()
  for (i in 1:length(variables)) {
    new_col_names[i] = paste0(variables[i], "_surprise")
  }
  colnames(surprise) = new_col_names
  combined = cbind(
    as.data.frame(data.ts), 
    rbind(
      matrix(rep(NA, 5*selection[2]), nrow=selection[2], ncol = 5), 
      surprise
    )
  )
  combined  = na.omit(combined )
  combined.ts = ts(combined , start = c(2002, combined[1, "month"]), freq=12)
  return(combined.ts)
}

combined= get_surprise_vars(data.ts)

# Replot correlations
vars = c(vars, new_col_names)
corr = cor(combined.ts[, vars])
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")

cor(combined.ts[, c("SAVINGS_DIFF_surprise", 
                    "UNEMPLOYMENT_DIFF_surprise", 
                    "CONSUMPTION_DIFF_surprise")])

# Check VIF
predictors = c("SAVINGS_DIFF_surprise", "UNEMPLOYMENT_DIFF_surprise", 
               "CONSUMPTION_DIFF_surprise", "log_inflation_surprise", "FEDFUNDS_DIFF", 
               "VOLATILITY_INDEX_DIFF")

get_VIF(predictors, combined.ts)

model = dynlm(
  SP500_DIFF ~  L(FEDFUNDS_DIFF, 0:10) +
    log_inflation_surprise + CONSUMPTION_DIFF_surprise + 
    VOLATILITY_INDEX_DIFF + COVID + anticipated + UNEMPLOYMENT_DIFF_surprise, 
  data=combined.ts
)
summary(model)

# HAC standard errors
m = ceiling(0.75 * (dim(combined.ts)[1])**(1/3))
nw = NeweyWest(model, lag = m, prewhite = F)
coeftest(model, vcov. = nw)

# Plot the coefficients
lags=10
coef(model)
coef_df = data.frame(coef(model)[2:(lags+2)])
coef_df=rownames_to_column(coef_df, var="coefficient")
ggplot(data = coef_df, aes(x=1:(lags+1), y=coef.model..2..lags...2.., group=1)) +
  geom_line(color="Dodgerblue")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  ylab("Coefficient") +
  xlab("Lag") + 
  ggtitle("Dynamic Effect of Fed Effective Rate on S&P500") +
  theme_minimal()


# Look at autocorrelation and partial autocorrelation
ggAcf(residuals(model))
ggPacf(residuals(model))

# Assess residuals
residual_plot("FEDFUNDS_DIFF", model, data=combined.ts,lags=lags)
ggplotly()
residual_plot("log_inflation_surprise", model, data=combined.ts, lags=lags)
residual_plot("CONSUMPTION_DIFF_surprise", model, data=combined.ts, lags=lags)
residual_plot("SAVINGS_DIFF_surprise", model, data=combined.ts, lags=lags)
residual_plot("VOLATILITY_INDEX_DIFF", model, data=combined.ts,lags=lags)

# F-test with HAC errors
ANOVA_DATA = data.ts[(lags+1) : dim(data.ts)[1]-1, ]
row.names(ANOVA_DATA) = 1:dim(ANOVA_DATA)[1]
ANOVA_DATA = ts(ANOVA_DATA)

res_model <- dynlm(SP500_DIFF ~ 1, data=ANOVA_DATA)
waldtest(model, 
         res_model, 
         vcov = NeweyWest(model, lag = m, prewhite = F))
# We Reject H0: all betas are 0.


##-------VECTOR AUTOREGRESSION---------
# Now we test if SP500 returns are endogenous. 
# Macroeconomic models can contain a part in the consumer problem
# Where consumers can decide to invest in stocks and bonds 
# and therefore in the next period, the returns on the stocks/bonds
# can effect consumer demand then consumer demand effects inflation and hence indirectly
# the Fed Funds Rate.

interest_rate = "FEDFUNDS_DIFF"
inflation_measure = "log_inflation"

# Intercept for VAR
TYPE = 'const'

var_data = data.ts[, c(inflation_measure, 
                       "CONSUMPTION_DIFF", 
                       "UNEMPLOYMENT_DIFF", 
                       interest_rate, 
                       "SAVINGS_DIFF",
                       "SP500_DIFF")
                    ]
print(ccf(var_data[, inflation_measure], var_data[, interest_rate], lag.max=10))

autoplot(var_data, facets = TRUE, colour=TRUE)
VARselect(
  var_data, 
  lag.max=6, 
  type=TYPE, 
  exogen = as.data.frame(data.ts[,"anticipated"])
)[["selection"]]

var.model = VAR(
  var_data, 
  p=2, 
  type=TYPE, 
  exogen = 
    as.data.frame(data.ts[,"anticipated"])
)
serial.test(var.model, lags.pt = 10, type="PT.asymptotic")
summary(var.model)
# x is our anticipated rate change dummy

# TREASURY1Y_DIFF Granger-cause's inflation but not the other way around
causality(var.model, "log_inflation")$Granger
causality(var.model, interest_rate)$Granger
grangertest(SP500_DIFF ~ FEDFUNDS_DIFF, order=2, data = var_data)

# look at impulse response functions 

# Interest rate on inflation
impulse = irf(
  var.model, 
  impulse = interest_rate,
  response = inflation_measure,
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

# We see that the IPR function has a similar shape as the coefficients make
# For the distributed lag model above.
