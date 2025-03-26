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
library(vars)
library(ggcorrplot)
library(AER)
library(stargazer)
library(ggdag)
library(MTS)

source("src/helpers.R")
source("src/data.R")


code = readLines("./FRED/fred-api-key.txt")
fredr_set_key(code)

data = data_pipeline()
row.names(data) = 1:dim(data)[1]

dim(data)
tail(data)
head(data,1)

data$anticipated = as.numeric(data$anticipated)


# Covid dummy variable
data$COVID = sapply(data$date, covid_dummy)
data$inflation_dummy = sapply(data$date, inflation_dummy)
# Note: SP500_DIFF is in log differences, i.e log returns


data = data[data["date"]>="2001-10-01", ]

data.ts = ts(dplyr::select(data, -c("date")), start = c(2001, data[1, "month"]), freq=12)
dim(data.ts)

plot_timeseries(diff(data.ts[, "taylor_residuals"]), name='')
adf.test(diff(data.ts[, "taylor_residuals"]))

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


result = adf.test(data.ts[,"CONSUMPTION_DIFF"])
result$p.value
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
bptest(model)
checkresiduals(model)
# bp rejects H0 so we most likely have non-constant variance
# The residuals are most likely stationary

# HAC standard errors for testing model coefficients
m = ceiling(0.75 * (dim(data.ts)[1])**(1/3))
nw = NeweyWest(model, lag = m, prewhite = F)
coeftest(model, vcov. = nw)


# Distributed lag model with no controls
# Assuming a change in fed funds from 1 year ago effects todays S&P500
model = dynlm(
  SP500_DIFF ~  L(FEDFUNDS_DIFF,0:11), 
  data=data.ts
)

stargazer(model, type="text")

AIC(model)

# Check for constant variance and no serial correlation
bptest(model)
checkresiduals(model)
summary(model)

# Plot the coefficients
plot_coef(model, lags=11)

# Cumulative dynamic multipliers
cum_model = dynlm(
  SP500_DIFF ~  L(d(FEDFUNDS_DIFF),0:10) + L(FEDFUNDS_DIFF,11), 
  data=data.ts
)

bptest(cum_model)
checkresiduals(cum_model)
summary(cum_model)

# Plot the coefficients
plot_coef(cum_model, lags=11)

#------ Dynamic Linear Model with Controls---------
# Controls:
# - Surprise in consumption growth, inflation, and unemployment: 
#    Expected values will already be incorporated in the S&P500 returns
#    unless there is an unexpected change.
# - Anticipated rate changes (anticipated):
#    A change in the FED rate may have no effect on returns if the change was anticipated

# The dynamic linear model model would not satisfy strict exogeneity meaning that a change in SP500 returns may indirectly 
# effect future federal funds rates through increased consumption.
# but SP500 may respond to future forecasts of inflation and a change in FED EFFECTIVE RATE.


# We use a VAR's residuals as a measure of surprise:
combined.ts = get_surprise_vars(data)

# Replot correlations
vars = c(vars, new_col_names)
corr = cor(combined.ts[, vars])
ggcorrplot(corr, hc.order = TRUE, outline.col = "white")

cor(combined.ts[, c("VOLATILITY_INDEX_DIFF", "FEDFUNDS_DIFF")])

cor(combined.ts[, c("SAVINGS_DIFF_surprise", 
                    "UNEMPLOYMENT_DIFF_surprise", 
                    "CONSUMPTION_DIFF_surprise", "SP500_DIFF")])


# Check VIF
predictors = c("SAVINGS_DIFF_surprise", "UNEMPLOYMENT_DIFF_surprise", 
               "CONSUMPTION_DIFF_surprise", "log_inflation_DIFF_surprise", "FEDFUNDS_DIFF", 
               "VOLATILITY_INDEX_DIFF")

get_VIF(predictors, combined.ts)

# Create dag to visualize effects of the variables
dag <- dagify(
  SP500 ~ FedRateChange + log_inflation_DIFF_surprise + 
    UNEMPLOYMENT_DIFF_surprise + Anticipated + TREASURY10Y_DIFF+
    SAVINGS_DIFF_surprise + "FedRateChangeLagged" +
    AnticipatedLagged,
  FedRateChange ~ log_inflation_DIFF_surprise + UNEMPLOYMENT_DIFF_surprise + TREASURY10Y_DIFF + "FedRateChange[t-1]",
  Anticipated ~ FedRateChange,
  AnticipatedLagged ~ FedRateChangeLagged,
  exposure = "FedRateChange",
  outcome = "SP500"
)

set.seed(8)
ggdag(dag, layout = "nicely") +theme_minimal() +
  geom_dag_edges(edge_color = "darkgray", edge_width = 1) +
  geom_dag_point(color = "lightblue", size = 20) +
  geom_dag_text(color = "black") +
  xlab("") +
  ylab("") +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) + 
  expand_limits(x = c(-3, 1.2)) +
  ggtitle("Variable Effects on Log S&P 500 Returns")
ggplotly()

# Assessing concern of simultaneity between SP500 returns and the volatility index
grangertest(SP500_DIFF ~ VOLATILITY_INDEX_DIFF, order=1, data = data)
grangertest(VOLATILITY_INDEX_DIFF ~ SP500_DIFF, data = data)

# Assuming a change in fed funds from 1 year ago effects todays S&P500
model = dynlm(
  SP500_DIFF ~  L(FEDFUNDS_DIFF, 0:11) + COVID + inflation_dummy + 
    log_inflation_DIFF_surprise + UNEMPLOYMENT_DIFF_surprise +
    CONSUMPTION_DIFF_surprise + TREASURY10Y_DIFF + SAVINGS_DIFF_surprise + 
    L(anticipated,0:3) + log_inflation, 
  data=combined.ts
)

AIC(model)

# Check for residual autocorrelation and non-constant variance 

checkresiduals(model, test="LB")
bptest(model)

stat = bptest(model)
stat$parameter
stargazer(data.frame(Statistic = stat$statistic, df = stat$parameter,
                     P_Value = stat$p.value),
          type = "text", summary = FALSE)

summary(model)
# Check for multicolinearity 
vif(model)

# This is here just in case if there was serial correlation or 
# non constant variance
m = ceiling(0.75 * (dim(combined.ts)[1])**(1/3))
nw = NeweyWest(model, lag = m, prewhite = F)
coeftest(model, vcov. = nw)

# Plot the coefficients
plot_coef(model, lags=11)


cum_model = dynlm(
  SP500_DIFF ~  L(d(FEDFUNDS_DIFF), 0:10) + L(FEDFUNDS_DIFF, 11) + COVID + inflation_dummy + 
    log_inflation_DIFF_surprise + UNEMPLOYMENT_DIFF_surprise +
    CONSUMPTION_DIFF_surprise + TREASURY10Y_DIFF + SAVINGS_DIFF_surprise + L(anticipated,0:3) + log_inflation_DIFF, 
  data=combined.ts
)
summary(cum_model)


# Plot the coefficients
plot_coef(cum_model, lags=11)


# Assess residuals
residual_plot("FEDFUNDS_DIFF", model, data=combined.ts,lags=11)
ggplotly()
residual_plot("log_inflation_DIFF_surprise", model, data=combined.ts, lags=11)
residual_plot("CONSUMPTION_DIFF_surprise", model, data=combined.ts, lags=11)
residual_plot("SAVINGS_DIFF_surprise", model, data=combined.ts, lags=11)


# F-test with HAC errors (This is here just incase)
ANOVA_DATA = combined.ts[(lags) : dim(combined.ts)[1] - 1, ]
row.names(ANOVA_DATA) = 1:dim(ANOVA_DATA)[1]
ANOVA_DATA = ts(ANOVA_DATA)
res_model <- dynlm(SP500_DIFF ~ 1, data=ANOVA_DATA)
waldtest(model, 
         res_model, 
         vcov = NeweyWest(model, lag = m, prewhite = F))
# We Reject H0: there are betas that are not 0.


# F test for cumulative model
res_model <- dynlm(SP500_DIFF ~ L(d(FEDFUNDS_DIFF), 0:10) + L(FEDFUNDS_DIFF, 11), data=combined.ts)
waldtest(cum_model, 
         res_model, 
         vcov = NeweyWest(model, lag = m, prewhite = F))


# Test if controls add value
res_model <- dynlm(SP500_DIFF ~  L(FEDFUNDS_DIFF, 0:11), data=combined.ts)

waldtest(model, 
         res_model)
# The one or more of the controls coefficients are most likely non zero


# RESET test
resettest(model, power = 2, type = "f")
resettest(cum_model, power = 2, type = "f")



checkresiduals(inst_model)

##-------VECTOR AUTOREGRESSION---------
# Now we test if SP500 returns are endogenous. 
# Macroeconomic models can contain a part in the consumer problem
# Where consumers can decide to invest in stocks and bonds 
# and therefore in the next period, the returns on the stocks/bonds
# can effect consumer demand then consumer demand effects inflation and hence indirectly
# the Fed Funds Rate.

interest_rate = "FEDFUNDS_DIFF"
inflation_measure = "log_inflation_DIFF"

# Intercept for VAR
TYPE = 'const'

var_data = data.ts[, c(
  "UNEMPLOYMENT_DIFF",
  "CONSUMPTION_DIFF",
  "SAVINGS_DIFF",
  "SP500_DIFF",
  inflation_measure,
  interest_rate)
                    ]

# CHeck multivariate autocorrelation
mq(var_data, lag=3)

print(ccf(var_data[, inflation_measure], var_data[, interest_rate], lag.max=10))


autoplot(var_data, facets = TRUE, colour=TRUE)
VARselect(
  var_data, 
  lag.max=6, 
  type=TYPE, 
  exogen = as.data.frame(data.ts[,"anticipated"])
)[["selection"]]

var.model = vars::VAR(
  var_data, 
  p=1, 
  type=TYPE, 
  exogen =
    as.data.frame(data.ts[, c("anticipated_lag1", "anticipated")])
)
vars::serial.test(var.model, lags.pt = 15, type="PT.asymptotic")
summary(var.model)

adf.test(residuals(var.model)[, 6])

residuals_var <- residuals(var.model)

# Plot residuals for each variable
par(mfrow = c(ncol(residuals_var), 1))  # Set layout for multiple plots
for (i in 1:ncol(residuals_var)) {
  plot(residuals_var[, i], type = "l", main = colnames(residuals_var)[i], ylab = "Residuals")
  abline(h = 0, col = "red", lty = 2)  # Add horizontal line at zero
}
par(mfrow = c(1, 1))  # Reset layout

# x is our anticipated rate change dummy
roots(var.model)

# TREASURY1Y_DIFF Granger-cause's inflation but not the other way around
causality(var.model, inflation_measure)$Granger
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

# We see that the IPR function has a negative shock that disperses over time 
# similarly to the distributed lag model above except the DLM has a positive
# shock after .

matrix <- matrix(c(
  1, 0, 0, 0, 0, 0,
  NA, 1, 0, 0, 0, 0,
  0, NA, 1, NA, 0, 0,
  0, 0, NA, 1, 0, 0,
  0, 0, 0, NA, 1, 0,
  0, NA, NA, NA, 0, 1
), nrow = 6, byrow = TRUE)


svar_est_a <- SVAR(var.model, Amat = a, max.iter = 1000, estmethod = "direct")
impulse = irf(
  svar_est_a, 
  impulse = interest_rate,
  response = "SP500_DIFF",
  n.ahead = 25,
)
plot(impulse)


