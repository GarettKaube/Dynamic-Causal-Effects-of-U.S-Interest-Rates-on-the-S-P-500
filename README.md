# Dynamic-Causal-Effects-of-U.S-Interest-Rates-on-the-S-P-500
Estimating Dynamic Causal Effects of U.S Interest Rates on the S&amp;P 500 using R

## Goal: 
See how the unanticipated change of the FED rate affect the returns on S&P 500 over time on a monthly basis.
## Process and Results:
Starts with obtaining data from FRED and Yahoo finance such as S&P 500 prices, FED FUNDS effective rate, Fed Funds futures, personal consumption,
and more macro data. Almost all data is differenced or converted to percent change. Data such as the volatility index and S&P500 prices
are seasonally adjusted to remove seasonal effects. S&P 500 prices are converted to log returns and FED FUNDS effective rate is converted 
to percent change in FED FUNDS effective rate and these are the variables used in the models.
The Fed Funds futures is used to estimate anticipated and uninticipated rate changes as described in Bernanke and Kuttner, 2005.
A vector autoregression is used to derive the amount of "surprise" there is in realized log inflation, and consumption growth where
the residuals of the model are the surprise. These residuals are used as controls in the linear model.

We start out with a dynamic linear model with lags in the FED FUNDS effective rate and the other control variables:

$$\Delta \log(SP500_t) = \beta_0 + \sum_{l=0}^{10}\beta_{1+l} \Delta FEDFUNDS_{t-l} + \beta_{11} \epsilon^ {\pi} _ {t} + \beta_{12} \epsilon ^{\Delta C}_{t} + \beta _{13} \epsilon ^{\Delta U} _{t}  + \beta _{14} \Delta VIX _{t} + \beta  _{15}COVID _t + \beta _{16} ANTICIPATED _t + u _t$$

Where $\epsilon^ {\pi} _ {t}$ is surprise in inflation, $\epsilon ^{\Delta C}_{t}$ surprise in consumption growth, and $\epsilon ^{\Delta U} _{t}$, surprise change in unemployment. 

The model resulted in an impulse response on S&P500 returns that is negativaly effected and recovers quickly:
![image](https://github.com/user-attachments/assets/7e5c5ffc-9625-4981-bafc-fced5d1da650)


Later we consider endogeneity and treat it by estimating a vector autoregression
with Percent change in FED FUNDS effective rate, the S&P500 returns, and other macro variables.
The VAR's impulse response function for a shock of Percent change in FED FUNDS effective rate on S&P500 returns
yielded a similmar response as the dynamic linear model's dynamic multipliers:
![image](https://github.com/user-attachments/assets/53bd0919-b5db-4d71-b60a-7b3485887d7b)


