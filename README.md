# Dynamic-Causal-Effects-of-U.S-Interest-Rates-on-the-S-P-500
Estimating Dynamic Causal Effects of U.S Interest Rates on the S&amp;P 500 using R

## Goal: 
See how the unanticipated change of the FED rate affect the returns on S&P 500 over time on a monthly basis.
## Process and Results:
Starts with obtaining data from FRED and Yahoo finance such as S&P 500 prices, FED FUNDS effective rate, Fed Funds futures, personal consumption,
and more macro data.
The Fed Funds futures is used to estimate anticipated and uninticipated rate changes as described in Bernanke and Kuttner, 2005.

We start out with a dynamic linear model with lags in the FED FUNDS effective rate and the other control variables which led
to an impulse response on S&P500 returns that is negativaly effected and recovers quickly.

Later we consider endogeneity and treat it by estimating a vector autoregression
with Percent change in FED FUNDS effective rate, the S&P500 returns, and other macro variables.
The VAR's impulse response function for a shock of Percent change in FED FUNDS effective rate on S&P500 returns
yielded a silimar response as the dynamic linear model's dynamic multipliers.
