library(tidyverse)
library(fredr)
source("src/data.R")

data = data_pipeline()

# Change in 1 year treasury vs log inflation
ggplot(data=data, aes(x=TREASURY1Y_DIFF, y=log_inflation)) +
  geom_point(aes(color=year))
ggplotly()

# Change in 1 year treasury vs change in log S&P500
ggplot(data=data, aes(x=TREASURY1Y_DIFF, y=SP500_DIFF)) +
  geom_point(aes(color=year))  +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
ggplotly()


# Change in 1 year real interest rate vs change in log S&P500
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


# 3M
ggplot(data=data, aes(x=TREASURY3M_DIFF, y=SP500_DIFF)) +
  geom_point(aes(color=year)) +
  geom_smooth(method = "lm", se = FALSE)
ggplotly()


ggplot(data=data, aes(x=TREASURY3M_DIFF, y=average_log_return)) +
  geom_point(aes(color=year)) +
  geom_smooth(method = "lm", se = FALSE)
ggplotly()


# FEDFUNDS RATE, we see a log transform may help
ggplot(data=data, aes(x=exp(FEDFUNDS_DIFF), y=SP500_DIFF)) +
  geom_point(aes(color=year)) +
  geom_smooth(method = "lm", se = FALSE)
ggplotly()
