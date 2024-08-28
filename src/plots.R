library(tidyverse)
library(fredr)
source("src/data.R")

data = data_pipeline()
row.names(data) = 1:dim(data)[1]
data = data %>% filter(date >= as.Date("2002-01-01"))
data$anticipated = as.numeric(data$anticipated)
# Note: SP500_DIFF is in log differences, i.e log returns

data.ts = ts(dplyr::select(data, -c("date")), start = c(2002, data[1, "month"]), freq=12)

line_color = "#ADD8E6"

plot_timeseries = function(series, name) {
  plot = autoplot(series, color=line_color) + 
    theme_minimal() +
    ylab(name)
  print(plot)
}

# Plot time series of some of the variables
plot_timeseries(data.ts[, "FEDFUNDS_DIFF_logs"], "OIL")
plot_timeseries(data.ts[, "VOLATILITY_INDEX_DIFF"], "Volatility change")
plot_timeseries(data.ts[, "VOLATILITY_INDEX"], "Volatility")
plot_timeseries(diff(log(data.ts[, "OIL_WTI"])), "OIL")
plot_timeseries(data.ts[, "TREASURY10Y_DIFF"], "TREASURY10Y Differenced")
plot_timeseries(data.ts[, "CONSUMPTION_DIFF"], "CONSUMPTION Growth")
plot_timeseries(data.ts[, "FEDFUNDS"], "FED EFFECTIVE RATE")
ggplotly()
plot_timeseries(data.ts[, "FEDFUNDS_DIFF"], "% CHANGE IN FED EFFECTIVE RATE")
plot_timeseries(data.ts[, "SP500_DIFF"], "S&P500 RETURNS")


scatter_plot = function(x, y, title) {
  plot = ggplot(data=data, aes(x=!!sym(x), y=!!sym(y))) +
    geom_point(aes(color=year)) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    ggtitle(title)
  print(plot)
}


# Change in FEDFUNDS RATE VS sp500
scatter_plot(x="FEDFUNDS_DIFF", y="SP500_DIFF", title="CHANGE IN FED FUNDS EFFECTIVE RATE VS S&P500 RETURNS")
ggplotly()

# FEDFUNDS RATE VS sp500
scatter_plot(x="FEDFUNDS", y="SP500_DIFF", title="FED FUNDS EFFECTIVE RATE VS S&P500 RETURNS")

# Consumption growth vs S&P500 RETURNS
scatter_plot(x="CONSUMPTION_DIFF", y="SP500_DIFF", title="CONSUMPTION GROWTH VS S&P500 RETURNS")

# log inflation vs S&P500 RETURNS
scatter_plot(x="log_inflation", y="SP500_DIFF", title="log_inflation VS S&P500 RETURNS")

# VOLATILITY_INDEX VS S&P500 RETURNS
scatter_plot(x="VOLATILITY_INDEX_DIFF", y="SP500_DIFF", title="VOLATILITY_INDEX VS S&P500 RETURNS")

# anticipated policy change bar plot
# 0 is unanticipated while 1 in anticipated 
n_anticipated = data %>% 
  group_by(anticipated) %>% 
  summarise(count = n())
n_anticipated = as.data.frame(n_anticipated)
n_anticipated[n_anticipated["anticipated"] == 0, "anticipated"] = "Unanticipated"
n_anticipated[n_anticipated["anticipated"] == 1, "anticipated"] = "anticipated"

ggplot(data=n_anticipated, aes(x=anticipated, y=count)) +
  geom_bar(stat="identity", fill="#ADD8E6") +
  geom_text(aes(label=count), vjust=1.6, color="white", size=3.5) +
  theme_minimal() +
  ggtitle("Number of Anticipated and Surprise Policy Changes")

# Average return for anticipated an un anticipated changes
n_anticipated_avg_return = data %>% 
  group_by(anticipated) %>% 
  summarise(avg_ret = round(mean(SP500_DIFF)*100,2))

n_anticipated_avg_return = as.data.frame(n_anticipated_avg_return)
n_anticipated_avg_return[n_anticipated_avg_return["anticipated"] == 0, "anticipated"] = "Unanticipated"
n_anticipated_avg_return[n_anticipated_avg_return["anticipated"] == 1, "anticipated"] = "anticipated"

ggplot(data=n_anticipated_avg_return, aes(x=anticipated, y=avg_ret)) +
  geom_bar(stat="identity", fill="#ADD8E6") +
  geom_text(aes(label=avg_ret), vjust=1.6, color="white", size=3.5) +
  theme_minimal() +
  ggtitle("Number of Anticipated and Surprise Policy Changes")

# Histograms
ggplot(data=data, aes(x=FEDFUNDS_DIFF)) +
  geom_histogram(fill=line_color) +
  theme_minimal()

ggplot(data=data, aes(x=SP500_DIFF)) +
  geom_histogram(fill=line_color) +
  theme_minimal()


