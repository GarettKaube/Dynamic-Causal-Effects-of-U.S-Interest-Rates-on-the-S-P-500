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

source("src/helpers.R")

fredr_set_key("9a048a276f1a939a1e64c77f214e5684")


get_data = function(hash_funct, start_=as.Date("2010-01-01")) {
  series_ids = keys(hash_funct)
  # Download time series 
  series_list = list()
  for (i in seq_along(series_ids)) {
    series = fredr(series_id=series_ids[i], observation_start=start_, frequency='m') %>% 
      drop_cols(name=hash_funct[[series_ids[i]]])
    
    series_list = append(series_list, list(series))
  }
  
  # merge the time series
  merged = merge(series_list[1], series_list[2], by="date")
  for (i in 3:length(series_list)) {
    merged = merge(merged, series_list[i], by="date")
  }
  
  return(merged)
}


data_pipeline = function() {
  # Federal Funds Effective Rate, Bank of Canada Overnight Rate, Coinbase Bitcoin
  # Hash function format is h[["FRED_ID"]] = "User choice name"
  h = hash()
  
  
  # Structure of the data config yaml file is as follows: 
  # FRED_data:
  #    FRED_CODE:
  #       name: str
  #       difference: bool
  #       log: bool
  #    FRED_CODE:
  #       ...
  # ...
  
  data_config = yaml.load_file("./data_config/data_config.yaml")
  data_config_fred = data_config$FRED_data
  
  for (data in names(data_config_fred)) {
    config = get(data, data_config_fred)
    if (get("download", config) == TRUE) {
      h[[data]] = get("name", config)
    }
  }
  
  # h[["CPILFESL"]] = "CPI"
  # h[["SP500"]] = "SP500"
  # h[["GS1"]] = "TREASURY1Y"
  # h[["GS10"]] = "TREASURY10Y"
  # h[["DTB3"]] = "TREASURY3M" # https://fred.stlouisfed.org/series/GS3M
  # h[["GS2"]] = "TREASURY2Y"
  # h[["WILL5000IND"]] = "Stock_Index" 
  # h[["MCOILWTICO"]] = "OIL_WTI"
  # h[["PCE"]] = "CONSUMPTION"
  # h[["UNRATE"]] = "UNEMPLOYMENT"
  # h[["PSAVERT"]] = "SAVINGS" # https://fred.stlouisfed.org/series/PSAVERT
  # h[["FEDFUNDS"]] = "FEDFUNDS" # https://fred.stlouisfed.org/series/FEDFUNDS
  # h[["T5YIEM"]] = "INFLATION_EXPECTATIONS_5Y" #https://fred.stlouisfed.org/series/T5YIEM
  data = get_data(h)
  
  
  # Create inflation variable
  data = difference_data(data, "CPI", log=TRUE) %>% 
    dplyr::rename("log_inflation" = "CPI_DIFF")
  
  # Ex-post
  data$REAL_INTEREST_3M = data$TREASURY3M - data$log_inflation
  data$REAL_INTEREST_1Y = data$TREASURY1Y - data$log_inflation
  #data$REAL_INTEREST_2Y = data$TREASURY2Y - data$log_inflation
  data$REAL_INTEREST_10Y = data$TREASURY10Y - data$log_inflation
  
  # Log difference data
  # difference_list = c("TREASURY1Y", "TREASURY3M", "TREASURY2Y", "TREASURY10Y", "SP500", "Stock_Index", "OIL_WTI", 
  #                     "REAL_INTEREST_3M", 
  #                     "REAL_INTEREST_1Y",
  #                     "REAL_INTEREST_2Y",
  #                     "REAL_INTEREST_10Y",
  #                     "CONSUMPTION",
  #                     "UNEMPLOYMENT",
  #                     "SAVINGS",
  #                     "FEDFUNDS",
  #                     "INFLATION_EXPECTATIONS_5Y"
  #                     )
  # 
  # log_difference = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, F, T, F)
  
  print(data)
  for (data_item in data_config_fred %>% names()) {
    config = get(data_item, data_config_fred)
    if (get("difference", config) == TRUE) {
      data = difference_data(data, get("name", config), log_diff = get("log", config))
    }
  }
  # for (ind in seq(length(difference_list))) {
  #   data = difference_data(data, difference_list[ind], log_diff = log_difference[ind])
  # }
  
  # Log transform TREASURY1Y
  data$log_TREASURY1Y = data$TREASURY1Y %>% log()
  
  # Calculate average return of SP500
  data = calculate_average_return("SP500", data)
  data = lag_data(data, c("TREASURY1Y_DIFF", "TREASURY3M_DIFF", "REAL_INTEREST_3M_DIFF", "FEDFUNDS_DIFF"), lags=c(6,6,6))

 
  
  # Remove NA
  data = na.omit(data)
  return(data)
}

library(yaml)
yo = yaml.load_file("./data_config/data_config.yaml")
yo
for (item in yo$FRED_data) {
  print(typeof(item))
    
  
}
names(yo$FRED_data)

for (i in yo$FRED_data[1]){
  print(i)
}
get("PSAVERT",yo$FRED_data)
