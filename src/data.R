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
library(yaml)

source("src/helpers.R")

code = readLines("./FRED/fred-api-key.txt")
code
fredr_set_key(code)



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
  

  data = get_data(h)
  
  
  # Create inflation variable
  #data = data %>% mutate(inflation = (CPI/lag(CPI, n=12L) - 1))
  data = difference_data(data, "CPI", lag = 12, log_diff = TRUE) %>% 
     dplyr::rename("log_inflation" = "CPI_DIFF")
  
  data = difference_data(data, "CPI", lag = 1, log_diff = TRUE) %>% 
    dplyr::rename("log_inflation_MOM" = "CPI_DIFF")
  
  data = difference_data(data, "producer_price_index", lag = 1, log_diff = TRUE) %>% 
    dplyr::rename("producer_price_index_change" = "producer_price_index_DIFF")
  
  # create annual change in log inflation
  #data = data %>% mutate(log_inflation_annual = log(CPI) - lag(log(CPI), n=12L))
  
  # Ex-post
  data$REAL_INTEREST_3M = data$TREASURY3M - data$log_inflation
  data$REAL_INTEREST_1Y = data$TREASURY1Y - data$log_inflation
  #data$REAL_INTEREST_2Y = data$TREASURY2Y - data$inflation
  data$REAL_INTEREST_10Y = data$TREASURY10Y - data$log_inflation
  

  
  print(data)
  for (data_item in data_config_fred %>% names()) {
    config = get(data_item, data_config_fred)
    if (get("difference", config) == TRUE) {
      data = difference_data(data, get("name", config), log_diff = get("log", config))
    }
  }

  
  # Log transform TREASURY1Y
  data$log_TREASURY1Y = data$TREASURY1Y %>% log()
  
  # Calculate average return of SP500
  data = calculate_average_return("SP500", data)
  data = lag_data(
    data, 
    c("TREASURY1Y_DIFF", "TREASURY3M_DIFF", 
      "REAL_INTEREST_3M_DIFF", "FEDFUNDS_DIFF", 
      "log_inflation", "CONSUMPTION_DIFF", "SAVINGS_DIFF",
      "UNEMPLOYMENT_DIFF", "NONE_FARM_PAYROLLS", "producer_price_index_change"), 
    lags=c(6,6,6,7,2, 1, 1, 2, 1, 1)
  )

 
  
  # Remove NA
  data = na.omit(data)
  return(data)
}


