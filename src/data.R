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
library(quantmod)
library(lubridate)

source("src/helpers.R")

code = readLines("./FRED/fred-api-key.txt")
code
fredr_set_key(code)

get_fred_data = function(hash_funct, start_=as.Date("1960-01-01")) {
  # Gets the data in the hash_funct from FRED and merges them on date column
  series_ids = keys(hash_funct)
  # Download time series and store them in a list
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

get_yahoo_data = function(symbol, var_name) {
  # Downloads S&P500 data from yahoo finance and 
  # converts it to monthly data using the last value of 
  # each month.
  yh_data <- getSymbols(
    symbol, 
    src='yahoo', 
    auto.assign=FALSE, 
    from = '1960-01-01'
  )
  yh_data = apply.monthly(yh_data, last)
  
  # Convert to month start
  index(yh_data) = floor_date(index(yh_data), "month")
  yh_data = data.frame(yh_data)
  
  # Fix characters in the target variable
  var = paste0(gsub("\\^", "", symbol), ".Close")
  var = gsub("=", ".", var)
  
  # Reset index and rename the variable
  yh_data = rownames_to_column(yh_data, "date") %>%
    dplyr::rename_with(~var_name, var)
  yh_data$date = as.Date(yh_data$date)

  return(yh_data[, c("date", var_name)])
}


get_nom_gdp = function(start_=as.Date("1960-01-01")) {
  series = fredr(series_id="GDP", observation_start=start_, frequency='q') %>% 
    drop_cols(name="NOMINAL_GDP")
  return(series)
}


data_pipeline = function() {
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
  
  # Hash function format is h[["FRED_ID"]] = "User choice name"
  for (data in names(data_config_fred)) {
    config = get(data, data_config_fred)
    if (get("download", config) == TRUE) {
      h[[data]] = get("name", config)
    }
  }
  data = get_fred_data(h)
  sp500 = get_yahoo_data("^GSPC", "SP500")
  futures = get_yahoo_data('ZQ=F', "FEDFUNDS_FUTURES")
  gdp = get_nom_gdp()
  
  data = merge(data, sp500, by="date")
  data = merge(data, futures, by="date")
  data = merge(data, gdp, by="date", all.x=TRUE)
  data["NOMINAL_GDP"] = data["NOMINAL_GDP"] %>% fill(NOMINAL_GDP)
  
  # Create YOY log inflation variable
  data = difference_data(data, "CPI", lag = 12, log_diff = TRUE) %>% 
     dplyr::rename("log_inflation" = "CPI_DIFF")
  data$inflation = (data$CPI - lag(data$CPI, 12)) / lag(data$CPI, 12)
  # MOM inflation
  data = difference_data(data, "CPI", lag = 1, log_diff = TRUE) %>% 
    dplyr::rename("log_inflation_MOM" = "CPI_DIFF")
  

  for (data_item in data_config_fred %>% names()) {
    config = get(data_item, data_config_fred)
    if (get("difference", config) == TRUE) {
      data = difference_data(data, get("name", config), log_diff = get("log", config))
    }
  }
  
  # Log transform TREASURY1Y
  data$log_TREASURY1Y = data$TREASURY1Y %>% log()
  
  data$FEDFUNDS_DIFF = (data$FEDFUNDS - lag(data$FEDFUNDS))/lag(data$FEDFUNDS)
  
  # Using method from Bernanke and Kuttner, 2005 to derive anticipated fed rate changes
  data$FEDFUNDS_FUTURES_DIFF = (data$FEDFUNDS_FUTURES - lag(data$FEDFUNDS_FUTURES))/lag(data$FEDFUNDS_FUTURES)
  data$unanticipated_rate = (100 - data$FEDFUNDS_FUTURES) - (100 - lag(data$FEDFUNDS_FUTURES))
  data$anticipated_rate = c(NA, diff(data$FEDFUNDS)) - data$unanticipated_rate
  data$anticipated = data$anticipated_rate <= 0.0025 & data$anticipated_rate >= -0.0025
  
  # Create date variables
  data[, 'year'] = data[, 'date'] %>% sapply(extract_year)
  data[, 'month'] = data[, 'date'] %>% sapply(extract_month)
  
  # Remove NA
  data$FEDFUNDS_DIFF_DIFF = c(NA, base::diff(data$FEDFUNDS_DIFF))
  data$log_inflation_DIFF = c(NA, base::diff(data$log_inflation))
  data = na.omit(data)
  return(data)
}


