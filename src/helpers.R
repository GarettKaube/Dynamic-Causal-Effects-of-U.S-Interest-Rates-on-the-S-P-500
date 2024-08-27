extract_year = function(x) {
  return(format(as.Date(x), format = "%Y"))
}

extract_month = function(x) {
  return(format(as.Date(x), format = "%m"))
}


residual_plot = function(var, model, lags = 1, data){
  print(data[lags, var])
  df = data.frame(var = data[(lags+1):dim(data)[1], var] ,res=residuals(model))
  ggplot(data = df, aes(x=var, y=res)) +
    geom_point(color='skyblue') +
    theme_minimal() +
    labs(y= "Residuals", x = var) +
    geom_smooth(method='lm', se=F)
  
  
}

lag_data = function(data, columns, lags = rep(1,length(columns))) {
  for (i in 1:length(columns)) {
    n_lags = lags[i]
    column = columns[i]
    
    # Create lagged columns
    for (j in 1:n_lags){
      name = paste0(column, j)
      data = mutate(data, !!sym(name) := data[, column] %>% lag(n=j))
    }
  }
  
  return(data)
}


difference_data = function(data, var, log_diff = FALSE, lag = 1) {
  name = paste0(var, "_DIFF")
  if (!log_diff) {

    data[, paste0(var, "_DIFF")] = c(rep(NA, lag), data[, var] %>% diff(lag = lag))
  }
  else {

    data[, paste0(var, "_DIFF")] = c(rep(NA, lag), data[, var] %>% log() %>% diff(lag = lag))
  }
  return(data)
}

pct_change = function(data, var) {
  data[, paste0(var, "_DIFF")] = c(NA, data[, var] %>% diff(lag = 12)) / (data[, var] %>% lag(n=12L))
  
}

calculate_average_sp500_return = function(df, return_period) {
  series1 = fredr(series_id="SP500", observation_start=as.Date("1950-01-01"), frequency='d') %>% 
     drop_cols(name="SP500") %>% 
    as.data.frame()
  series1[, 'year'] = series1[, 'date'] %>% sapply(extract_year)
  series1[, 'month'] = series1[, 'date'] %>% sapply(extract_month)
  
  df[, 'year'] = df[, 'date'] %>% sapply(extract_year)
  df[, 'month'] = df[, 'date'] %>% sapply(extract_month)
  
  series1 = na.omit(series1)
  series1[, "RET"] = 1+(c(NA, diff(series1[, "SP500"])) / lag(series1[, "SP500"], n=return_period))
  series1$log_RET = series1$RET %>% log()
  
  series1 = series1 %>% na.omit()
  
  df2 = series1 %>% 
    group_by(year, month) %>% 
    summarise(
      average_log_return = mean(log_RET)
    ) %>% 
    as_tibble()
  
  df = merge(df, df2, by=c('year', 'month'))
  df[, 'average_return'] = exp(df[, 'average_log_return'])
  return(df)
}


plot_fitted_and_weights = function(model, data, predictor, target) {
  fitted = model$fitted.values
  residuals_df = data.frame(fitted=fitted, x=data[, predictor])
  data$weights = model$rweights
  
  plot = ggplot(data=data, aes(x=!!sym(predictor), y=!!sym(target))) +
    geom_point(aes(color=weights)) +
    geom_line(data=residuals_df , aes(x=x, y=fitted), color='black') +
    scale_colour_gradient(low = "yellow", high = "red")
  
  print(plot)
}



