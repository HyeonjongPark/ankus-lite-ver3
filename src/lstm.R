# Core Tidyverse
#library(tidyverse)
library(glue)
library(forcats)


#install.packages("timetk")
#install.packages("tidyquant")
#install.packages("tibbletime")

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

#install.packages("cowplot")

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

#install.packages("rsample")
#install.packages("yardstick")

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)
library(ggplot2)
library(forecast)

#install.packages("MLmetrics")
library(MLmetrics)
conflict_prefer("week", "lubridate")





func = function(x, preprocessing) {
  
  if(preprocessing == 1) { # 1일경우 2015년부터 판매한 상품을 추출후 전처리 작업 실시
    goods2020 = x %>% filter(invoicedate >= "2020-01-01") %>% select(itemname) %>% unique()
    goods2020 = goods2020$itemname
    
    
    goods2015 = x %>% filter(invoicedate >= "2015-01-01" & invoicedate <= "2015-04-01") %>% select(itemname) %>% unique()
    goods2015 = goods2015$itemname
    
    
    current_sell2 = goods2020[goods2020 %in% goods2015] 
    
    sg_test = x %>% filter(itemname == current_sell2)
    
    data = sg_test %>% group_by(yearmonth) %>% summarise(sum_qty = sum(qty))
    data$yearmonth = paste0(data$yearmonth, "-01")
    data$yearmonth = as.Date(data$yearmonth, "%Y-%m-%d")
    
    return(data)
  }
  else { # 1이 아닐 경우, 모든상품에 대해 작업 실시
    data = x %>% group_by(yearmonth) %>% summarise(sum_qty = sum(qty))
    data$yearmonth = paste0(data$yearmonth, "-01")
    data$yearmonth = as.Date(data$yearmonth, "%Y-%m-%d")
    
    return(data)
    
  }
}



data = func(sgsp_plus %>% filter(item == "땅콩류", custclass == "대리점") , 0)
hs_plus$custname %>% unique()

data = func(hs_plus %>% filter(custclass != "홍콩", custname == "쿠팡") , 0)


ts_func = function(x, start_year, end_year) {
  data_ts <- x %>% 
    padr::pad() %>%  # complete data
    pull(sum_qty) %>%
    ts(frequency = 12, start = c(year(range(x$yearmonth)[[1]]), 1)) %>% 
    zoo::na.fill(fill = "extend")
  
  training_ts <- window(data_ts, start = c(start_year, 1), end = c(end_year, 1))
  testing_ts <- window(data_ts, start = c(end_year, 1), end = c(end_year, 12))
  
  autoplot(training_ts, series = "Training Data") + 
    autolayer(testing_ts, series = "Test Data")
  
  ## ets 모델
  
  model_AAA <- ets(training_ts, model = "AAA")
  
  forecast_AAA <- forecast(model_AAA, h = 12 * 1)
  
  conflict_prefer("RMSE", "MLmetrics")
  aaa_rmse = RMSE(y_pred = forecast_AAA$mean, y_true = testing_ts) ## 9889.019
  
  
  
  ## hw 모델
  
  model_hw <- HoltWinters(training_ts)
  
  forecast_hw <- forecast(model_hw, h = 12 * 1)
  
  hw_rmse = RMSE(y_pred = forecast_hw$mean, y_true = testing_ts) ## 8704.417
  
  
  if(aaa_rmse < hw_rmse) {
    print(aaa_rmse)
    return(autoplot(forecast_AAA, series = "AAA") +
      autolayer(testing_ts, series = "Actual"))
  }
  
  else {
    print(hw_rmse)
    return(autoplot(forecast_hw, series = "HoltWinters") +
      autolayer(testing_ts, series = "Actual"))

  }
}

ts_func(data, 2017, 2019)




conflict_prefer("year", "lubridate")
library(lubridate)


data_ts <- data %>% 
  padr::pad() %>%  # complete data
  pull(sum_qty) %>%
  ts(frequency = 12, start = c(year(range(data$yearmonth)[[1]]), 1)) %>% 
  zoo::na.fill(fill = "extend")

autoplot(decompose(data_ts))

autoplot(decompose(window(data_ts, start = c(2017, 1), end = c(2019, 12))))


training_ts <- window(data_ts, start = c(2017, 1), end = c(2019, 1))
testing_ts <- window(data_ts, start = c(2019, 1), end = c(2019, 12))

autoplot(training_ts, series = "Training Data") + 
  autolayer(testing_ts, series = "Test Data")




## ets 모델

model_AAA <- ets(training_ts, model = "AAA")
autoplot(model_AAA$fitted, series = "AAA") +
  autolayer(training_ts, series = "Actual")

forecast_AAA <- forecast(model_AAA, h = 12 * 1)
autoplot(forecast_AAA, series = "AAA") +
  autolayer(testing_ts, series = "Actual")

conflict_prefer("RMSE", "MLmetrics")
RMSE(y_pred = forecast_AAA$mean, y_true = testing_ts) ## 9889.019



## hw 모델

model_hw <- HoltWinters(training_ts)
autoplot(model_hw$fitted[,1], "HoltWinters") +
  autolayer(training_ts, series = "Actual")

forecast_hw <- forecast(model_hw, h = 12 * 1)


autoplot(forecast_hw, series = "HoltWinters") +
  autolayer(testing_ts, series = "Actual")


RMSE(y_pred = forecast_hw$mean, y_true = testing_ts) ## 8704.417



## arima 모델

training_ts %>% 
  tseries::adf.test()

training_ts %>% 
  diff() %>%
  tseries::adf.test()

training_ts %>% 
  diff() %>% 
  pacf()

arima010 <- arima(training_ts, order = c(0,1,0))
arima010

arima010$residuals %>% pacf()

arima014 <- arima(training_ts, order = c(0,1,4))
arima014

arimaAuto <- auto.arima(training_ts, seasonal = F)
arimaAuto


RMSE(forecast(arima010, h = 12 * 1)$mean, testing_ts) ## 12816.3
RMSE(forecast(arima014, h = 12 * 1)$mean, testing_ts) ## 14175.59
RMSE(forecast(arimaAuto, h = 12 * 1)$mean, testing_ts) ## 14633.16

arimaAuto_ms <- auto.arima(training_ts)
arimaAuto_ms

RMSE(forecast(arimaAuto, h = 12 * 1)$mean, testing_ts)
RMSE(forecast(arimaAuto_ms, h = 12 * 1)$mean, testing_ts)



arimaAuto
autoplot(arimaAuto$fitted, "arima") +
  autolayer(training_ts, series = "Actual")

forecast_arima <- forecast(arimaAuto, h = 12 * 1)


autoplot(forecast_arima, series = "arima") +
  autolayer(testing_ts, series = "Actual")


RMSE(y_pred = forecast_arima$mean, y_true = testing_ts) ## 8704.417
















## lstm





### test 용###--------------------------------------------------------------------
func2 = function(x) {
  x$year = substr(x$yearmonth, 1, 4)
  x$week = week(x$invoicedate)
  x$yearweek = paste0(x$year, "-", x$week)
  data = x %>% group_by(yearweek) %>% summarise(sum_qty = sum(qty))
  
  data$year = substr(data$yearweek, 1, 4)
  data$week = substr(data$yearweek, 6, 7)
  
  data$year = as.integer(data$year)
  data$week = as.integer(data$week)
  
  conflict_prefer("arrange", "dplyr")
  data = data %>% arrange(year, week)
  data$yearweek = factor(data$yearweek, levels = data$yearweek)
  return(data)
}


data2 = func2(sg_test %>% filter(item == "너트류"))





data2 %>% ggplot(aes(x = yearweek, y = sum_qty, group = 1)) + geom_line(color = "blue") +
  theme(title = element_text(size = 15),
        axis.text.x = element_text(size=8,
                                   face='italic',
                                   angle=90))

data2

data2 = sg_test %>% filter(item == "너트류", custclass == "할인점") %>% group_by(invoicedate) %>% summarise(sum_qty = sum(qty))

data2
#-------------------------------------------------------------------------------------


df_lstm <- data.frame(
  list(
    "h-3" = c(),
    "h-2" = c(),
    "h-1" = c(),
    "h+0" = c(),
    "target" = c()
  )
)

data2

for(i in 1:nrow(data2)) {
  h1 <- ifelse(i - 1 <= 0, NA, data[i - 1, ]$sum_qty)
  h2 <- ifelse(i - 2 <= 0, NA, data[i - 2, ]$sum_qty)
  h3 <- ifelse(i - 3 <= 0, NA, data[i - 3, ]$sum_qty)
  target <- ifelse(i + 1 > nrow(data), NA, data[i + 1, ]$sum_qty)
  de <- data.frame(h3, h2, h1, data[i, ]$sum_qty, target)
  names(de) <- c("h-3", "h-2", "h-1", "h+0", "target")
  df_lstm <- rbind(df_lstm, de)
}
df_lstm

df_lstm = na.omit(df_lstm)
df_lstm <- head(df_lstm, 320)
df_lstm %>% dim

train_lstm <- df_lstm[1:200,]
test_lstm <- df_lstm[201:240,]

mean_train <- mean(train_lstm$`h+0`)
sd_train <- sd(train_lstm$`h+0`)
train_lstm$`h-3` <- sapply(train_lstm$`h-3`, function(x) {
  return((x - mean_train) / sd_train)
})
train_lstm$`h-2` <- sapply(train_lstm$`h-2`, function(x) {
  return((x - mean_train) / sd_train)
})
train_lstm$`h-1` <- sapply(train_lstm$`h-1`, function(x) {
  return((x - mean_train) / sd_train)
})
train_lstm$`h+0` <- sapply(train_lstm$`h+0`, function(x) {
  return((x - mean_train) / sd_train)
})
train_lstm$`target` <- sapply(train_lstm$`target`, function(x) {
  return((x - mean_train) / sd_train)
})

test_lstm$`h-3` <- sapply(test_lstm$`h-3`, function(x) {
  return((x - mean_train) / sd_train)
})
test_lstm$`h-2` <- sapply(test_lstm$`h-2`, function(x) {
  return((x - mean_train) / sd_train)
})
test_lstm$`h-1` <- sapply(test_lstm$`h-1`, function(x) {
  return((x - mean_train) / sd_train)
})
test_lstm$`h+0` <- sapply(test_lstm$`h+0`, function(x) {
  return((x - mean_train) / sd_train)
})
test_lstm$`target` <- sapply(test_lstm$`target`, function(x) {
  return((x - mean_train) / sd_train)
})


X_train <- array(data.matrix(select(train_lstm, -target)), dim = c(200, 4, 1))
X_test <- array(data.matrix(select(test_lstm, -target)), dim = c(40, 4, 1))
y_train <- array(train_lstm$target, dim = c(200, 1))
y_test <- array(test_lstm$target, dim = c(40, 1))



lstm_model <- keras_model_sequential() %>%
  layer_lstm(units = 64,
             batch_input_shape = c(10, 4, 1), # batch size, step, features
             return_sequences = TRUE,
             stateful = TRUE)  %>%
  layer_lstm(units = 32,
             return_sequences = FALSE,
             stateful = TRUE) %>%
  layer_dense(units = 1)

lstm_model %>%
  compile(loss = 'mse', optimizer = 'adam')
summary(lstm_model)


history <- lstm_model %>% fit( 
  X_train, 
  y_train,
  batch_size = 10,
  epochs = 500, 
  shuffle = FALSE
)


MSE(predict(lstm_model, X_test, batch_size = 10), y_test)
pred <- ts(predict(lstm_model, X_test, batch_size = 10))
actual <- ts(y_test)
autoplot(pred, series = "Prediction") +
  autolayer(actual, series = "Actual")





##  lstm 2번째 방법







