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



data = func(sgsp_plus %>% filter(item == "너트류", custclass == "할인점") , 0)

data = func(sgsp_minus %>% filter(item == "땅콩류") , 0)

hs_plus$itemname %>% table %>% sort(decreasing = T)
data = func(hs_plus %>% filter(custclass != "홍콩") , 0)

data = func(hs_plus %>% filter(custclass != "홍콩", itemname == "지에스 고소한쇠고기육포_20") , 0)

data = func(hs_minus %>% filter(custclass != "홍콩") , 0)

data = func(mgb_plus %>% filter(item == "머거본", custclass == "대리점") , 0)

conflict_prefer("arrange", "dplyr")


data
data_t = hs_plus %>% filter(custclass != "홍콩") %>% group_by(yearmonth) %>% summarise(sumqty = sum(qty))

data_t$month = substr(data_t$yearmonth, 6, 7)
data_t$year = substr(data_t$yearmonth, 1, 4)
data_t$season = ifelse(data_t$month %in% c("12","01","02") , "winter",
                       ifelse(data_t$month %in% c("03","04","05") , "srping",
                              ifelse(data_t$month %in% c("06","07","08") , "summer","fall")))
                                     

data_t_2017 = data_t %>% filter(year == "2017")
data_t_2017$qty_ratio = data_t_2017$sumqty / sum(data_t_2017$sumqty) * 100

lm(data_t$sumqty ~ data_t$month + data_t$year + data_t$season) %>% summary()

ts_func = function(x, start_year, end_year) {
  data_ts <- x %>% 
    padr::pad() %>%  # complete data
    pull(sum_qty) %>%
    ts(frequency = 12, start = c(year(range(x$yearmonth)[[1]]), 1)) %>% 
    zoo::na.fill(fill = "extend")
  
  training_ts <- window(data_ts, start = c(start_year, 1), end = c(end_year, 1))
  testing_ts <- window(data_ts, start = c(end_year, 1), end = c(end_year, 12))
  
  at =autoplot(training_ts, series = "Training Data") + 
    autolayer(testing_ts, series = "Test Data")
  
  ## ets 모델
  
  model_AAA <- ets(training_ts, model = "AAA")
  
  forecast_AAA <- forecast(model_AAA, h = 12 * 1)
  

  aaa_rmse = RMSE(y_pred = forecast_AAA$mean, y_true = testing_ts) ## 9889.019
  aaa_mape = MAPE(y_pred = forecast_AAA$mean, y_true = testing_ts)
  
  
  ## hw 모델
  
  model_hw <- HoltWinters(training_ts)
  
  forecast_hw <- forecast(model_hw, h = 12 * 1)
  
  hw_rmse = RMSE(y_pred = forecast_hw$mean, y_true = testing_ts) ## 8704.417
  hw_mape = MAPE(y_pred = forecast_hw$mean, y_true = testing_ts)
  
  ## arima 모델
  
  arima010 <- arima(training_ts, order = c(0,1,0))
  arima014 <- arima(training_ts, order = c(0,1,4))
  arimaAuto <- auto.arima(training_ts, seasonal = F)
  
  ar10_mape = MAPE(forecast(arima010, h = 12 * 1)$mean, testing_ts) ## 12816.3
  ar14_mape = MAPE(forecast(arima014, h = 12 * 1)$mean, testing_ts) ## 14175.59
  arauto_mape = MAPE(forecast(arimaAuto, h = 12 * 1)$mean, testing_ts) ## 14633.16
  
  ar10_rmse = RMSE(forecast(arima010, h = 12 * 1)$mean, testing_ts) ## 12816.3
  ar14_rmse = RMSE(forecast(arima014, h = 12 * 1)$mean, testing_ts) ## 14175.59
  arauto_rmse = RMSE(forecast(arimaAuto, h = 12 * 1)$mean, testing_ts) ## 14633.16
  
  
  
  print("aaa rmse and mape ")
  print(aaa_rmse)
  print(aaa_mape)
  
  print("hw rmse and mape")
  print(hw_rmse)
  print(hw_mape)
  
  print("arima10 rmse and mape")
  print(ar10_rmse)
  print(ar10_mape)
  print("arima14 rmse and mape")
  print(ar14_rmse)
  print(ar14_mape)
  print("arima auto rmse and mape")
  print(arauto_rmse)
  print(arauto_mape)
  
  
  if(aaa_mape < hw_mape) {
    return(autoplot(forecast_AAA, series = "AAA") +
      autolayer(testing_ts, series = "Actual"))
  }
  
  else {

    return(autoplot(forecast_hw, series = "HoltWinters") +
      autolayer(testing_ts, series = "Actual"))

  }
}
#library(conflicted)
#conflict_prefer("RMSE", "MLmetrics")
#conflict_prefer("year", "lubridate")
ts_func(data, 2017, 2019)




#conflict_prefer("year", "lubridate")
library(lubridate)


data_ts <- data %>% 
  padr::pad() %>%  # complete data
  pull(sum_qty) %>%
  ts(frequency = 12, start = c(year(range(data$yearmonth)[[1]]), 1)) %>% 
  zoo::na.fill(fill = "extend")

data_ts
autoplot(decompose(data_ts))

autoplot(decompose(window(data_ts, start = c(2015, 1), end = c(2019, 12))))


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

#conflict_prefer("RMSE", "MLmetrics")
RMSE(y_pred = forecast_AAA$mean, y_true = testing_ts) ## 9889.019
MAPE(y_pred = forecast_AAA$mean, y_true = testing_ts) ## 9889.019


aaa = cbind(forecast_AAA, as.data.frame(testing_ts))
aaa = aaa[,c(6,1:5)]
names(aaa)[1] = "real"
aaa

## hw 모델

model_hw <- HoltWinters(training_ts)
autoplot(model_hw$fitted[,1], "HoltWinters") +
  autolayer(training_ts, series = "Actual")

forecast_hw <- forecast(model_hw, h = 12 * 1)


autoplot(forecast_hw, series = "HoltWinters") +
  autolayer(testing_ts, series = "Actual")


RMSE(y_pred = forecast_hw$mean, y_true = testing_ts) ## 8704.417
MAPE(y_pred = forecast_hw$mean, y_true = testing_ts) ## 9889.019

hw = cbind(forecast_hw, as.data.frame(testing_ts))
hw = hw[,c(6,1:5)]
names(hw)[1] = "real"
hw



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


autoplot(forecast(arima010, h = 12 * 1), series = "arima") +
  autolayer(testing_ts, series = "Actual")
autoplot(forecast(arima014, h = 12 * 1), series = "arima") +
  autolayer(testing_ts, series = "Actual")
autoplot(forecast(arimaAuto, h = 12 * 1), series = "arima") +
  autolayer(testing_ts, series = "Actual")


RMSE(forecast(arima010, h = 12 * 1)$mean, testing_ts) ## 12816.3
RMSE(forecast(arima014, h = 12 * 1)$mean, testing_ts) ## 14175.59
RMSE(forecast(arimaAuto, h = 12 * 1)$mean, testing_ts) ## 14633.16

MAPE(forecast(arima010, h = 12 * 1)$mean, testing_ts) ## 12816.3
MAPE(forecast(arima014, h = 12 * 1)$mean, testing_ts) ## 14175.59
MAPE(forecast(arimaAuto, h = 12 * 1)$mean, testing_ts) ## 14633.16


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
  
  data = data %>% arrange(year, week)
  data$yearweek = factor(data$yearweek, levels = data$yearweek)
  return(data)
}

data2 = hs_plus %>% filter(custclass != "홍콩")

data2 =  sgsp_plus %>% filter(item == "너트류", custclass == "대리점")

goods2020 = data2 %>% filter(invoicedate >= "2020-01-01") %>% select(itemname) %>% unique()
goods2020 = goods2020$itemname
goods2020 %>% length

goods2019 = data2 %>% filter(invoicedate >= "2019-01-01" & invoicedate <= "2019-03-01") %>% select(itemname) %>% unique()
goods2019 = goods2019$itemname
goods2019 %>% length

goods2018 = data2 %>% filter(invoicedate >= "2018-01-01" & invoicedate <= "2018-03-01") %>% select(itemname) %>% unique()
goods2018 = goods2018$itemname
goods2018 %>% length


goods2017 = data2 %>% filter(invoicedate >= "2017-01-01" & invoicedate <= "2017-03-01") %>% select(itemname) %>% unique()
goods2017 = goods2017$itemname
goods2017 %>% length

current_sell = goods2020[goods2020 %in% goods2017] 
current_sell = current_sell[current_sell %in% goods2018] 
current_sell = current_sell[current_sell %in% goods2019] 
current_sell %>% length # 17 ~20 까지 꾸준히 판매되고 있는 제품들

hs_plus %>% filter(itemname == current_sell) %>% select(itemname) %>% table()


#conflict_prefer("week", "lubridate")
data2 = func2(hs_plus %>% filter(custclass != "홍콩", itemname == "코리아세븐 쇠고기육포채_16"))
sgsp_plus %>% filter(itemname == "너트프라자 270G") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty)) %>% ggplot(aes(invoicedate, sum_qty)) +
  geom_line(color = "blue")

data2 = hs_plus %>% filter(custclass != "홍콩") %>% 
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))
names(data2)[1] = "yearweek"

data2 = func2(sgsp_plus %>% filter(item == "너트류", custclass == "대리점", itemname == "너트프라자 270G") )

data2 = sgsp_plus %>% filter(item == "너트류", custclass == "할인점")

data2 = sgsp_plus %>% filter(item == "너트류", custclass == "할인점", itemname == "꾸이랑믹스넛 220G") %>% 
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))
names(data2)[1] = "yearweek"


data2 %>% ggplot(aes(x = yearweek, y = sum_qty, group = 1)) + geom_line(color = "blue") +
  theme(title = element_text(size = 15),
        axis.text.x = element_text(size=8,
                                   face='italic',
                                   angle=90))

#data2 = sg_test %>% filter(item == "너트류", custclass == "할인점") %>% group_by(invoicedate) %>% summarise(sum_qty = sum(qty))


#------------------

df_lstm <- data.frame(
  list(
    "h-3" = c(),
    "h-2" = c(),
    "h-1" = c(),
    "h+0" = c(),
    "target" = c()
  )
)

for(i in 1:nrow(data2)) {
  h1 <- ifelse(i - 1 <= 0, NA, data2[i - 1, ]$sum_qty)
  h2 <- ifelse(i - 2 <= 0, NA, data2[i - 2, ]$sum_qty)
  h3 <- ifelse(i - 3 <= 0, NA, data2[i - 3, ]$sum_qty)
  target <- ifelse(i + 1 > nrow(data2), NA, data2[i + 1, ]$sum_qty)
  de <- data.frame(h3, h2, h1, data2[i, ]$sum_qty, target)
  names(de) <- c("h-3", "h-2", "h-1", "h+0", "target")
  df_lstm <- rbind(df_lstm, de)
}
df_lstm

df_lstm = na.omit(df_lstm)
df_lstm %>% dim
#df_lstm <- head(df_lstm, 320)
df_lstm %>% dim

# 120, 24, 12 => 추세 잘반영
# 132, 12, 12 => 잘 반영
train_num = 132
test_num = 12
batch_num = 12

train_lstm <- df_lstm[1:train_num,]
test_lstm <- df_lstm[(train_num + 1):(train_num + test_num),]
test_lstm

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


X_train <- array(data.matrix(select(train_lstm, -target)), dim = c(train_num, 4, 1))
X_test <- array(data.matrix(select(test_lstm, -target)), dim = c(test_num, 4, 1))
y_train <- array(train_lstm$target, dim = c(train_num, 1))
y_test <- array(test_lstm$target, dim = c(test_num, 1))


lstm_model <- keras_model_sequential() %>%
  layer_lstm(units = 64,
             batch_input_shape = c(batch_num, 4, 1), # batch size, step, features
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
  batch_size = batch_num,
  epochs = 300, 
  shuffle = FALSE
)

plot(history)


MSE(predict(lstm_model, X_test, batch_size = batch_num), y_test)

pred <- ts(predict(lstm_model, X_test, batch_size = batch_num))
pred

  
actual <- ts(y_test)
autoplot(pred, series = "Prediction") +
  autolayer(actual, series = "Actual")

RMSE(y_pred = pred, y_true = actual) ## 9889.019
MAPE(y_pred = pred, y_true = actual) ## 9889.019




## 실제 값 예측

p = pred * sd_train + mean_train
#p = p - 500
a = ts(y_test) * sd_train + mean_train



cbind(a,p)
autoplot(p, series = "Prediction") +
  autolayer(a, series = "Actual")

RMSE(y_pred = p, y_true = a) ## 9889.019
MAPE(y_pred = p, y_true = a) ## 9889.019



## 금요일 오전 ui
## 모델 월요일

##  lstm 2번째 방법




















################ 10개 모델 생성
mape_vec = vector()

for(roop in 21:30) {
  set.seed(i)
  
  train_num = 132
  test_num = 12
  batch_num = 12
  
  train_lstm <- df_lstm[1:train_num,]
  test_lstm <- df_lstm[(train_num + 1):(train_num + test_num),]
  
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
  
  
  X_train <- array(data.matrix(select(train_lstm, -target)), dim = c(train_num, 4, 1))
  X_test <- array(data.matrix(select(test_lstm, -target)), dim = c(test_num, 4, 1))
  y_train <- array(train_lstm$target, dim = c(train_num, 1))
  y_test <- array(test_lstm$target, dim = c(test_num, 1))
  
  
  lstm_model <- keras_model_sequential() %>%
    layer_lstm(units = 64,
               batch_input_shape = c(batch_num, 4, 1), # batch size, step, features
               return_sequences = TRUE,
               stateful = TRUE)  %>%
    layer_lstm(units = 32,
               return_sequences = FALSE,
               stateful = TRUE) %>%
    layer_dense(units = 1)
  
  lstm_model %>%
    compile(loss = 'mse', optimizer = 'adam')
  
  history <- lstm_model %>% fit( 
    X_train, 
    y_train,
    batch_size = batch_num,
    epochs = 300, 
    shuffle = FALSE
  )
  
  pred <- ts(predict(lstm_model, X_test, batch_size = batch_num))
  actual <- ts(y_test)
  
  
  ## 실제 값 예측
  
  p = pred * sd_train + mean_train
  #p = p - 500
  a = ts(y_test) * sd_train + mean_train
  
  
  auto_p = autoplot(p, series = "Prediction") +
    autolayer(a, series = "Actual")
  
  print(auto_p)

  print(MAPE(y_pred = p, y_true = a))
  mape_vec = c(mape_vec, MAPE(y_pred = p, y_true = a))
  
}
mape_vec








