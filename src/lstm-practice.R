library(keras)
library(tensorflow)
library(ggplot2)
library(MLmetrics)
library(data.table)
rm(list=ls())
# 
getwd()
setwd("E:/data-analysis/ankus/ankus-lite-ver2")
amzn <- read.csv("./original-data/input/amzn.us.txt")
googl <- read.csv("./original-data/input/googl.us.txt")
aapl <- read.csv("./original-data/input/aapl.us.txt")
fb <- read.csv("./original-data/input/fb.us.txt")
msft <- read.csv("./original-data/input/msft.us.txt")
ss = read.csv("./original-data/input/samsung.csv")
ss$X = NULL

amzn %>% as.data.table
googl %>% as.data.table
aapl %>% as.data.table
fb %>% as.data.table
msft %>% as.data.table
ss %>% as.data.table


amzn$Date <- as.Date(amzn$Date)
googl$Date <- as.Date(googl$Date)
aapl$Date <- as.Date(aapl$Date)
fb$Date <- as.Date(fb$Date)
msft$Date <- as.Date(msft$Date)
ss$Date <- as.Date(ss$Date)

Series_am<-amzn[,5]
Series_go<-googl[,5]
Series_ap<-aapl[,5]
Series_fb<-fb[,5]
Series_ms<-msft[,5]
Series_ss = ss[,5]
Series_ss2 = ss[,4]

options(scipen = 100)

ggplot(amzn, aes(Date, Close)) + geom_line(color = "#00AFBB")+ ylab("주가")+ xlab("시간")+ggtitle("Amazon.com")
ggplot(googl, aes(Date, Close)) + geom_line(color = "red")+ ylab("주가")+ xlab("시간")+ggtitle("Alphabet Inc.")
ggplot(aapl, aes(Date, Close)) + geom_line(color = "blue")+ ylab("주가")+ xlab("시간")+ggtitle("Apple, Inc.")
ggplot(fb, aes(Date, Close)) + geom_line(color = "green")+ ylab("주가")+ xlab("시간")+ggtitle("Facebook, Inc.")
ggplot(msft, aes(Date, Close)) + geom_line(color = "orange")+ ylab("주가")+ xlab("시간")+ggtitle("Microsoft Corp.")
ggplot(ss, aes(Date, Close)) + geom_line(color = "blue")+ ylab("주가")+ xlab("시간")+ggtitle("Samsung, Inc.")


diffed_am = diff(Series_am, differences = 1)
diffed_go = diff(Series_go, differences = 1)
diffed_ap = diff(Series_ap, differences = 1)
diffed_fb = diff(Series_fb, differences = 1)
diffed_ms = diff(Series_ms, differences = 1)
diffed_ss = diff(Series_ss, differences = 1)
diffed_ss2 = diff(Series_ss2, differences = 1)

lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}

supervised_am = lag_transform(diffed_am, 1)
supervised_go = lag_transform(diffed_go, 1)
supervised_ap = lag_transform(diffed_ap, 1)
supervised_fb = lag_transform(diffed_fb, 1)
supervised_ms = lag_transform(diffed_ms, 1)
supervised_ss = lag_transform(diffed_ss, 1)
supervised_ss2 = lag_transform(diffed_ss2, 1)


N_am = nrow(supervised_am)
n_am = round(N_am *0.7, digits = 0)
train_am = supervised_am[1:n_am, ]
test_am  = supervised_am[(n_am+1):N_am,  ]

N_go = nrow(supervised_go)
n_go = round(N_go *0.7, digits = 0)
train_go = supervised_go[1:n_go, ]
test_go  = supervised_go[(n_go+1):N_go,  ]

N_ap = nrow(supervised_ap)
n_ap = round(N_ap *0.7, digits = 0)
train_ap = supervised_ap[1:n_ap, ]
test_ap  = supervised_ap[(n_ap+1):N_ap,  ]

N_fb = nrow(supervised_fb)
n_fb = round(N_fb *0.7, digits = 0)
train_fb = supervised_fb[1:n_fb, ]
test_fb  = supervised_fb[(n_fb+1):N_fb,  ]

N_ms = nrow(supervised_ms)
n_ms = round(N_ms *0.7, digits = 0)
train_ms = supervised_ms[1:n_ms, ]
test_ms  = supervised_ms[(n_ms+1):N_ms,  ]

N_ss = nrow(supervised_ss)
n_ss = round(N_ss *0.7, digits = 0)
train_ss = supervised_ss[1:n_ss, ]
test_ss  = supervised_ss[(n_ss+1):N_ss,  ]

N_ss2 = nrow(supervised_ss2)
n_ss2 = round(N_ss2 *0.7, digits = 0)
train_ss2 = supervised_ss2[1:n_ss2, ]
test_ss2  = supervised_ss2[(n_ss2+1):N_ss2,  ]


scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
}

Scaled_am = scale_data(train_am, test_am, c(-1, 1))
Scaled_go = scale_data(train_go, test_go, c(-1, 1))
Scaled_ap = scale_data(train_ap, test_ap, c(-1, 1))
Scaled_fb = scale_data(train_fb, test_fb, c(-1, 1))
Scaled_ms = scale_data(train_ms, test_ms, c(-1, 1))
Scaled_ss = scale_data(train_ss, test_ss, c(-1, 1))
Scaled_ss2 = scale_data(train_ss2, test_ss2, c(-1, 1))


y_train_am = Scaled_am$scaled_train[, 2]
x_train_am = Scaled_am$scaled_train[, 1]
y_test_am = Scaled_am$scaled_test[, 2]
x_test_am = Scaled_am$scaled_test[, 1]

y_train_go = Scaled_go$scaled_train[, 2]
x_train_go = Scaled_go$scaled_train[, 1]
y_test_go = Scaled_go$scaled_test[, 2]
x_test_go = Scaled_go$scaled_test[, 1]

y_train_ap = Scaled_ap$scaled_train[, 2]
x_train_ap = Scaled_ap$scaled_train[, 1]
y_test_ap = Scaled_ap$scaled_test[, 2]
x_test_ap = Scaled_ap$scaled_test[, 1]

y_train_fb = Scaled_fb$scaled_train[, 2]
x_train_fb = Scaled_fb$scaled_train[, 1]
y_test_fb = Scaled_fb$scaled_test[, 2]
x_test_fb = Scaled_fb$scaled_test[, 1]

y_train_ms = Scaled_ms$scaled_train[, 2]
x_train_ms = Scaled_ms$scaled_train[, 1]
y_test_ms = Scaled_ms$scaled_test[, 2]
x_test_ms = Scaled_ms$scaled_test[, 1]

y_train_ss = Scaled_ss$scaled_train[, 2]
x_train_ss = Scaled_ss$scaled_train[, 1]
y_test_ss = Scaled_ss$scaled_test[, 2]
x_test_ss = Scaled_ss$scaled_test[, 1]

y_train_ss2 = Scaled_ss2$scaled_train[, 2]
x_train_ss2 = Scaled_ss2$scaled_train[, 1]
y_test_ss2 = Scaled_ss2$scaled_test[, 2]
x_test_ss2 = Scaled_ss2$scaled_test[, 1]


invert_scaling = function(Scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(Scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (Scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

#### *Построение модели на обучающей выборке*

batch_size = 1
units = 1


dim(x_train_am) <- c(length(x_train_am), 1, 1)
X_shape2_am = dim(x_train_am)[2]
X_shape3_am = dim(x_train_am)[3]

dim(x_train_go) <- c(length(x_train_go), 1, 1)
X_shape2_go = dim(x_train_go)[2]
X_shape3_go = dim(x_train_go)[3]

dim(x_train_ap) <- c(length(x_train_ap), 1, 1)
X_shape2_ap = dim(x_train_ap)[2]
X_shape3_ap = dim(x_train_ap)[3]

dim(x_train_fb) <- c(length(x_train_fb), 1, 1)
X_shape2_fb = dim(x_train_fb)[2]
X_shape3_fb = dim(x_train_fb)[3]

dim(x_train_ms) <- c(length(x_train_ms), 1, 1)
X_shape2_ms = dim(x_train_ms)[2]
X_shape3_ms = dim(x_train_ms)[3]

x_train_ss = c(x_train_ss, x_train_ss2)
x_test_ss = c(x_test_ss, x_test_ss2)
y_train_ss = cbind(y_train_ss, y_train_ss2)
y_test_ss = cbind(y_test_ss, y_test_ss2)


## 실험
x_train_ss <- array(x_train_ss, c(length(x_train_ss)/2, 1, 2))
dim(x_train_ss)
X_shape2_ss = dim(x_train_ss)[2]
X_shape3_ss = dim(x_train_ss)[3]

x_test_ss <- array(x_test_ss, c(length(x_test_ss)/2, 1, 2))


model_am <- keras_model_sequential() 
model_am%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2_am, X_shape3_am), stateful= TRUE)%>%
  layer_dense(units = 1)

model_go <- keras_model_sequential() 
model_go%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2_go, X_shape3_go), stateful= TRUE)%>%
  layer_dense(units = 1)

model_ap <- keras_model_sequential() 
model_ap%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2_ap, X_shape3_ap), stateful= TRUE)%>%
  layer_dense(units = 1)

model_fb <- keras_model_sequential() 
model_fb%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2_fb, X_shape3_fb), stateful= TRUE)%>%
  layer_dense(units = 1)

model_ms <- keras_model_sequential() 
model_ms%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2_ms, X_shape3_ms), stateful= TRUE)%>%
  layer_dense(units = 1)

model_ss <- keras_model_sequential() 
model_ss%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2_ss, X_shape3_ss), stateful= TRUE)%>%
  layer_dense(units = 1)



model_am %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)
summary(model_am)

model_go %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)
summary(model_go)

model_ap %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)
summary(model_ap)

model_fb %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)
summary(model_fb)

model_ms %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)
summary(model_ms)

model_ss %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)
summary(model_ss)


Epochs = 50   
for(i in 1:Epochs ){
  model_am %>% fit(x_train_am, y_train_am, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model_am %>% reset_states()
}

for(i in 1:Epochs ){
  model_go %>% fit(x_train_go, y_train_go, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model_go %>% reset_states()
}

for(i in 1:Epochs ){
  model_ap %>% fit(x_train_ap, y_train_ap, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model_ap %>% reset_states()
}

for(i in 1:Epochs ){
  model_fb %>% fit(x_train_fb, y_train_fb, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model_fb %>% reset_states()
}

for(i in 1:Epochs ){
  model_ms %>% fit(x_train_ms, y_train_ms, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model_ms %>% reset_states()
}

for(i in 1:Epochs ){
  model_ss %>% fit(x_train_ss, y_train_ss, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model_ss %>% reset_states()
}


L_am = length(x_test_am)
scaler_am = Scaled_am$scaler
predictions_am = numeric(L_am)
for(i in 1:L_am){
  X_am = x_test_am[i]
  dim(X_am) = c(1,1,1)
  yhat = model_am %>% predict(X_am, batch_size=batch_size)
  # invert scaling
  yhat_am = invert_scaling(yhat, scaler_am,  c(-1, 1))
  # invert differencing
  yhat_am  = yhat_am + Series_am[(n_am+i)]
  # store
  predictions_am[i] <- yhat_am
}

L_go = length(x_test_go)
scaler_go = Scaled_go$scaler
predictions_go = numeric(L_go)
for(i in 1:L_go){
  X_go = x_test_go[i]
  dim(X_go) = c(1,1,1)
  yhat_go = model_go %>% predict(X_go, batch_size=batch_size)
  # invert scaling
  yhat_go = invert_scaling(yhat, scaler_go,  c(-1, 1))
  # invert differencing
  yhat_go  = yhat_go + Series_go[(n_go+i)]
  # store
  predictions_go[i] <- yhat_go
}

L_ap = length(x_test_ap)
scaler_ap = Scaled_ap$scaler
predictions_ap = numeric(L_ap)
for(i in 1:L_ap){
  X_ap = x_test_ap[i]
  dim(X_ap) = c(1,1,1)
  yhat_ap = model_ap %>% predict(X_ap, batch_size=batch_size)
  # invert scaling
  yhat_ap = invert_scaling(yhat, scaler_ap,  c(-1, 1))
  # invert differencing
  yhat_ap  = yhat_ap + Series_ap[(n_ap+i)]
  # store
  predictions_ap[i] <- yhat_ap
}

L_fb = length(x_test_fb)
scaler_fb = Scaled_fb$scaler
predictions_fb = numeric(L_fb)
for(i in 1:L_fb){
  X_fb = x_test_fb[i]
  dim(X_fb) = c(1,1,1)
  yhat_fb = model_fb %>% predict(X_fb, batch_size=batch_size)
  # invert scaling
  yhat_fb = invert_scaling(yhat, scaler_fb,  c(-1, 1))
  # invert differencing
  yhat_fb  = yhat_fb + Series_fb[(n_fb+i)]
  # store
  predictions_fb[i] <- yhat_fb
}

L_ms = length(x_test_ms)
scaler_ms = Scaled_ms$scaler
predictions_ms = numeric(L_ms)
for(i in 1:L_ms){
  X_ms = x_test_ms[i]
  dim(X_ms) = c(1,1,1)
  yhat_ms = model_ms %>% predict(X_ms, batch_size=batch_size)
  # invert scaling
  yhat_ms = invert_scaling(yhat, scaler_ms,  c(-1, 1))
  # invert differencing
  yhat_ms  = yhat_ms + Series_ms[(n_ms+i)]
  # store
  predictions_ms[i] <- yhat_ms
}

L_ss = length(x_test_ss[,,1])
scaler_ss = Scaled_ss$scaler
predictions_ss = numeric(L_ss)
for(i in 1:L_ss){
  X_ss = x_test_ss[,,][i,]
  dim(X_ss) = c(1,1,2)
  yhat = model_ss %>% predict(X_ss, batch_size=batch_size)
  # invert scaling
  yhat_ss = invert_scaling(yhat, scaler_ss,  c(-1, 1))
  # invert differencing
  yhat_ss  = yhat_ss + Series_ss[(n_ss+i)]
  # store
  predictions_ss[i] <- yhat_ss
}




RMSE(predictions_am,Series_am[3607:5152])
RMSE(predictions_go,Series_go[2333:3332])
RMSE(predictions_ap,Series_ap[5855:8363])
RMSE(predictions_fb,Series_fb[967:1380])
RMSE(predictions_ms,Series_ms[5588:7982])
RMSE(predictions_ss,Series_ss[4194:5990])

MAPE(predictions_am,Series_am[3607:5152])
MAPE(predictions_go,Series_go[2333:3332])
MAPE(predictions_ap,Series_ap[5855:8363])
MAPE(predictions_fb,Series_fb[967:1380])
MAPE(predictions_ms,Series_ms[5588:7982])
MAPE(predictions_ss,Series_ss[4194:5990])


data_am<-amzn[3607:5152,]
data_am$Date <- as.Date(data_am$Date)
merge_am<-cbind(data_am,predictions_am)
ggplot(data = merge_am, aes(x = Date, y = Close))+ 
  geom_line(col='red')+
  geom_line(aes(y=predictions_am),col='blue')+ ylab("주가")+ xlab("시간")+ggtitle("Amazon.com")


data_go<-googl[2333:3332,]
data_go$Date <- as.Date(data_go$Date)
merge_go<-cbind(data_go,predictions_go)
ggplot(data = merge_go, aes(x = Date, y = Close))+ 
  geom_line(col='red')+
  geom_line(aes(y=predictions_go),col='blue')+ ylab("주가")+ xlab("시간")+ggtitle("Alphabet Inc.")

data_ap<-aapl[5855:8363,]
data_ap$Date <- as.Date(data_ap$Date)
merge_ap<-cbind(data_ap,predictions_ap)
ggplot(data = merge_ap, aes(x = Date, y = Close))+ 
  geom_line(col='red')+
  geom_line(aes(y=predictions_ap),col='blue')+ ylab("주가")+ xlab("시간")+ggtitle("Apple, Inc.")

data_fb<-fb[967:1380,]
data_fb$Date <- as.Date(data_fb$Date)
merge_fb<-cbind(data_fb,predictions_fb)
ggplot(data = merge_fb, aes(x = Date, y = Close))+ 
  geom_line(col='red')+
  geom_line(aes(y=predictions_fb),col='blue')+ ylab("주가")+ xlab("시간")+ggtitle("Facebook, Inc.")

data_ms<-msft[5588:7982,]
data_ms$Date <- as.Date(data_ms$Date)
merge_ms<-cbind(data_ms,predictions_ms)
ggplot(data = merge_ms, aes(x = Date, y = Close))+ 
  geom_line(col='red')+
  geom_line(aes(y=predictions_ms),col='blue')+ ylab("주가")+ xlab("시간")+ggtitle("Microsoft Corp.")


data_ss<-ss[4194:5990,]
data_ss$Date <- as.Date(data_ss$Date)
data_ss %>% nrow()
predictions_ss %>% length
merge_ss<-cbind(data_ss,predictions_ss)
ggplot(data = merge_ss, aes(x = Date, y = Close))+ 
  geom_line(col='red')+
  geom_line(aes(y=predictions_ss+260),col='blue')+ ylab("주가")+ xlab("시간")+ggtitle("Microsoft Corp.")



