library(keras)
library(tensorflow)
library(ggplot2)
library(MLmetrics)


sgsp_plus$qty %>% table

sgsp270 = sgsp_plus %>% filter(item == "아몬드류", custclass == "편의점") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))

sgsp270 = sgsp_plus %>% filter(itemname == "너트프라자 270G") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))

hs_plus %>% filter(custclass != "홍콩") %>% select(itemname) %>% table() %>% sort()

sgsp_plus %>% filter(item == "아몬드류", custclass == "편의점") %>% select(itemname) %>% table() %>% sort()


sgsp270 = sgsp_plus %>% filter(item == "아몬드류", custclass == "편의점", itemname == "보광 칼몬드 100G") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))


sgsp270 = hs_plus %>% filter(custclass != "홍콩", itemname == "지에스 고소한쇠고기육포_20") %>% 
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))


sgsp270 = sgsp_plus %>% filter(item == "너트류") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))

sgsp270 = sgsp_plus %>% filter(item == "너트류", custclass == "할인점") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))

sgsp270 = sgsp_plus %>% filter(item == "너트류", custclass == "할인점", itemname == "너트프라자 270G") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))

sgsp270 = sgsp_plus %>% filter(item == "아몬드류", custclass == "할인점", itemname == "칼몬드 150G (이마트/20입)") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))


sgsp270 = mgb_plus %>% filter(itemname == "믹스넛 22G(오비)", custclass == "오비맥주") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))

sgsp270 = sgsp_minus %>% filter(item == "아몬드류") %>%
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))








# heat map 그리기
sgsp270 = sgsp_plus %>% filter(item == "아몬드류", custclass == "할인점", itemname == cur[1]) %>% 
  group_by(invoicedate, month, itemname,day) %>% summarise(sum_qty = sum(qty))
#sgsp270$month <- factor(sgsp270$month, levels=levels(sgsp270$month))

cur = sgsp270$itemname %>% unique()

setwd("E:/data-analysis/ankus/ankus-lite-ver2/visualization/세계식품-아몬드-할인점")

for(i in 142:length(cur)) {
  sgsp270 = sgsp_plus %>% filter(item == "아몬드류", custclass == "할인점", itemname == cur[i]) %>% 
    group_by(invoicedate, year, week,itemname) %>% summarise(sum_qty = sum(qty))
  
  attach(sgsp270)
  
  
  #overall summary
  
  col1 = "#d8e1cf" 
  col2 = "#438484"
  
  
  ggplot(sgsp270, aes(week, year)) + geom_tile(aes(fill = sum_qty),colour = "white") +
    scale_fill_gradient(low = col1, high = col2) +  
    guides(fill=guide_legend(title="total sale")) +
    labs(title = "frequency",
         x = "Week", y = "Year") +
    theme_bw() + theme_minimal() 
  
  ggsave(paste0(cur[i],".jpg"))
  
  detach(sgsp270)
  
  print(i)
}


sgsp270 = mgb_plus %>% filter(itemname == "믹스넛 22G(오비)", custclass == "오비맥주", year == "2015") %>%
  group_by(invoicedate, month, day, itemname) %>% summarise(sum_qty = sum(qty))

attach(sgsp270)


#overall summary

col1 = "#d8e1cf" 
col2 = "#438484"


ggplot(sgsp270, aes(month, day)) + geom_tile(aes(fill = sum_qty),colour = "white") +
  scale_fill_gradient(low = col1, high = col2) +  
  guides(fill=guide_legend(title="total sale")) +
  labs(title = "frequency",
       x = "month", y = "day") +
  theme_bw() + theme_minimal() 

detach(sgsp270)







## 실험
#1 기존
sgsp270 = sgsp_plus %>% filter(item == "아몬드류", custclass == "할인점", itemname == (current_sell[1])) %>% 
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))

#2 일자 채워넣기
sgsp270 = consome2 %>% filter(item == "아몬드류", custclass == "할인점", itemname == (current_sell[1])) %>% 
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))

#3 주말 제거
sgsp270 = sgsp_plus %>% filter(weekday != "토요일", weekday != "일요일",
                               isholiday == "N",
                               item == "아몬드류", custclass == "할인점", 
                               itemname == (current_sell[1])) %>% 
  group_by(invoicedate) %>% summarise(sum_qty = sum(qty))


Epochs = 8

names(sgsp270) = c("Date", "Close")

Series_sg<-sgsp270[,2]$Close

ggplot(sgsp270, aes(Date, Close)) + geom_line(color = "brown")+ ylab("발주")+ xlab("일별")+ggtitle("일별 발주")


# 비정상성을 제거하기 위해 
diffed_sg = diff(Series_sg, differences = 1)

# 지연변수 생성
lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}

supervised_sg = lag_transform(diffed_sg, 1)

# train용과 test용으로 나누고, 시계열 특성상 순서가 중요하므로 셔플을 하지 않는다.
N_sg = nrow(supervised_sg)
n_sg = round(N_sg *0.8, digits = 0)
train_sg = supervised_sg[1:n_sg, ]
test_sg  = supervised_sg[(n_sg+1):N_sg,  ]



# 데이터 범위를 정규화 하기 위해 feature_range 매개변수 적용
# (0,1)은 분산이 적은 데이터에 일반적
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


# lstm은 기본 활성화 기능은 범위가 -1 ~ 1 인 S자형 기능.
# train과 test 데이터를 스케일링하는 데 사용하는 벡터
# 이를 통해 테스트 데이터의 최소값과 최대값이 모델에 영향을 주지 않는다.
Scaled_sg = scale_data(train_sg, test_sg, c(-1, 1))

y_train_sg = Scaled_sg$scaled_train[, 2]
x_train_sg = Scaled_sg$scaled_train[, 1]
y_test_sg = Scaled_sg$scaled_test[, 2]
x_test_sg = Scaled_sg$scaled_test[, 1]

# 예측값을 원래 스케일로 되돌린다.
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

batch_size = 1        
units = 1

#  샘플 : 각 샘플의 관측치 - batch size
# 타입스텝 : 관측 데이터에 대한 별도의 타입스텝.
dim(x_train_sg) <- c(length(x_train_sg), 1, 1)
X_shape2_sg = dim(x_train_sg)[2]
X_shape3_sg = dim(x_train_sg)[3]

model_sg <- keras_model_sequential() 
model_sg%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2_sg, X_shape3_sg), stateful= TRUE)%>%
  layer_dense(units = 1)

# 평균제곱 오차는 손실함수로 사용되며 ADAM 은 최적화 알고리즘으로 사용
# 정확도는 모델의 정확도를 평가하기위한 metric
model_sg %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)
summary(model_sg)

# 데이터 볼륨에 대한 최적의 epoch =50, 가중치는 50번 이동
# 값이 낮거나 높을수록 과적합 문제 발생

Epochs = 5

for(i in 1:Epochs ){
  model_sg %>% fit(x_train_sg, y_train_sg, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model_sg %>% reset_states()
}



### 실험
Epochs = 500
loss_df = vector()
acc_df = vector()
for(i in 1:Epochs ){
  hist = model_sg %>% fit(x_train_sg, y_train_sg, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  loss_df = c(loss_df, hist$metrics$loss)
  acc_df = c(acc_df, hist$metrics$accuracy)
  model_sg %>% reset_states()
}
plot(loss_df, type = "b")
plot(acc_df, type = "b")

histo = model_sg %>% fit(
  x_train_sg, y_train_sg, epochs=5000,
  batch_size=batch_size,
  verbose = 1,
  validation_split = 0.2
)
#conflict_prefer("arrange", "dplyr")
plot(histo$metrics$loss, type = "b", color = "blue")

###

L_sg = length(x_test_sg)
scaler_sg = Scaled_sg$scaler
predictions_sg = numeric(L_sg)


for(i in 1:L_sg){
  X_sg = x_test_sg[i]
  dim(X_sg) = c(1,1,1)
  yhat = model_sg %>% predict(X_sg, batch_size=batch_size)
  # invert scaling
  yhat_sg = invert_scaling(yhat, scaler_sg,  c(-1, 1))
  # invert differencing
  yhat_sg  = yhat_sg + Series_sg[(n_sg+i)]
  # store
  predictions_sg[i] <- yhat_sg
}


len1 = length(predictions_sg)
len2 = length(Series_sg)
len1


RMSE(predictions_sg,Series_sg[(len2-len1+1):len2])
MAPE(predictions_sg,Series_sg[(len2-len1+1):len2])


data_sg<-sgsp270[(len2-len1+1):len2,]

### 앞에 트레인 부분 추가해서 그래프 그려보기
#data_sg<-sgsp270[1:len2,]


data_sg$Date <- as.Date(data_sg$Date)
merge_sg<-cbind(data_sg,predictions_sg)
names(merge_sg) = c("Date", "Close", "predictions_sg")

diff = mean(merge_sg$Close) - mean(merge_sg$predictions_sg)

merge_sg$predictions_sg = merge_sg$predictions_sg + diff

RMSE(merge_sg$predictions_sg , merge_sg$Close)
MAPE(merge_sg$predictions_sg , merge_sg$Close)

ggplot(data = merge_sg, aes(x = Date, y = Close))+ 
  geom_line(col='red')+
  geom_line(aes(y=predictions_sg),col='blue')+ ylab("일 별 발주량")+ xlab("일별")+ggtitle("제품명")







#conflict_prefer("month", "lubridate")


merge_sg$year = year(merge_sg$Date)
merge_sg$month = month(merge_sg$Date)
merge_sg$week = week(merge_sg$Date)

merge_sg$yearweek = paste0(merge_sg$year,"-",merge_sg$week)
merge_sg$yearmonth = paste0(merge_sg$year,"-",merge_sg$month)


merge_sg$year = as.integer(merge_sg$year)
merge_sg$month = as.integer(merge_sg$month)
merge_sg$week = as.integer(merge_sg$week)


## 주별

merge_sg2 = merge_sg %>% arrange(year, week)
merge_sg2 %>% str

## 월별
#merge_sg2 = merge_sg %>% arrange(year, month)



merge_sg2$yearweek = factor(merge_sg2$yearweek, levels = unique(merge_sg2$yearweek))

## 주별

merge_sg2 = merge_sg %>% group_by(factor(yearweek, levels = unique(yearweek))) %>% summarise(Close_t = sum(Close), predictions_sg_t = sum(predictions_sg))

## 월별
#merge_sg2 = merge_sg %>% group_by(factor(yearmonth, levels = unique(yearmonth))) %>% summarise(Close_t = sum(Close), predictions_sg_t = sum(predictions_sg))


#diff = mean(merge_sg2$Close_t) - mean(merge_sg2$predictions_sg_t)

merge_sg2 %>% as.data.frame()

names(merge_sg2) = c("yearweek", "Close_t", "predictions_sg_t")

ggplot(data = merge_sg2, aes(x = yearweek, y = Close_t, group = 1))+ 
  geom_line(col='red')+
  geom_line(aes(y=predictions_sg_t),col='blue')+ ylab("주별 발주량")+ xlab("주 단위")+ggtitle("홍선?") +
  theme(title = element_text(size = 15),
        axis.text.x = element_text(size=8,
                                   face='italic',
                                   angle=70))





merge_sg3 = merge_sg2
merge_sg3$predictions_sg_t = NULL
names(merge_sg3) = c("yearweek", "value")
merge_sg3$key = "real"

merge_sg4 = merge_sg2
merge_sg4$Close_t = NULL
names(merge_sg4) = c("yearweek", "value")
merge_sg4$key = "pred"

merge_sg5 = rbind(merge_sg3, merge_sg4)
merge_sg5 %>% as.data.frame()

g = ggplot(merge_sg5, aes(x = yearweek, y = value, group = key)) +
  geom_line(aes(color = key, linetype = key)) +
  geom_point(size = 1.8, aes(color = key, pch = key))+
  scale_color_manual(values = c("blue", "red")) +
  theme(title = element_text(size = 15),
        axis.text.x = element_text(size=8,
                                   face='italic',
                                   angle=90))  %>% print()

print(g)
getwd()
#setwd("./visualization/인기상품_모델링/")
#ggsave(paste0(current_sell[1],".jpg"))


merge_sg2
merge_sg2 %>% tail()

RMSE(merge_sg2$predictions_sg_t , merge_sg2$Close_t)
MAPE(merge_sg2$predictions_sg_t , merge_sg2$Close_t)

