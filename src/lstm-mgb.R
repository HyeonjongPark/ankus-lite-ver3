mgb_plus %>% select(item) %>% table
mgb_plus %>% select(custclass) %>% table %>% sort


it = "머거본"
cu = "대리점"


goods2020 = mgb_plus %>% filter(item == it, custclass == cu, invoicedate >= "2020-01-01") %>% select(itemname) %>% unique()
goods2020 = goods2020$itemname
goods2020 %>% length

goods2019 = mgb_plus %>% filter(item == it, custclass == cu, invoicedate >= "2019-01-01" & invoicedate <= "2019-03-01") %>% select(itemname) %>% unique()
goods2019 = goods2019$itemname
goods2019 %>% length

goods2018 = mgb_plus %>% filter(item == it, custclass == cu, invoicedate >= "2018-01-01" & invoicedate <= "2018-03-01") %>% select(itemname) %>% unique()
goods2018 = goods2018$itemname
goods2018 %>% length

goods2017 = mgb_plus %>% filter(item == it, custclass == cu, invoicedate >= "2017-01-01" & invoicedate <= "2017-03-01") %>% select(itemname) %>% unique()
goods2017 = goods2017$itemname
goods2017 %>% length

goods2016 = mgb_plus %>% filter(item == it, custclass == cu, invoicedate >= "2016-01-01" & invoicedate <= "2016-03-01") %>% select(itemname) %>% unique()
goods2016 = goods2016$itemname
goods2016 %>% length

goods2015 = mgb_plus %>% filter(item == it, custclass == cu, invoicedate >= "2015-01-01" & invoicedate <= "2015-03-01") %>% select(itemname) %>% unique()
goods2015 = goods2015$itemname
goods2015 %>% length

current_sell = goods2020[goods2020 %in% goods2017] 
current_sell = current_sell[current_sell %in% goods2018] 
current_sell = current_sell[current_sell %in% goods2019] 
current_sell = current_sell[current_sell %in% goods2016] 
current_sell = current_sell[current_sell %in% goods2015] 

current_sell %>% length # 15 ~20 년 까지 꾸준히 판매되고 있는 제품들


current_sell = goods2020





mape_vec = data.frame(name = current_sell, mape = NA)
leng_vec = data.frame(name = current_sell, len = NA)

setwd(paste0("E:/data-analysis/ankus/ankus-lite-ver2/visualization/인기상품_모델링/머거본/대리점/all/"))
getwd()

for(hot_item in 1:(length(current_sell))) {
  
  sgsp270 = mgb_plus %>% filter(item == it, custclass == cu, itemname == (current_sell[hot_item])) %>% 
    group_by(invoicedate) %>% summarise(sum_qty = sum(qty))
  
  names(sgsp270) = c("Date", "Close")
  Series_sg<-sgsp270[,2]$Close
  diffed_sg = diff(Series_sg, differences = 1)
  
  lag_transform <- function(x, k= 1){
    
    lagged =  c(rep(NA, k), x[1:(length(x)-k)])
    DF = as.data.frame(cbind(lagged, x))
    colnames(DF) <- c( paste0('x-', k), 'x')
    DF[is.na(DF)] <- 0
    return(DF)
  }
  
  supervised_sg = lag_transform(diffed_sg, 1)
  
  N_sg = nrow(supervised_sg)
  n_sg = round(N_sg *0.85, digits = 0)
  train_sg = supervised_sg[1:n_sg, ]
  test_sg  = supervised_sg[(n_sg+1):N_sg,  ]
  
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
  
  Scaled_sg = scale_data(train_sg, test_sg, c(-1, 1))
  
  y_train_sg = Scaled_sg$scaled_train[, 2]
  x_train_sg = Scaled_sg$scaled_train[, 1]
  y_test_sg = Scaled_sg$scaled_test[, 2]
  x_test_sg = Scaled_sg$scaled_test[, 1]
  
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
  
  dim(x_train_sg) <- c(length(x_train_sg), 1, 1)
  X_shape2_sg = dim(x_train_sg)[2]
  X_shape3_sg = dim(x_train_sg)[3]
  
  model_sg <- keras_model_sequential() 
  model_sg%>%
    layer_lstm(units, batch_input_shape = c(batch_size, X_shape2_sg, X_shape3_sg), stateful= TRUE)%>%
    layer_dense(units = 1)
  
  
  model_sg %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
    metrics = c('accuracy')
  )
  
  Epochs = 5
  
  for(i in 1:Epochs ){
    model_sg %>% fit(x_train_sg, y_train_sg, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
    model_sg %>% reset_states()
  }
  
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
  
  #RMSE(predictions_sg,Series_sg[(len2-len1+1):len2])
  #MAPE(predictions_sg,Series_sg[(len2-len1+1):len2])
  
  
  data_sg<-sgsp270[(len2-len1+1):len2,]
  
  ### 앞에 트레인 부분 추가해서 그래프 그려보기
  #data_sg<-sgsp270[1:len2,]
  
  
  data_sg$Date <- as.Date(data_sg$Date)
  merge_sg<-cbind(data_sg,predictions_sg)
  names(merge_sg) = c("Date", "Close", "predictions_sg")
  
  diff = mean(merge_sg$Close) - mean(merge_sg$predictions_sg)
  
  merge_sg$predictions_sg = merge_sg$predictions_sg + diff
  
  #RMSE(merge_sg$predictions_sg , merge_sg$Close)
  #MAPE(merge_sg$predictions_sg , merge_sg$Close)
  
  #ggplot(data = merge_sg, aes(x = Date, y = Close))+ 
  #  geom_line(col='red')+
  #  geom_line(aes(y=predictions_sg),col='blue')+ ylab("일 별 발주량")+ xlab("일별")+ggtitle("제품명")
  
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
  
  ## 월별
  #merge_sg2 = merge_sg %>% arrange(year, month)
  
  
  
  merge_sg2$yearweek = factor(merge_sg2$yearweek, levels = unique(merge_sg2$yearweek))
  
  ## 주별
  
  merge_sg2 = merge_sg %>% group_by(factor(yearweek, levels = unique(yearweek))) %>% summarise(Close_t = sum(Close), predictions_sg_t = sum(predictions_sg))
  
  ## 월별
  #merge_sg2 = merge_sg %>% group_by(factor(yearmonth, levels = unique(yearmonth))) %>% summarise(Close_t = sum(Close), predictions_sg_t = sum(predictions_sg))
  
  
  #diff = mean(merge_sg2$Close_t) - mean(merge_sg2$predictions_sg_t)
  
  names(merge_sg2) = c("yearweek", "Close_t", "predictions_sg_t")
  
  merge_sg3 = merge_sg2
  merge_sg3$predictions_sg_t = NULL
  names(merge_sg3) = c("yearweek", "value")
  merge_sg3$key = "real"
  
  merge_sg4 = merge_sg2
  merge_sg4$Close_t = NULL
  names(merge_sg4) = c("yearweek", "value")
  merge_sg4$key = "pred"
  
  merge_sg5 = rbind(merge_sg3, merge_sg4)
  
  
  g = ggplot(merge_sg5, aes(x = yearweek, y = value, group = key)) +
    geom_line(aes(color = key, linetype = key)) +
    geom_point(size = 1.8, aes(color = key, pch = key))+
    scale_color_manual(values = c("blue", "red")) +
    theme(title = element_text(size = 15),
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=90)) + ggtitle(current_sell[hot_item])
  print(g)
  
  ggsave(paste0(current_sell[hot_item],".jpg"))
  
  #RMSE(merge_sg2$predictions_sg_t , merge_sg2$Close_t)
  mape_cal = MAPE(merge_sg2$predictions_sg_t , merge_sg2$Close_t)
  mape_vec$mape[hot_item] = mape_cal
  
  leng_vec$len[hot_item] = nrow(sgsp270)
  
}

mape_vec
write.csv(mape_vec, "./mape_info.csv")
write.csv(leng_vec, "./leng_info.csv")
