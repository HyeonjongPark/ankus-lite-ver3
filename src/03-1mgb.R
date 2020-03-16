
mgb = wfood %>% filter(companyname == "(주)머거본") 

# custclass가 결측인 경우(375개) 제외
mgb = mgb %>% filter(is.na(custclass) == FALSE) 

colSums(is.na(mgb))

mgb$yearmonth = substr(mgb$invoicedate, 1, 7)
mgb$month = substr(mgb$invoicedate, 6, 7)

day_levels <- c("일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일")
mgb$weekday = factor(weekdays((mgb$invoicedate)), levels=day_levels, ordered=TRUE)

mgb %>% as.data.table()

#fwrite(mgb, "./preprocessing-data/mgb.csv")



### 데이터 로드


mgb = fread("./preprocessing-data/mgb.csv", encoding = "UTF-8")
mgb$weekday = NULL


mgb$invoicedate = as.Date(mgb$invoicedate, "%Y-%m-%d")

day_levels <- c("일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일")
mgb$weekday = factor(weekdays((mgb$invoicedate)), levels=day_levels, ordered=TRUE)

mgb %>% as.data.table


## 발주 +
mgb_plus = mgb %>% filter(qty > 0)

## 반품 -
mgb_minus = mgb %>% filter(qty < 0)


func_plot(mgb_plus %>% filter(item == "머거본", custclass == "대리점")) # 78


# 업종별 빈도
mgb$custclass %>% table() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "custclass", y = "빈도") + ggtitle("업종별 빈도") +
  theme(title = element_text(size = 15))



# 계절별 판매량
mgb %>% group_by(season) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = season, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "계절", y = "평균판매량") + ggtitle("계절별 판매량") +
  theme(title = element_text(size = 15))


# 아이템별 판매량
mgb %>% group_by(item) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = item, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "아이템", y = "평균판매량") + ggtitle("아이템별 판매량") +
  theme(title = element_text(size = 15))


# 공휴일 유무에 따른 판매량
mgb %>% filter(invoicedate <= "2018-12-30") %>% 
  group_by(isholiday) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = isholiday, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "공휴일유무", y = "평균판매량") + ggtitle("공휴일 유무에 따른 판매량") +
  theme(title = element_text(size = 15))


