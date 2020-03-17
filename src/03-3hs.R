
hs = wfood %>% filter(companyname == "(주)홍선")

colSums(is.na(hs))


# custclass가 결측인 경우가 너무 많아 제거 불가
#hs = hs %>% filter(is.na(custclass) == FALSE) 


hs$yearmonth = substr(hs$invoicedate, 1, 7)
hs$month = substr(hs$invoicedate, 6, 7)

day_levels <- c("일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일")
hs$weekday = factor(weekdays((hs$invoicedate)), levels=day_levels, ordered=TRUE)


#fwrite(hs, "./preprocessing-data/hs.csv")


## 데이터 로드

hs = fread("./preprocessing-data/hs.csv", encoding = "UTF-8")
hs$weekday = NULL


hs$invoicedate = as.Date(hs$invoicedate, "%Y-%m-%d")

day_levels <- c("일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일")
hs$weekday = factor(weekdays((hs$invoicedate)), levels=day_levels, ordered=TRUE)

hs %>% as.data.table


## 발주 +
hs_plus = hs %>% filter(qty > 0)

## 반품 -
hs_minus = hs %>% filter(qty < 0)

hs_plus$item %>% unique()
hs_plus$custclass %>% unique()
hs_plus$custclass %>% table()

hs_plus$item %>% unique()
hs_plus$custclass %>% unique()

func_plot(hs_plus %>% filter(custclass != "홍콩")) # 78



# 업종별 빈도
hs$custclass %>% table() %>% as.data.frame() %>% 
  ggplot(aes(x = ., y = Freq)) + geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "custclass", y = "빈도") + ggtitle("업종별 빈도") +
  theme(title = element_text(size = 15))



# 계절별 판매량
hs %>% group_by(season) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = season, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "계절", y = "평균판매량") + ggtitle("계절별 판매량") +
  theme(title = element_text(size = 15))


# 아이템별 판매량
hs %>% group_by(item) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = item, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "아이템", y = "평균판매량") + ggtitle("아이템별 판매량") +
  theme(title = element_text(size = 15))


# 공휴일 유무에 따른 판매량
hs %>% filter(invoicedate <= "2018-12-30") %>% 
  group_by(isholiday) %>% summarise(mean_qty = mean(qty)) %>% 
  ggplot(aes(x = isholiday, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
  labs(x = "공휴일유무", y = "평균판매량") + ggtitle("공휴일 유무에 따른 판매량") +
  theme(title = element_text(size = 15))



