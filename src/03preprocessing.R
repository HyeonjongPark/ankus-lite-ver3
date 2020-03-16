
## 조인(wfood + pub_hol = wfood)

wfood = full_join(wfood, pub_hol)
wfood$isholiday[is.na(wfood$isholiday)] = "N"

wfood = wfood %>% arrange(invoicedate) %>% as.data.table() # 날짜별 정렬


## season 파생변수 생성
wfood$season = NA 
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("06","07","08")]  = "summer"
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("09","10")]  = "fall"
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("11","12","01","02")]  = "winter"
wfood$season[substr(wfood$invoicedate, 6, 7) %in% c("03","04","05")]  = "spring"


# 2019년 공휴일 지정 
wfood[wfood$invoicedate == "2019-01-01",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-02-04",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-02-05",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-02-06",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-03-01",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-05-05",]$isholiday = "Y"
wfood[wfood$invoicedate == "2019-05-12",]$isholiday = "Y"




wfood$companyname %>% unique()
wfood$item %>% unique()





# 회사에 따라 세 분류의 데이터로 분리

mgb = wfood %>% filter(companyname == "(주)머거본") 
sgsp = wfood %>% filter(companyname == "세계식품(주)")
hs = wfood %>% filter(companyname == "(주)홍선")

wfood$itemseq %>% unique() %>% length() # 2170

mgb$itemseq %>% unique() %>% length() # 885
sgsp$itemseq %>% unique() %>% length() # 1847
hs$itemseq %>% unique() %>% length() # 88
  



wfood$itemname %>% unique() %>% length() # 2302

mgb$itemname %>% unique() %>% length() # 913
sgsp$itemname %>% unique() %>% length() # 1904
hs$itemname %>% unique() %>% length() # 119


wfood_seq = wfood %>%  group_by(itemseq, itemname) %>% count() %>% as.data.frame()
wfood %>% filter(itemseq == "698")
wfood_seq %>% group_by(itemseq) %>% count() %>% arrange(desc(n))


conflict_prefer("count", "dplyr")
wfood %>% filter(custclass == "수출" & qty > 0) %>% select(qty) %>% sort(qty) %>% tail()
wfood %>% filter(custclass == "오비맥주" & qty > 0) 


