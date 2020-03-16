
## 라이브러리 로드


library(dplyr)
library(ggplot2)
library(data.table)
library(patchwork)
library(readxl)
library(lubridate)


## 데이터 로드 - wfood

getwd()
wfood = data.frame()
for(i in 1:11) {
  a = read_excel("./ankus-lite-wfood_110718/wfood_salsdb.xls", i)
  a = as.data.frame(a)
  wfood = rbind(wfood, a)
  print(i)
}
b = read_excel("./ankus-lite-wfood_110718/wfood_salsdb(1).xls", 1)
b = as.data.frame(b)
wfood = rbind(wfood, b)

for(i in 1:3) {
  a = read_excel("./ankus-lite-wfood_110718/wfood_salsdb(2).xls", i)
  a = as.data.frame(a)
  wfood = rbind(wfood, a)
  print(i)
}


wfood = as.data.table(wfood)
wfood$invoicedate = as.Date(wfood$invoicedate, '%Y%m%d') 
wfood = wfood %>% arrange(invoicedate) %>% as.data.table() # 날짜별 정렬



## 데이터 로드 - pub_hol

pub_hol = read_excel("./ankus-lite-wfood_113211/pubholiday.xls", 1)
pub_hol = as.data.table(pub_hol)

pub_hol$locdate = as.Date(pub_hol$locdate, "%Y%m%d")
names(pub_hol)[1] = "invoicedate"


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



wfood
