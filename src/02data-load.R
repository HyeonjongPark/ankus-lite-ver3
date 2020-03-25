
## 라이브러리 로드



library(dplyr)
library(ggplot2)
library(data.table)
library(patchwork)
library(readxl)
library(lubridate)


## 데이터 로드 - wfood

getwd()
setwd("E:/data-analysis/ankus/ankus-lite-ver2")

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



## 데이터 로드 - pub_hol

pub_hol = read_excel("./ankus-lite-wfood_113211/pubholiday.xls", 1)
pub_hol = as.data.table(pub_hol)





## 데이터 로드 - weather

a = fread("./ankus-lite-wfood_130632/weather_info1.csv")
b = fread("./ankus-lite-wfood_130632/weather_info2.csv")
wtr = rbind(a,b)

wtr %>% str
as.Date(wtr$mtime, "%Y-%m-%d %H:%M:%S")

wtr %>% select(mtime) %>% unique()

