rm(list=ls())

# cd c:\selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.9.1.jar -port 4449


library(rvest)
library(stringr)
library(RSelenium)
library(rJava)
library(XML)
library(lubridate)
library(dplyr)

ch=wdman::chrome(port=4449L) #크롬드라이버를 포트 4449번에 배정
remDr=remoteDriver(remoteServerAddr = "localhost", port=4449L, browserName='chrome') #remort설정
remDr$open() #크롬 Open
remDr$navigate("https://finance.naver.com/item/sise_day.nhn?code=005930") # 삼성 주식 정보 홈페이지 이동

samsung = data.frame()
for(i in 1:599) {
  remDr$navigate(paste0("https://finance.naver.com/item/sise_day.nhn?code=005930&page=",i)) # 페이지 이동
  frontPage = remDr$getPageSource() # 스크래핑
  
  date = read_html(frontPage[[1]]) %>% html_nodes(paste0('.tah.p10.gray03')) %>% html_text() # 날짜에 해당하는 태그 읽기
  date = gsub("[.]","-",date) # 전처리
  date = ymd(date) # 날짜 데이터 타입으로 변경
  
  stock = matrix(read_html(frontPage[[1]]) %>% html_nodes(paste0('.tah.p11')) %>% html_text(), ncol = 6 , byrow = T)
  stock = stock[,c(3,4,5,1,6)] # 필요 컬럼 추출 (시가, 고가, 저가, 종가, 거래량)
  colnames(stock) = c("Open", "High", "Low", "Close", "Volume") # 컬럼명 설정
  stock = gsub(",","", stock) # 전처리
  stock = as.data.frame(stock)
  
  # factor형 -> integer형
  stock$Open = as.integer(as.character(stock$Open))
  stock$High = as.integer(as.character(stock$High))
  stock$Low = as.integer(as.character(stock$Low))
  stock$Close = as.integer(as.character(stock$Close))
  stock$Volume = as.integer(as.character(stock$Volume))
  
  df = cbind(date,stock) 
  samsung = rbind(samsung, df) # 페이지 별 데이터 결합
  Sys.sleep(1) 
}

remDr$close() #크롬 Close

colnames(samsung)[1] = "Date"
samsung$OpenInt = 0
samsung = samsung %>% arrange(Date) # 날짜별 정렬
write.csv(samsung, "E:/data-analysis/ankus/ankus-lite-ver2/original-data/input/samsung.csv")




