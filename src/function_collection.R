
library(dplyr)
library(ggplot2)
library(data.table)
library(patchwork)
library(tidyverse)
library(plyr)


#install.packages("conflicted")
library(conflicted)
conflict_prefer("summarise", "dplyr")
conflict_prefer("filter", "dplyr")

# plot - 주문횟수 대비 판매량 함수 생성

func_sale = function(x){
  a = x %>% group_by(yearmonth) %>% summarise(pp = sum(qty))
  b = x %>% select(yearmonth) %>% table() %>% as.matrix() %>% as.data.frame()
  k = as.data.frame(a$pp / b$V1)
  colnames(k)[1] = "count_per_sale"
  k$date = a$yearmonth
  
  return(k)
}



# plot - eda 함수 생성

func_plot = function(x) {
  p1 = x %>% group_by(yearmonth) %>% summarise(qty_sum = sum(qty)) %>% 
    ggplot(aes(x = yearmonth, y = qty_sum, group = 1)) + geom_line(color = "blue", size = 1.5) +
    labs(x = "년월", y = "총판매량") + ggtitle("년월별 총 판매량") +
    theme(title = element_text(size = 15),
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=90))
  
  p2 = func_sale(x) %>% ggplot(aes(x = date, y = count_per_sale, group = 1)) + 
    geom_line(color = "skyblue", size = 1.5) + 
    labs(x = "년월", y = "판매량") + ggtitle("년월별 주문건수 대비 판매량") +
    theme(title = element_text(size = 15),
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=90))
  
  
  p3 = x %>% group_by(month) %>% summarise(qty_sum = sum(qty)) %>% 
    ggplot(aes(x = month, y = qty_sum, group = 1)) + geom_line(color = "pink", size = 1.5) +
    labs(x = "월", y = "총판매량") + ggtitle("월별 판매량") +
    theme(title = element_text(size = 15),
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=45))
  
  p4 = x %>% group_by(weekday) %>% summarise(qty_sum = sum(qty)) %>% 
    ggplot(aes(x = weekday, y = qty_sum, fill = weekday)) + geom_bar(stat = "identity") +
    labs(x = "요일", y = "총판매량") + ggtitle("요일별 판매량") +
    theme(title = element_text(size = 15), 
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=45))
  
  
  p5 = x %>% group_by(season) %>% summarise(qty_sum = sum(qty)) %>% 
    ggplot(aes(x = season, y = qty_sum, fill = season)) + geom_bar(stat = "identity" ) +
    scale_fill_manual(values = c("#FFCC00", "#FF9900", 
                                 "#FF6600", "#FF3300")) +
    labs(x = "계절", y = "총판매량") + ggtitle("계절별 판매량") +
    theme(title = element_text(size = 15),
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=45))
  
  print(dim(x))
  return((p1/p2)/(p3+p4+p5))
}


## 감성분석 함수

sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '', sentence)        # 숫자 제거
    
    word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}




## 탐색적 자료분석 함수

eda_func = function(x) {
  
  # 업종별 빈도
  p1 = x$custclass %>% table() %>% as.data.frame() %>% 
    ggplot(aes(x = ., y = Freq, fill = .)) + geom_bar(stat = "identity") +
    labs(x = "업종", y = "빈도") + ggtitle("업종별 빈도") +
    theme(title = element_text(size = 15), 
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=45))
  
  #ggsave("./visualization/세계식품업종별빈도.jpg")
  
  # 계절별 판매량
  p2 = x %>% group_by(season) %>% summarise(mean_qty = mean(qty)) %>% 
    ggplot(aes(x = season, y = mean_qty, fill = season)) + geom_bar(stat = "identity" ) +
    scale_fill_manual(values = c("#FFCC00", "#FF9900", 
                                 "#FF6600", "#FF3300")) +
    labs(x = "계절", y = "평균판매량") + ggtitle("계절별 판매량") +
    theme(title = element_text(size = 15),
          axis.text.x = element_text(size=13,
                                     face='italic',
                                     angle=45))
  
  # 아이템별 판매량
  p3 = x %>% group_by(item) %>% summarise(mean_qty = mean(qty)) %>% 
    ggplot(aes(x = item, y = mean_qty, fill = item)) + geom_bar(stat = "identity") +
    labs(x = "아이템", y = "평균판매량") + ggtitle("아이템별 판매량") +
    theme(title = element_text(size = 15), 
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=45))
  
  
  # 공휴일 유무에 따른 판매량
  p4 = x %>% filter(invoicedate <= "2018-12-30") %>% 
    group_by(isholiday) %>% summarise(mean_qty = mean(qty)) %>% 
    ggplot(aes(x = isholiday, y = mean_qty)) + geom_bar(stat = "identity", fill = "skyblue") + 
    labs(x = "공휴일유무", y = "평균판매량") + ggtitle("공휴일 유무에 따른 판매량") +
    theme(title = element_text(size = 15))
  
  
  # 탐색적 자료분석 결과.
  return(p1 / p3 /  (p2 + p4))
}

