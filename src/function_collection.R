
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







func_plot_all = function(x) {
  p1 = x %>% group_by(yearmonth,item) %>% summarise(qty_sum = sum(qty)) %>% 
    ggplot(aes(x = yearmonth, y = qty_sum, group = item, colour = item)) + geom_line(size = 1.5) +
    labs(x = "년월", y = "총판매량") + ggtitle("년월별 총 판매량") +
    theme(title = element_text(size = 15),
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=90))
  
  #p2 = func_sale(x) %>% ggplot(aes(x = date, y = count_per_sale, group = 1)) + 
  #  geom_line(color = "skyblue", size = 1.5) + 
  #  labs(x = "년월", y = "판매량") + ggtitle("년월별 주문건수 대비 판매량") +
  #  theme(title = element_text(size = 15),
  #        axis.text.x = element_text(size=8,
  #                                   face='italic',
  #                                   angle=90))
  
  
  p3 = x %>% group_by(month, item) %>% summarise(qty_sum = sum(qty)) %>% 
    ggplot(aes(x = month, y = qty_sum, group = item, colour = item)) + geom_line(size = 1.5) +
    labs(x = "월", y = "총판매량") + ggtitle("월별 판매량") +
    theme(title = element_text(size = 15),
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=45))
  
  p4 = x %>% group_by(weekday,item) %>% summarise(qty_sum = sum(qty)) %>% 
    ggplot(aes(x = weekday, y = qty_sum, fill = item)) + geom_bar(stat = "identity", width = 0.3, position = "dodge") +
    labs(x = "요일", y = "총판매량") + ggtitle("요일별 판매량") +
    theme(title = element_text(size = 15), 
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=45))
  
  
  p5 = x %>% group_by(season, item) %>% summarise(qty_sum = sum(qty)) %>% 
    ggplot(aes(x = season, y = qty_sum, fill = item)) + geom_bar(stat = "identity", width = 0.3, position = "dodge" ) +
    scale_fill_manual(values = c("#FFCC00", "#FF9900", 
                                 "#FF6600", "#FF3300")) +
    labs(x = "계절", y = "총판매량") + ggtitle("계절별 판매량") +
    theme(title = element_text(size = 15),
          axis.text.x = element_text(size=8,
                                     face='italic',
                                     angle=45))
  
  print(dim(x))
  return((p1/p3)/(p4+p5))
}




## 제품별 애니메이션

anim_plot = function(company, date_select) {
  theme_set(theme_classic())
  
  company_g = company 
  company_g = company_g %>% group_by(itemname, yearmonth) %>% summarise(qty_sum = sum(qty))
  company_g$int_date = gsub("-","",company_g$yearmonth)
  company_g$int_date = as.integer(company_g$int_date)
  company_g = as.data.frame(company_g)
  
  v1=seq(201501,201512,1)
  v2=seq(201601,201612,1)
  v3=seq(201701,201712,1)
  v4=seq(201801,201812,1)
  v5=seq(201901,201912,1)
  v6=seq(202001,202003,1)
  
  
  
  if(date_select == 2017) {
    all_dt = c(v3,v4,v5,v6)
  }
  else if(date_select == 2015){
    all_dt = c(v1,v2,v3,v4,v5,v6)
  }
  all_dt = as.data.frame(all_dt)
  names(all_dt) = "int_date"
  
  
  
  item_uniq = company_g$itemname %>% unique()
  
  full_date_df = data.frame()
  
  for(i in 1:length(item_uniq)) {
    si = company_g %>% filter(itemname == item_uniq[i])
    mer = left_join(all_dt, si)
    mer$itemname = unique(si$itemname)
    mer$qty_sum[is.na(mer$qty_sum) == T] = 0
    full_date_df = rbind(full_date_df, mer)
    
  }
  
  cum_company = data.frame()
  for(i in 1:(length(item_uniq))) {
    temp = full_date_df %>% filter(itemname == item_uniq[i])
    temp = temp %>% arrange(int_date)
    temp$qum_sum = cumsum(temp$qty_sum)
    
    cum_company = rbind(cum_company, temp)
    print(i)
  }
  
  cum_company = cum_company %>% group_by(int_date) %>% mutate(rank = min_rank(-qum_sum)*1,
                                                              Value_rel = qum_sum/qum_sum[rank==1],
                                                              Value_lbl = paste0(" ",qum_sum)) %>% 
    filter(rank <= 20) %>% 
    ungroup()
  
  
  p <- ggplot(cum_company, aes(rank, group = itemname, 
                               fill = as.factor(itemname), color = as.factor(itemname))) +
    geom_tile(aes(y = qum_sum/2,
                  height = qum_sum,
                  width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(itemname, " ")), vjust = 0.2, hjust = 1) +
    geom_text(aes(y=qum_sum,label = Value_lbl, hjust=0)) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse() +
    guides(color = FALSE, fill = FALSE) +
    
    labs(title='{closest_state}', x = "", y = "발주량",
         caption = "Sources: World food | Plot generated Hyeon Jong") +
    theme(plot.title = element_text(hjust = 0, size = 22),
          axis.ticks.y = element_blank(),  # These relate to the axes post-flip
          axis.text.y  = element_blank(),  # These relate to the axes post-flip
          plot.margin = margin(1,3,1,6, "cm")) +
    
    transition_states(int_date, transition_length = 4, state_length = 1) +
    ease_aes('cubic-in-out')
  
  return(p)
}




