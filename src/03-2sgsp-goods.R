
## 상품 상세 데이터 로드
gds_dtl = read_excel("./ankus-lite-wfood_113211/shopng_goods_dtls_new.xls", 1)
gds_dtl =as.data.frame(gds_dtl)

gds_rvw = data.frame()
for(i in 1:5){
  a = read_excel("./ankus-lite-wfood_113211/shopng_goods_review.xls", i)
  a = as.data.frame(a)
  gds_rvw = rbind(gds_rvw, a)
  print(i)
}


gds = left_join(gds_rvw, gds_dtl, by = c("goods_no", "goods_no"))
gds = as.data.table(gds)
gds %>% head

gds$item %>% table()



gds$goods_url =NULL
gds$writer = NULL
gds$review_no = NULL
gds$purch_de = NULL
#gds$last_updt_dt.x %>% unique()
gds$last_updt_dt.x = NULL
#gds$last_updt_dt.y %>% unique()
gds$last_updt_dt.y = NULL
#gds$delivery %>% table()
gds$goods_option = NULL
gds$goods_sttus =NULL
gds$orgnp = NULL
gds$brand = NULL
#gds$prdnm_modlnm %>% unique()
gds$prdnm_modlnm = NULL
#gds$cttpc %>% unique()
gds$cttpc =NULL
gds$cret_dt.y = NULL
gds$shopng_knd = NULL
gds$seler_nm = NULL
gds$mng_no.y = NULL


colSums(is.na(gds))



## 감성분석

positive <- readLines("./original-data/positive.txt", encoding = "UTF-8")
negative <- readLines("./original-data/negative.txt", encoding = "UTF-8")


result=sentimental(gds$goods_review, positive, negative) # 시간이 좀 걸리네..

gds$score =  result$score





################################################################################################
################################################################################################
################################################################################################

## 데이터 로드

#fwrite(gds, "./preprocessing-data/gds.csv")

gds = fread("./preprocessing-data/gds.csv", encoding = "UTF-8")

gds = gds[is.na(gds$recommand) == FALSE,]


gds$remark[gds$score >=1] = "긍정"
gds$remark[gds$score ==0] = "중립"
gds$remark[gds$score < 0] = "부정"



gds$invoicedate = substr(gds$cret_dt.x, 1, 10)
gds$invoicedate = as.Date(gds$invoicedate, "%Y-%m-%d")
gds = gds %>% arrange(invoicedate)
gds$yearmonth = substr(gds$invoicedate, 1, 7)
gds$month = substr(gds$invoicedate, 6, 7)

day_levels <- c("일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일")
gds$weekday = factor(weekdays((gds$invoicedate)), levels=day_levels, ordered=TRUE)

gds$recommand %>% table()
colSums(is.na(gds))

gds = gds[is.na(gds$recommand) == FALSE,]
gds$recommand[gds$recommand == "추천안함"] = 1
gds$recommand[gds$recommand == "보통"] = 3
gds$recommand[gds$recommand == "추천"] = 4
gds$recommand[gds$recommand == "적극추천"] = 5

gds$recommand = as.integer(gds$recommand)

gds %>% str()
gds = gds[is.na(gds$recommand) == FALSE,]


gds %>% as.data.table()

colSums(is.na(gds))

a = as.data.frame(gds) %>% group_by(yearmonth) %>%
  summarise(mean_score = mean(score), mean_recommand = mean(recommand)) %>%
  as.data.frame()
a



a$mean_score = scale(a$mean_score)
a$mean_recommand = scale(a$mean_recommand)

cor(a[,-1])
p = a %>% 
  ggplot(aes(x = yearmonth, y = mean_score, group = 1)) + geom_line(color = "blue")
p + geom_line(aes(x = yearmonth, y = mean_recommand), color = "red") 


gds_nut = gds %>% filter(item == "너트류")

gds_nut %>% as.data.table()
gds_nut %>% arrange(cret_dt.x) %>% as.data.table()


a = gds_nut %>% group_by(yearmonth) %>%
  summarise(mean_score = mean(score), mean_recommand = mean(recommand)) %>%
  as.data.frame()
a$mean_score = scale(a$mean_score)
a$mean_recommand = scale(a$mean_recommand)

cor(a[,-1])
p = a %>% 
  ggplot(aes(x = yearmonth, y = mean_score, group = 1)) + geom_line(color = "blue")
p + geom_line(aes(x = yearmonth, y = mean_recommand), color = "red") + 
  theme(title = element_text(size = 15),
        axis.text.x = element_text(size=8,
                                   face='italic',
                                   angle=90))
  


398 + 230 + 33 + 79 + 126 + 166 + 132 + 59 + 34

