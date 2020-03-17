
sgsp = wfood %>% filter(companyname == "세계식품(주)")

colSums(is.na(sgsp))

sgsp$itemname %>% unique()


# custclass가 결측인 경우(15개) 제외
sgsp = sgsp %>% filter(is.na(custclass) == FALSE) 


sgsp$yearmonth = substr(sgsp$invoicedate, 1, 7)
sgsp$month = substr(sgsp$invoicedate, 6, 7)

day_levels <- c("일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일")
sgsp$weekday = factor(weekdays((sgsp$invoicedate)), levels=day_levels, ordered=TRUE)











## 데이터 로드###############################################################################
#############################################################################################
#############################################################################################



rm(list = ls())
#fwrite(sgsp, "./preprocessing-data/sgsp.csv")
sgsp = fread("./preprocessing-data/sgsp.csv", encoding = "UTF-8")
sgsp$weekday = NULL

sgsp$invoicedate = as.Date(sgsp$invoicedate, "%Y-%m-%d")

day_levels <- c("일요일", "월요일", "화요일", "수요일", "목요일", "금요일", "토요일")
sgsp$weekday = factor(weekdays((sgsp$invoicedate)), levels=day_levels, ordered=TRUE)

sgsp %>% as.data.table


## 발주 +
sgsp_plus = sgsp %>% filter(qty > 0)

## 반품 -
sgsp_minus = sgsp %>% filter(qty < 0)





source("./src/function_collection.R", encoding = "utf-8")





eda_func(sgsp)
eda_func(sgsp_plus)
eda_func(sgsp_minus)



func_plot(sgsp_plus %>% filter(item == "땅콩류", custclass == "편의점")) # 600
func_plot(sgsp_plus %>% filter(item == "땅콩류", custclass == "할인점")) # 340
func_plot(sgsp_plus %>% filter(item == "땅콩류", custclass == "대리점")) # 280
func_plot(sgsp_plus %>% filter(item == "땅콩류", custclass == "온라인")) # 160

func_plot(sgsp_plus %>% filter(item == "너트류", custclass == "편의점")) # 217
func_plot(sgsp_plus %>% filter(item == "너트류", custclass == "할인점")) # 560
func_plot(sgsp_plus %>% filter(item == "너트류", custclass == "대리점")) # 169
func_plot(sgsp_plus %>% filter(item == "너트류", custclass == "온라인")) # 91

func_plot(sgsp_plus %>% filter(item == "아몬드류", custclass == "편의점")) # 725
func_plot(sgsp_plus %>% filter(item == "아몬드류", custclass == "할인점")) # 479
func_plot(sgsp_plus %>% filter(item == "아몬드류", custclass == "대리점")) # 117
func_plot(sgsp_plus %>% filter(item == "아몬드류", custclass == "온라인")) # 78

func_plot(sgsp_plus %>% filter(custclass == "대리점")) # 78

# 이 시기에 건포류는 판매하지 않음.
#func_plot(sgsp_plus %>% filter(item == "건포류", custclass == "편의점"))
#func_plot(sgsp_plus %>% filter(item == "건포류", custclass == "할인점"))
#func_plot(sgsp_plus %>% filter(item == "건포류", custclass == "대리점"))
#func_plot(sgsp_plus %>% filter(item == "건포류", custclass == "온라인"))

