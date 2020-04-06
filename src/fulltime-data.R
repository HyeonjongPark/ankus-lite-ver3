
current_sell

sgsp_plus %>% tail

full_date = seq(as.Date('20150101', '%Y%m%d'),
           as.Date('20200301', '%Y%m%d'), 1)

length(full_date)

date_df = data.frame(ind = 1:length(full_date), invoicedate = full_date)

str(date_df)


consome = sgsp_plus %>% filter(item == "아몬드류", custclass == "할인점", itemname == (current_sell[1]))

unique(consome$itemseq)[1]

consome2 = left_join(date_df, consome)

consome2$itemname = current_sell[1]
consome2$item = unique(consome$item)[1]
consome2$custclass = unique(consome$custclass)[1]

consome2[is.na(consome2$qty),]$qty = 0

consome2



