d=fread_mysql(tbname = "schede_mensili")
s=fread_mysql(tbname = "strati")
s=s[anno==2014]

setkey(d, id_strato)
setkey(s, id_strato)

SDcols=c("costo_carburante", "giorni_mare","codlft199","regione")

d=s[d,SDcols,with=T]


library(ggplot2)

# d[giorni_mare>0, quantile(costo_carburante/giorni_mare,.75)+ 1.5*IQR(costo_carburante/giorni_mare), by="codlft199,regione" ]

d[giorni_mare>0 ,ggplot(.SD, aes(x= codlft199,y=costo_carburante/giorni_mare)) +geom_boxplot(aes(fill=regione),outlier.size=3 ,outlier.colour="red"), .SDcols=c("costo_carburante", "giorni_mare","codlft199","regione")]

d[giorni_mare>0 ,ggplot(.SD, aes(x= codlft199,y=costo_carburante/giorni_mare)) +geom_boxplot(aes(fill=regione),outlier.size=3 ,outlier.colour="red") + geom_text(aes(label=ifelse(costo_carburante/giorni_mare>4*IQR(costo_carburante/giorni_mare),1,""))), .SDcols=c("costo_carburante", "giorni_mare","codlft199","regione")]

#####################################################
# pie chart
temp=d[,list(value=as.numeric(sum(value)) ),  keyby=.(codsis199,var)]
temp2=temp[,list(tot=sum(value) ), keyby=.(codsis199)]
temp[temp2, value:=ifelse(tot==0,0,value/tot)]

temp[,pie_label_position:=cumsum(value), by=codsis199]
temp[,pie_label_position:=pie_label_position-.5*value]

temp[, ggplot(.SD, aes(x="",y=value,fill=var)) + geom_bar(stat="identity") + coord_polar(theta="y") + facet_wrap(~eval(parse(text="codsis199"))) + geom_text(aes(label = paste0(round(100*value,0), "%"), y=pie_label_position) ) ]


