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

