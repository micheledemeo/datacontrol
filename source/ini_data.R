wd=getwd()

# elenco battelli per controllo consegne ####
bat=fread_mysql(tbname = 'battelli_rilevatori')
bat=bat[ anno==2014, list(id_battello ,id_rilevatore)]
setkey(bat, id_battello)

# tabella degli id: battello,strato,rilevatore. 
ids=fread_mysql(tbname = 'battelli')
ids=ids[,.(id_battello=id,id_strato)]
setkey(ids, id_battello)
ids=ids[bat]
ids[is.na(id_strato) , id_strato:=0L]

# dataset mensili #####
d_mensili=fread_mysql(tbname = 'schede_mensili')
d_mensili=d_mensili[anno==2014]
d_mensili[,id_strato:=NULL]
setkey(d_mensili, id_battello)
d_mensili=ids[d_mensili]
d_mensili[is.na(id_strato) , id_strato:=0L]

consegne_mensili=d_mensili[,list(m=.N),.(id_battello,mensilita)][mensilita==12,m:=12][,mensilita:=NULL]
setkey(consegne_mensili, id_battello)
bat=consegne_mensili[bat][is.na(m),m:=0]
rm(consegne_mensili)

id.vars=c('id_battello','id_rilevatore','id_strato')
vars=fread(pastedir(wd,"source/vars_schede_mensili"),header = F)
vars=c(id.vars,vars$V1)

# crea control_var_mensili table con variabili che non vanno usate in melt, ma aggregate a parte e messe in join
control_var_mensili=d_mensili[,list(giorni_mare=sum2(giorni_mare),equipaggio_medio=ifelse(sum2(giorni_mare)==0,mean(equipaggio_medio),sum2(equipaggio_medio*giorni_mare)/sum2(giorni_mare)) ,volume_carburante=sum2(volume_carburante+volume_lubrificante) ),keyby=.(id_battello)]

d_mensili=d_mensili[,vars,with=F]
options(warn=-1)
d_mensili=melt(d_mensili,id.vars = 1:3, measure.vars = (4:ncol(d_mensili)), variable.factor = F)
rm(vars)
options(warn=0)

d_mensili=d_mensili[,list(value=sum2(value)), keyby=.(id_battello,id_rilevatore,id_strato,variable)]
d_mensili[is.na(value), value:=0]

# dataset annuali #####
d_annuali=fread_mysql(tbname = 'schede_annuali')
d_annuali=d_annuali[anno==2014]
d_annuali[,id_strato:=NULL]
setkey(d_annuali, id_battello)
d_annuali=ids[d_annuali]
d_annuali[is.na(id_strato) , id_strato:=0L]

consegne_annuali=d_annuali[,list(a=1),keyby=id_battello]
bat=consegne_annuali[bat][is.na(a),a:=0]
rm(consegne_annuali)
bat=bat[,list(yearly_sent=sum2(a),yearly_tot=.N,monthly_sent=sum2(m),monthly_tot=12*.N),keyby=id_rilevatore]
perc_consegne_annuali=bat[,sum2(yearly_sent)/sum2(yearly_tot)]
perc_consegne_annuali = if (perc_consegne_annuali>=.99 & perc_consegne_annuali<1) 99 else round(100*perc_consegne_annuali)
perc_consegne_annuali=paste0("Yearly data sent: ", perc_consegne_annuali ,"%" )
perc_consegne_mensili=bat[,sum2(monthly_sent)/sum2(monthly_tot)]
perc_consegne_mensili = if (perc_consegne_mensili>=.99 & perc_consegne_mensili<1) 99 else round(100*perc_consegne_mensili)
perc_consegne_mensili=paste0("Monthly data sent: ", perc_consegne_mensili ,"%" )


# crea control_var_mensili table con variabili che non vanno usate in melt, ma aggregate a parte e messe in join
control_var_annuali=d_annuali[,list(Valore_di_mercato_del_battello=mean(Valore_di_mercato_del_battello),Numero_di_proprietari_del_battello=mean(Numero_di_proprietari_del_battello)),keyby=.(id_battello)]

vars=fread(pastedir(wd,"source/vars_schede_annuali"),header = F)
vars=c(id.vars,vars$V1)

d_annuali=d_annuali[,vars,with=F]
options(warn=-1)
d_annuali=melt(d_annuali,id.vars = 1:3, measure.vars = (4:ncol(d_annuali)), variable.factor = F)
rm(vars)
options(warn=0)

d_annuali=d_annuali[,list(value=sum2(value)), keyby=.(id_battello,id_rilevatore,id_strato,variable)]
d_annuali[is.na(value), value:=0]

# union mensili e annuali
d=rbindlist(list(d_mensili,d_annuali))
rm(d_mensili,d_annuali)
setkey(d, variable)

# importa tab di aggregazioni voci di costo e aggrega voci ####
aggrega_var=fread(pastedir(wd,"source/aggrega_var"),select = c('variable','var'))
setkey(aggrega_var, variable)
d=aggrega_var[d]
rm(aggrega_var)
d[is.na(var),var:="unclassified"]
d=d[,list(value=sum2(value)), keyby=.(id_rilevatore,id_battello,id_strato,var)]

# join with control variables
setkey(control_var_mensili, id_battello)
setkey(control_var_annuali, id_battello)
setkey(d, id_battello)

d=control_var_mensili[d]
d[is.na(giorni_mare), (c('giorni_mare','equipaggio_medio','volume_carburante')):=list(0,0,0)]

d=control_var_annuali[d]
d[is.na(Valore_di_mercato_del_battello), (c('Valore_di_mercato_del_battello','Numero_di_proprietari_del_battello')):=list(0,0)]

# importa labels di strati e associa a d ####
s=fread_mysql(tbname = 'strati')
s=s[anno==2014][,c('anno','id'):=NULL]
setkey(d, id_strato)
setkey(s, id_strato)
d=s[d]
rm(s)

# d è pronto:
