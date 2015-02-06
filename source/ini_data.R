wd=getwd()

# elenco bat_riltelli per controllo consegne ####
bat_ril=fread_mysql(tbname = 'battelli_rilevatori')
bat_ril=bat_ril[ anno==2014, list(id_battello ,id_rilevatore)]
setkey(bat_ril, id_battello)

# importa tab di aggregazioni voci di costo e genera all data.table ####
aggrega_var=fread(pastedir(wd,"source/aggrega_var"),select = c('variable','var'))
all=fread_mysql(tbname = 'battelli')
all=all[grepl("2014",data_riferimento), .(id_battello=id,id_strato)]
setkey(all,id_battello)
all=all[bat_ril,nomatch=0][,i:=0]
cj=aggrega_var[,.(i=0,var=unique(var))]
setkey(cj,i)
setkey(all,i)
all=all[cj, allow.cartesian=T][,i:=NULL]
rm(cj)

# importa labels per strati ####
s=fread_mysql(tbname = 'strati')
s=s[anno==2014][,c('anno','id'):=NULL]
setkey(all, id_strato)
setkey(s, id_strato)
all=s[all]
rm(s)
all[,gsa:=as.character(gsa)]
all[is.na(regione), (c('regione','codsis199', 'codlft199', 'gsa', 'descrizione')):=list("UNKNOWN")]

# importa dati mensili ####
d_mensili=fread_mysql(tbname = 'schede_mensili')
d_mensili=d_mensili[anno==2014]
# crea control_var_mensili table con variabili che non vanno usate in melt, ma aggregate a parte e messe in join
control_var_mensili=d_mensili[,list(giorni_mare=sum2(giorni_mare),equipaggio_medio=ifelse(sum2(giorni_mare)==0,mean(equipaggio_medio),sum2(equipaggio_medio*giorni_mare)/sum2(giorni_mare)) ,volume_carburante=sum2(volume_carburante+volume_lubrificante) ),keyby=.(id_battello)]
vars=fread(pastedir(wd,"source/vars_schede_mensili"),header = F)
vars=c("id_battello","mensilita",vars$V1)
d_mensili=d_mensili[,vars,with=F]

# prepara dati per calcolo consegne mensili ####
consegne_mensili=d_mensili[,list(m=.N),.(id_battello,mensilita)][mensilita==12,m:=12][,mensilita:=NULL]
setkey(consegne_mensili, id_battello,m)
#se vi sono (erroneamente) schede con mensilita uguale 1 e 12, lascio mensilita 12:
consegne_mensili=consegne_mensili[.(unique(id_battello)),mult="last"]
consegne_mensili=consegne_mensili[.(id_battello),mult="last"]
bat_ril=consegne_mensili[bat_ril][is.na(m),m:=0]
rm(consegne_mensili)
d_mensili[,mensilita:=NULL]

# esegui melt su mensili ####
options(warn=-1)
d_mensili=melt(d_mensili,id.vars = 1, measure.vars = (4:ncol(d_mensili)), variable.factor = F)
rm(vars)
options(warn=0)
d_mensili=d_mensili[,list(value=sum2(value)), keyby=.(id_battello,variable)]
d_mensili[is.na(value), value:=0]

# importa dati annuali ####
d_annuali=fread_mysql(tbname = 'schede_annuali')
d_annuali=d_annuali[anno==2014]
# crea control_var_mensili table con variabili che non vanno usate in melt, ma aggregate a parte e messe in join
control_var_annuali=d_annuali[,list(Valore_di_mercato_del_bat_riltello=mean(Valore_di_mercato_del_battello),Numero_di_proprietari_del_bat_riltello=mean(Valore_di_mercato_del_battello)),keyby=.(id_battello)]
vars=fread(pastedir(wd,"source/vars_schede_annuali"),header = F)
vars=c("id_battello",vars$V1)
d_annuali=d_annuali[,vars,with=F]

# calcola consegne annuali e mensili ####
consegne_annuali=d_annuali[,list(a=1),keyby=id_battello]
bat_ril=consegne_annuali[bat_ril][is.na(a),a:=0]
rm(consegne_annuali)
bat_ril=bat_ril[,list(yearly_sent=sum2(a),yearly_tot=.N,monthly_sent=sum2(m),monthly_tot=12*.N),keyby=id_rilevatore]
perc_consegne_annuali=bat_ril[,sum2(yearly_sent)/sum2(yearly_tot)]
perc_consegne_annuali = if (perc_consegne_annuali>=.99 & perc_consegne_annuali<1) 99 else round(100*perc_consegne_annuali)
perc_consegne_annuali=paste0("Yearly data sent: ", perc_consegne_annuali ,"%" )
perc_consegne_mensili=bat_ril[,sum2(monthly_sent)/sum2(monthly_tot)]
perc_consegne_mensili = if (perc_consegne_mensili>=.99 & perc_consegne_mensili<1) 99 else round(100*perc_consegne_mensili)
perc_consegne_mensili=paste0("Monthly data sent: ", perc_consegne_mensili ,"%" )

# melt su dati annuali ####
options(warn=-1)
d_annuali=melt(d_annuali,id.vars = 1, measure.vars = (4:ncol(d_annuali)), variable.factor = F)
rm(vars)
options(warn=0)
d_annuali=d_annuali[,list(value=sum2(value)), keyby=.(id_battello,variable)]
d_annuali[is.na(value), value:=0]

# union mensili e annuali
d=rbindlist(list(d_mensili,d_annuali))
rm(d_mensili,d_annuali)
setkey(d, variable)

# aggrega in macro voci di costo ####
setkey(aggrega_var, variable)
d=aggrega_var[d]
rm(aggrega_var)
d[is.na(var),var:="unclassified"]
d=d[,list(value=sum2(value)), keyby=.(id_battello,var)]

# join with d and control variables
setkey(control_var_mensili, id_battello)
setkey(control_var_annuali, id_battello)
setkey(d, id_battello)
d=control_var_mensili[d]
d[is.na(giorni_mare), (c('giorni_mare','equipaggio_medio','volume_carburante')):=list(0,0,0)]
d=control_var_annuali[d]
d[is.na(Valore_di_mercato_del_bat_riltello), (c('Valore_di_mercato_del_bat_riltello','Numero_di_proprietari_del_bat_riltello')):=list(0,0)]
rm(control_var_annuali,control_var_mensili)

# left join with all ####
d[,sent:=as.numeric(0)]
var_names=names(d)[!names(d) %in% c('id_battello','var')]
setkey(all,id_battello,var)
setkey(d,id_battello,var)
all=d[all]
all[is.na(value), (var_names):=list(0)]
rm(d)

