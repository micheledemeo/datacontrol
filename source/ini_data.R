wd=getwd()

# load year
year_local=fread(pastedir(wd,"source/year"))$V1

# elenco bat_riltelli per controllo consegne ####
bat_ril=fread_mysql(tbname = 'battelli_rilevatori')
bat_ril=bat_ril[ anno==year_local, list(id_battello ,id_rilevatore)]
setkey(bat_ril, id_battello)

# importa tab di aggregazioni voci di costo, genera flotta per pesi, genera all data.table ####
aggrega_var=fread(pastedir(wd,"source/aggrega_var"),select = c('variable','var'))
all=fread_mysql(tbname = 'battelli')
all=all[grepl(year_local,data_riferimento), .(id_battello=id,id_strato,numero_ue,lft)]
setkey(all,id_battello)
all=all[bat_ril,nomatch=0][,i:=0]
bat_ril=bat_ril[ all[,list(id_battello,id_strato)] ]
# crea flotta per calcolo pesi
flotta=fread(pastedir(wd,"source/flotta"))

if ( file.exists(pastedir(wd,"source/data_current_year")) ) {
  cy=fread(pastedir(wd,"source/data_current_year"))
}

if ( nrow(all[lft==0])>0 ) {
  all2=all[lft==0]
  setkey(all2, numero_ue)
  setkey(flotta, numero_ue)
  flotta2=all2[flotta, nomatch=0][,list(id_battello, lft=i.lft)]
  rm(all2)
  setkey(flotta2, id_battello)
  setkey(all, id_battello)  
  all[flotta2,lft:=i.lft]
  rm(flotta2)
  
  if ( nrow(all[lft==0])>0 ) all[lft==0 , lft:=all[lft>0,mean(lft)] ]
}

setkey(flotta, id_strato)
flotta = flotta[ .(all[, unique(id_strato)]) ]
flotta=flotta[,list(numero_ue,id_strato,lft,id_battello=0)]
flotta = rbindlist( list(flotta,all[,list(numero_ue,id_strato,lft,id_battello)] ) )
setkey(flotta, numero_ue,id_battello)
flotta = flotta[.(unique(numero_ue)), mult="last"]
all[,lft:=NULL]
cj=aggrega_var[,.(i=0,var=unique(var))]
setkey(cj,i)
setkey(all,i)
all=all[cj, allow.cartesian=T][,i:=NULL]
rm(cj)

# importa labels per strati ####
s=fread_mysql(tbname = 'strati')
s=s[anno==year_local][,c('anno','id'):=NULL]
setkey(all, id_strato)
setkey(s, id_strato)
all=s[all]
all[,gsa:=as.character(gsa)]
all[is.na(regione), (c('regione','codsis199', 'codlft199', 'gsa', 'descrizione')):=list("UNKNOWN")]

# importa dati per yoy in waterfall ####
yoy=fread(pastedir(wd,"source/yoy"),select = c('id_strato','carbur','alcova','spcom','alcofi','spmanu','lavoro','proflor','ricavi','giorni') ) 
yoy=melt(yoy,id.vars = 1, measure.vars = (2:ncol(yoy)), variable.factor = F,variable.name = "var", value.name = "yoy_value")
setkey(yoy, id_strato)
yoy=s[yoy][,descrizione:=NULL]
rm(s)

# importa dati mensili ####
d_mensili=fread_mysql(tbname = 'schede_mensili')
d_mensili=d_mensili[anno==year_local]
# crea control_var_mensili table con variabili che non vanno usate in melt, ma aggregate a parte e messe in join
control_var_mensili=d_mensili[,list(giorni_mare=sum2(giorni_mare),equipaggio_medio=ifelse(sum2(giorni_mare)==0,mean(equipaggio_medio),sum2(equipaggio_medio*giorni_mare)/sum2(giorni_mare)) ,volume_carburante=sum2(volume_carburante+volume_lubrificante) ),keyby=.(id_battello)]
vars=fread(pastedir(wd,"source/vars_schede_mensili"),header = F)
vars=c("id_battello","mensilita",vars$V1)
# salvo info su percentuale_alla_parte
parte_fisso=d_mensili[percentuale_alla_parte>0,list(id_battello,percentuale_alla_parte)]
parte_fisso=parte_fisso[,list(percentuale_alla_parte=mean(percentuale_alla_parte)),keyby=.(id_battello)]
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
d_mensili=melt(d_mensili,id.vars = 1, measure.vars = (2:ncol(d_mensili)), variable.factor = F)
rm(vars)
options(warn=0)
sample_rate=copy(d_mensili)
d_mensili=d_mensili[,list(value=sum2(value)), keyby=.(id_battello,variable)]
d_mensili[is.na(value), value:=0]

# importa dati annuali ####
d_annuali=fread_mysql(tbname = 'schede_annuali')
d_annuali=d_annuali[anno==year_local]
# crea control_var_mensili table con variabili che non vanno usate in melt, ma aggregate a parte e messe in join
control_var_annuali=d_annuali[,list(Valore_di_mercato_del_battello=mean(Valore_di_mercato_del_battello),Numero_di_proprietari_del_battello=mean(Numero_di_proprietari_del_battello)),keyby=.(id_battello)]
vars=fread(pastedir(wd,"source/vars_schede_annuali"),header = F)
vars=c("id_battello",vars$V1)
d_annuali=d_annuali[,vars,with=F]

# calcola consegne annuali e mensili ####
consegne_annuali=d_annuali[,list(a=1),keyby=id_battello]
bat_ril=consegne_annuali[bat_ril][is.na(a),a:=0]
rm(consegne_annuali)
# consegne a livello di strato:
bat_str=bat_ril[,list(yearly_sent=sum2(a),yearly_tot=.N,monthly_sent=sum2(m),monthly_tot=12*.N),keyby=id_strato]
# consegne a livello di id_rilevatore:
bat_ril=bat_ril[,list(yearly_sent=sum2(a),yearly_tot=.N,monthly_sent=sum2(m),monthly_tot=12*.N),keyby=id_rilevatore]
perc_consegne_annuali=bat_ril[,sum2(yearly_sent)/sum2(yearly_tot)]
perc_consegne_annuali = if (perc_consegne_annuali>=.99 & perc_consegne_annuali<1) 99 else round(100*perc_consegne_annuali)
perc_consegne_annuali=paste0("Yearly data sent: ", perc_consegne_annuali ,"%" )
perc_consegne_mensili=bat_ril[,sum2(monthly_sent)/sum2(monthly_tot)]
perc_consegne_mensili = if (perc_consegne_mensili>=.99 & perc_consegne_mensili<1) 99 else round(100*perc_consegne_mensili)
perc_consegne_mensili=paste0("Monthly data sent: ", perc_consegne_mensili ,"%" )

# melt su dati annuali ####
options(warn=-1)
d_annuali=melt(d_annuali,id.vars = 1, measure.vars = (2:ncol(d_annuali)), variable.factor = F)
rm(vars)
options(warn=0)
sample_rate = rbindlist(list(sample_rate,d_annuali),fill = T)
d_annuali=d_annuali[,list(value=sum2(value)), keyby=.(id_battello,variable)]
d_annuali[is.na(value), value:=0]

# union mensili e annuali
d=rbindlist(list(d_mensili,d_annuali))
rm(d_mensili,d_annuali)

# fak ####
################################################################################################
source( paste(wd, "source/ricavi_fak.R", sep="/"),loc=T )
################################################################################################

# monte e stima retribuzione_lorda 
# (a cui poi si aggiungono oneri sociali, irpef, inail per ottenere lavoro, mediante join con aggrega_var)
setkey(d, variable)
var_per_monte=c('oneri_sociali','irpef','inail_malattie','inail_infortuni','costo_carburante','costo_lubrificante','diritti_mercato_ittico','provvigioni_grossista','provvigioni_astatore','facchinaggio_prodotti_ittici','spese_per_automezzi','spese_per_ghiaccio','cassette_e_imballaggio','altri_costi','riparazione_reti', 'spese_panatica_di_bordo','acquisto_esche','spese_telefonia_di_bordo','spese_tv_di_bordo','valore_totale')
stima_retribuzione_lorda=d[.(var_per_monte)][variable!='valore_totale',value:=-value][,list(monte=sum(value)),keyby=id_battello]
stima_retribuzione_lorda[monte<0, monte:=0]
temp=all[var=="lavoro",list(id_battello,id_strato)]
setkey(temp, id_battello)
setkey(parte_fisso, id_battello)
parte_fisso=parte_fisso[temp]
rm(temp)
parte_fisso[,percentuale_alla_parte:=mean(percentuale_alla_parte,na.rm = T), keyby=id_strato]
parte_fisso[is.na(percentuale_alla_parte),percentuale_alla_parte:=45]
parte_fisso=parte_fisso[,list(id_battello,percentuale_alla_parte=round(percentuale_alla_parte/100,2))]
setkey(parte_fisso, id_battello)
stima_retribuzione_lorda=parte_fisso[stima_retribuzione_lorda]
stima_retribuzione_lorda[is.na(percentuale_alla_parte), percentuale_alla_parte:=.5]
stima_retribuzione_lorda=stima_retribuzione_lorda[, ret_lor:=as.integer(round(percentuale_alla_parte*monte,0))][,list(id_battello, ret_lor)]
setkey(d, id_battello)
setkey(stima_retribuzione_lorda, id_battello)
d=stima_retribuzione_lorda[d]
d[ variable=='retribuzioni_lorde' ,value:=ret_lor ]
d[,ret_lor:=NULL]
rm(stima_retribuzione_lorda,parte_fisso)

# aggrega in macro voci di costo ####
setkey(d, variable)
setkey(aggrega_var, variable)
d=aggrega_var[d]
d[is.na(var),var:="unclassified"]
d=d[,list(value=sum2(value)), keyby=.(id_battello,var)]

# calcola sample rate ####
setkey(sample_rate,variable)
sample_rate=aggrega_var[sample_rate]
sample_rate=sample_rate[,list(card_count=.N,null_count=sum(is.na(value))),by=list(id_battello,var)]
sample_strata=all[,.N,keyby=list(id_battello,id_strato)][,N:=NULL]
setkey(sample_strata, id_battello)
sample_rate=sample_strata[sample_rate]
sample_rate=sample_rate[!is.na(id_strato),list(card_count=sum(card_count),null_count=sum(null_count),sample_units=length(unique(id_battello))),keyby=list(id_strato,var)]
fl=flotta[,list(universe_units=.N),keyby=id_strato]
sample_rate=fl[sample_rate]
st=all[,.N,keyby=list(id_strato,regione,codsis199,codlft199,gsa)][,N:=NULL]
sample_rate=st[sample_rate]
sample_rate=sample_rate[,list(id_strato,regione,codsis199,codlft199,gsa,var,null_count,card_count,sample_units,universe_units)]
sample_rate[,sample_achieved_units:=round(sample_units *(1-null_count/card_count) )]
rm(aggrega_var,sample_strata,fl,st)

# join with d and control variables
setkey(control_var_mensili, id_battello)
setkey(control_var_annuali, id_battello)
setkey(d, id_battello)
d=control_var_mensili[d]
d[is.na(giorni_mare), (c('giorni_mare','equipaggio_medio','volume_carburante')):=list(0,0,0)]
d=control_var_annuali[d]
d[is.na(Valore_di_mercato_del_battello), (c('Valore_di_mercato_del_battello','Numero_di_proprietari_del_battello')):=list(0,0)]
rm(control_var_annuali,control_var_mensili)

# left join with all ####
d[,sent:=as.numeric(1)]
var_names=names(d)[!names(d) %in% c('id_battello','var')]
setkey(all,id_battello,var)
setkey(d,id_battello,var)
all=d[all]
all[is.na(value), (var_names):=list(0)]
rm(d)

# aggiorna sent=1 quando battello ha almeno un sent=1 e un sent=0 (ha inviato solo una tra scheda annuale e mensile)
setkey(all, id_battello)
all[ all[sent==1,.(unique(id_battello))] , sent:=1 ]

# calcola pesi iniziali per battello ####
source( paste(wd, "source/ini_pr_i.R", sep="/"),loc=T )

# calcola dati parametrici per controllo outliers ####
all[,parameter:=as.numeric(0) ]
all[var %in% c('spmanu','alcofi','amm','indeb','invest'),parameter:=round(as.numeric(value)) ]
all[var %in% c('alcova','carbur','ricavi','ricavi_est') &  giorni_mare>0, parameter:=round(value/giorni_mare) ]
all[var=='lavoro' &  equipaggio_medio>0, parameter:=round(value/equipaggio_medio) ]
ricavi=all[var=='ricavi' & value>0, .(id_battello,ricavi=value)]
setkey(all, id_battello)
setkey(ricavi, id_battello)
all=ricavi[all]
all[var=='spcom' & ricavi>0 ,  parameter:=round(value/ricavi,3)  ]
all[,ricavi:=NULL]
rm(ricavi)
#all[,value:=as.numeric(value)]

# deriva elenco rilevatori ####
ril_dt=all[,.N,keyby=id_rilevatore]
ril_cognome=fread_mysql(tbname = 'rilevatori',integer64="character")[,list(id,cognome)]
setnames(ril_cognome, 'id', 'id_rilevatore')
setkey(ril_cognome, id_rilevatore)
ril_dt=ril_cognome[ril_dt][,N:=NULL]
rm(ril_cognome)

# crea strato battello ####
strato_battello=all[,.N,keyby=list(id_battello,id_strato)][,N:=NULL]

# gestisci valori iniziali di value e parameter, in accordo con la hist ####
# value_or è il valore originiale scaricato da server
# value_ok è il valore finale su cui si fa l'espansione. esso comprende le imputazioni dell'utente
# value è il campo usato nelle visualizzazioni grafiche. è pari a value_or (default) se "Keep outliers in your charts", mentre diventa pari a value_ok quando "Take a tour with imputations"

all[,c('value_ok','value_or','parameter_ok','parameter_or','is_ok','hist_is_ok','notes'):=list(value,value,parameter,parameter,0,as.numeric(NA),"") ]

# verifico se c'è un nicoda in ftp (faccio il get e verifico se esiste in locale)), quindi lo importo e aggiorno all (in tal caso infatti nicoda.csv è più recente di dati in tabella). 
# Se non c'è in ftp, importo da db e aggiorno all
ftp(action = "get")
if( file.exists(paste0(temp_dir_nicoda,"\\nicoda.csv")) && length(readLines(paste0(temp_dir_nicoda,"\\nicoda.csv"),n = 1))>0 ) {
  hist=fread( paste0(temp_dir_nicoda,"\\nicoda.csv") , sep=";")
  setnames(hist, names(hist), c('id_battello','var','day','year','pr_i','hist_value','hist_parameter','hist_notes','closing_session'))
  
} else {
  hist=fread_mysql(tbname = 'nicoda')
}

if(nrow(hist)>0){
  hist=hist[hist_notes!="control record"]
  hist_join=hist[,list(id_battello,var,hist_value,hist_parameter,hist_notes,closing_session)]
  hist_join[,c('id_battello','var','hist_value','hist_parameter','hist_notes','closing_session'):=list(as.integer(id_battello),as.character(var),as.integer(round(hist_value,0)),as.numeric(hist_parameter),as.character(hist_notes),as.character(closing_session))]
  setkey(hist_join, id_battello,var)
  setkey(all, id_battello,var)
  hist_join[,hist_parameter:=as.numeric(hist_parameter)]
  all=hist_join[all]
  all[!is.na(hist_value), (c('value_ok','parameter_ok','notes','is_ok','hist_is_ok')):=list(hist_value,hist_parameter,hist_notes,1L,1L)]
  rm(hist_join)
} else {
  all[,c('hist_value','hist_parameter','hist_notes','hist_is_ok'):=list(NA)]
  hist=data.table(id_battello=0L,var="",hist_value=0L,hist_parameter=0.,hist_notes="",closing_session="")[0]
}

