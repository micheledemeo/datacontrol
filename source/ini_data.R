# costi import ####
d=fread_mysql(tbname = 'schede_mensili')
d=d[anno==2014]
vars=c('id','giorni_mare','retribuzioni_lorde','oneri_sociali','irpef','inail_malattie','inail_infortuni','costo_carburante','costo_lubrificante','diritti_mercato_ittico','provvigioni_grossista','provvigioni_astatore','facchinaggio_prodotti_ittici','spese_per_automezzi','spese_per_ghiaccio','cassette_e_imballaggio','altri_costi','spese_verniciatura','manutenzione_ordinaria','spese_per_fabbro_e_carpenterie','spese_alaggio','riparazioni_impianto_elettrico','riparazioni_impianto_meccanico','riparazioni_impianto_idraulico','riparazione_radar','riparazione_reti','riparazione_frigoriferi','acquisto_cavi','spedizione_pratiche_doganali','spese_panatica_di_bordo','acquisto_nuovi_ami','indumenti_di_lavoro_per_equipaggio','acquisto_esche','spese_telefonia_di_bordo','spese_tv_di_bordo','spese_varie_per_materiale_dotazione_di_bordo','altri_costi_di_produzione')

# crea ids table con var, rimuovi ids da d ####
ids=d[,.(id,id_rilevatore,id_battello,id_strato,giorni_mare)]
d=d[,vars,with=F]
options(warn=-1)
d=melt(d,id.vars = 1, measure.vars = (2:ncol(d)), variable.factor = F)
rm(vars)
options(warn=0)
d[is.na(value), value:=0]
setkey(d, variable)

# importa tab di aggregazioni voci di costo e aggrega voci ####
aggrega_var=fread(pastedir(wd,"input/aggrega_var"),select = c('variable','var'))
setkey(aggrega_var, variable)
d=aggrega_var[d]
rm(aggrega_var)
d[is.na(var),var:="costi_non_classificati"]
d=d[,list(value=sum(value)), keyby=.(id,var)]
setkey(ids, id)
d=ids[d]
rm(ids)
d=d[,list(value=sum(value)),.(id_rilevatore,id_battello,id_strato,giorni_mare,var)]

# importa labels di strati e associa a d ####
s=fread_mysql(tbname = 'strati')
s=s[anno==2014][,c('anno','id'):=NULL]
setkey(d, id_strato)
setkey(s, id_strato)
d=s[d]
rm(s)

# d è pronto
