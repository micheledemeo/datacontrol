wd=getwd()

# elenco battelli per controllo consegne ####
bat=fread_mysql(tbname = 'battelli_rilevatori')
bat=bat[ anno==2014, list(id_battello ,id_rilevatore)]
setkey(bat, id_battello)

# dataset mensili #####
d_mensili=fread_mysql(tbname = 'schede_mensili')
d_mensili=d_mensili[anno==2014]
consegne_mensili=d_mensili[,list(m=.N),.(id_battello,mensilita)][mensilita==12,m:=12][,mensilita:=NULL]
setkey(consegne_mensili, id_battello)
bat=consegne_mensili[bat][is.na(m),m:=0]
rm(consegne_mensili)

vars=c('id_battello','id_rilevatore','id_strato' ,'retribuzioni_lorde','oneri_sociali','irpef','inail_malattie','inail_infortuni','costo_carburante','costo_lubrificante','diritti_mercato_ittico','provvigioni_grossista','provvigioni_astatore','facchinaggio_prodotti_ittici','spese_per_automezzi','spese_per_ghiaccio','cassette_e_imballaggio','altri_costi','spese_verniciatura','manutenzione_ordinaria','spese_per_fabbro_e_carpenterie','spese_alaggio','riparazioni_impianto_elettrico','riparazioni_impianto_meccanico','riparazioni_impianto_idraulico','riparazione_radar','riparazione_reti','riparazione_frigoriferi','acquisto_cavi','spedizione_pratiche_doganali','spese_panatica_di_bordo','acquisto_nuovi_ami','indumenti_di_lavoro_per_equipaggio','acquisto_esche','spese_telefonia_di_bordo','spese_tv_di_bordo','spese_varie_per_materiale_dotazione_di_bordo','altri_costi_di_produzione')

# crea control_var_mensili table con variabili che non vanno usate in melt, ma aggregate a parte e messe in join
control_var_mensili=d_mensili[,list(giorni_mare=sum(giorni_mare),equipaggio_medio=mean(equipaggio_medio)),keyby=.(id_battello)]

d_mensili=d_mensili[,vars,with=F]
options(warn=-1)
d_mensili=melt(d_mensili,id.vars = 1:3, measure.vars = (4:ncol(d_mensili)), variable.factor = F)
rm(vars)
options(warn=0)

d_mensili=d_mensili[,list(value=sum(value)), keyby=.(id_battello,id_rilevatore,id_strato,variable)]
d_mensili[is.na(value), value:=0]

# dataset annuali #####
d_annuali=fread_mysql(tbname = 'schede_annuali')
d_annuali=d_annuali[anno==2014]

consegne_annuali=d_annuali[,list(a=1),keyby=id_battello]
bat=consegne_annuali[bat][is.na(a),a:=0]
rm(consegne_annuali)
bat=bat[,list(yearly_sent=sum(a),yearly_tot=.N,monthly_sent=sum(m),monthly_tot=12*.N),keyby=id_rilevatore]
perc_consegne_annuali=bat[,sum(yearly_sent)/sum(yearly_tot)]
perc_consegne_annuali = if (perc_consegne_annuali>=.99 & perc_consegne_annuali<1) 99 else round(100*perc_consegne_annuali)
perc_consegne_annuali=paste0("Yearly data sent: ", perc_consegne_annuali ,"%" )
perc_consegne_mensili=bat[,sum(monthly_sent)/sum(monthly_tot)]
perc_consegne_mensili = if (perc_consegne_mensili>=.99 & perc_consegne_mensili<1) 99 else round(100*perc_consegne_mensili)
perc_consegne_mensili=paste0("Monthly data sent: ", perc_consegne_mensili ,"%" )

vars=c('id_battello','id_rilevatore','id_strato','contabilita_e_tenuta_libri_paga','spese_legali_e_notarili','assicurazione_sul_natante','costi_di_gestione_cc_bancari','interessi_passivi_su_cc_bancari_e_mutui','spese_servizi_e_quota_associativa_sindacale','servizi_di_banchina','gestione_e_costi_magazzini_a_terra','spese_viaggi_e_spostamenti','bollo_e_assicurazione_automezzi_aziendali','manutenzione_automezzi_aziendali','acquisto_automezzi_aziendali','spese_cancelleria_e_stampati','costi_ispezione_RINA','collaudi_VHF','spese_certificati_sanitari','estintori','servizio_veterinario_ausl','rinnovo_medicinali','certificati_sanitari_ICCAT','partita_iva_e_altri_diritti_amministrativi','spese_istanze_e_bolli_con_PA_per_istanze','rinnovo_licenze_di_pesca','tributi_a_capitanerie_di_porto','quota_annuale_CCIAA','spese_MUD_CONAI','canone_blubox','altri_costi_fissi','indebitamento_per_fido_bancario','indebitamento_per_mutuo','indebitamento_per_altri_prestiti','indebitamento_per_anticipo_da_fornitori','acquisto_motore','acquisto_nuovi_attrezzi_da_pesca','acquisto_nuove_reti','acquisto_nuovo_impianto_elettrico','acquisto_nuovo_impianto_idraulico','acquisto_nuovo_frigorifero','acquisto_nuova_blubox','acquisto_nuova_strumentazione','rimborso_fermo_pesca','ricavi_extra_pesca','altri_sussidi','fonte_informazione','spese_tracciabilita','ricavi_trasformazione','quote_annuali_ammortamento')

# crea control_var_mensili table con variabili che non vanno usate in melt, ma aggregate a parte e messe in join
control_var_annuali=d_annuali[,list(Valore_di_mercato_del_battello=mean(Valore_di_mercato_del_battello),Numero_di_proprietari_del_battello=mean(Numero_di_proprietari_del_battello)),keyby=.(id_battello)]

d_annuali=d_annuali[,vars,with=F]
options(warn=-1)
d_annuali=melt(d_annuali,id.vars = 1:3, measure.vars = (4:ncol(d_mensili)), variable.factor = F)
rm(vars)
options(warn=0)

d_annuali=d_annuali[,list(value=sum(value)), keyby=.(id_battello,id_rilevatore,id_strato,variable)]
d_annuali[is.na(value), value:=0]

# union mensili e annuali
d=rbindlist(list(d_mensili,d_annuali))
rm(d_mensili,d_annuali)
setkey(d, variable)

# importa tab di aggregazioni voci di costo e aggrega voci ####
aggrega_var=fread(pastedir(wd,"input/aggrega_var"),select = c('variable','var'))
setkey(aggrega_var, variable)
d=aggrega_var[d]
rm(aggrega_var)
d[is.na(var),var:="unclassified"]
d=d[,list(value=sum(value)), keyby=.(id_rilevatore,id_battello,id_strato,var)]

# join with control variables
setkey(control_var_mensili, id_battello)
setkey(control_var_annuali, id_battello)
setkey(d, id_battello)

d=control_var_mensili[d]
d[is.na(giorni_mare), (c('giorni_mare','equipaggio_medio')):=list(0,0)]

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
