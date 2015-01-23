wd=getwd()
d=fread_mysql(tbname = 'schede_annuali')
d=d[anno==2014]

vars=c('contabilita_e_tenuta_libri_paga','spese_legali_e_notarili','assicurazione_sul_natante','costi_di_gestione_cc_bancari','interessi_passivi_su_cc_bancari_e_mutui','spese_servizi_e_quota_associativa_sindacale','servizi_di_banchina','gestione_e_costi_magazzini_a_terra','spese_viaggi_e_spostamenti','bollo_e_assicurazione_automezzi_aziendali','manutenzione_automezzi_aziendali','acquisto_automezzi_aziendali','spese_cancelleria_e_stampati','costi_ispezione_RINA','collaudi_VHF','spese_certificati_sanitari','estintori','servizio_veterinario_ausl','rinnovo_medicinali','certificati_sanitari_ICCAT','partita_iva_e_altri_diritti_amministrativi','spese_istanze_e_bolli_con_PA_per_istanze','rinnovo_licenze_di_pesca','tributi_a_capitanerie_di_porto','quota_annuale_CCIAA','spese_MUD_CONAI','canone_blubox','altri_costi_fissi','indebitamento_per_fido_bancario','indebitamento_per_mutuo','indebitamento_per_altri_prestiti','indebitamento_per_anticipo_da_fornitori','acquisto_motore','acquisto_nuovi_attrezzi_da_pesca','acquisto_nuove_reti','acquisto_nuovo_impianto_elettrico','acquisto_nuovo_impianto_idraulico','acquisto_nuovo_frigorifero','acquisto_nuova_blubox','acquisto_nuova_strumentazione','rimborso_fermo_pesca','ricavi_extra_pesca','altri_sussidi','fonte_informazione','spese_tracciabilita','ricavi_trasformazione','quote_annuali_ammortamento')
ids=d[,.(id,id_rilevatore,id_battello,id_strato,Valore_di_mercato_del_battello,Numero_di_proprietari_del_battello)]
d=d[,vars,with=F]
options(warn=-1)
d=melt(d,id.vars = 1, measure.vars = (2:ncol(d)), variable.factor = F)
rm(vars)
options(warn=0)
d[is.na(value), value:=0]
setkey(d, variable)
