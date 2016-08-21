if( file.exists(pastedir(wd,"source/ricavi_campionari_fak")) ) {

  fak=fread( pastedir(wd,"source/ricavi_campionari_fak") )
  fak[,id_battello:=as.integer(id_battello)]
  fak = fak[!is.na(id_battello)]
  setkey(fak,id_battello)
  setkey(d,id_battello)
  d=fak[d]
  d[variable=="valore_totale" & !is.na(ricavi_fak), value:=as.numeric(ricavi_fak)]
  d[,ricavi_fak:=NULL]

}
