if( file.exists(pastedir(Sys.getenv("LOCALAPPDATA"),"/ricavi_campionari_fak")) ) {
  
  fak=fread( pastedir(Sys.getenv("LOCALAPPDATA"),"/ricavi_campionari_fak") )
  setkey(fak,id_battello)
  setkey(d,id_battello)
  d=fak[d]
  d[variable=="valore_totale" & !is.na(ricavi_fak), value:=ricavi_fak]
  d[,ricavi_fak:=NULL]

}