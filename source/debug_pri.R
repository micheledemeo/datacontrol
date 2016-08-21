for (s in flotta_temp[!id_strato %in% strati_censimento ,unique(id_strato)]) {
  print(s)
  flotta_temp[ id_strato==s,data.table(.SD[id_battello>0,.(lft,id_battello,eti)],pr_i=diag(hv_pij(lft,  n=nrow(.SD[id_battello>0]), eti=.SD[id_battello>0,eti], M=T) ) ) , keyby=id_strato]
  
}

