
# aggiorna i sent in flotta
setkey(flotta, id_battello)
flotta[.( all[sent==1,unique(id_battello)] )  , sent:=1 ]
flotta[is.na(sent) , sent:=0]
# strati con almeno un sent=0 e almeno 2 unità con sent=1
flotta[,remove_to_hv:=0]
setkey(flotta, id_strato,sent)
#str_sent: strati con almeno un campionario inviato. Mettendo sent=0, i battelli campionari di questo strato che hanno sent=0(non inviati), vanno eliminati dal calcolo
str_sent=flotta[,sum(sent), by=id_strato][V1>1,.(id_strato,sent=0)]
setkey(str_sent, id_strato,sent)
# tag per battelli campionari da non considerare nel calcolo hv, essendoci nello strato almeno due unità con sent=1 che possono esser usati come proxy per le mancate risposte (ib_battello>0 & sent=0)
flotta[str_sent, remove_to_hv:=ifelse(id_battello>0,1,0)]
flotta_temp=flotta[,list(id_strato,lft,id_battello=ifelse(remove_to_hv==1,0,id_battello))]

# calcola pr_i e gestisci censimenti####
setkey(flotta_temp, id_strato,lft)
flotta_temp[,eti:=1:nrow(.SD), by=id_strato]
strati_censimento=flotta_temp[,list(.N,n=sum(ifelse(id_battello>0,1,0))),keyby=id_strato][N-n==0,id_strato]

if(length(strati_censimento)>0) {
  
  pr_i=flotta_temp[!id_strato %in% strati_censimento,data.table(.SD[id_battello>0,.(lft,id_battello,eti)],pr_i=diag(hv_pij(lft,  n=nrow(.SD[id_battello>0]), eti=.SD[id_battello>0,eti], M=T) ) ) , keyby=id_strato]
  pr_i=pr_i[,list(id_battello,pr_i)]
  pr_i=rbindlist(list(pr_i,flotta_temp[id_strato %in% strati_censimento,.(id_battello, pr_i=1)]))
  
} else {
  
  pr_i=flotta_temp[,data.table(.SD[id_battello>0,.(lft,id_battello,eti)],pr_i=diag(hv_pij(lft,  n=nrow(.SD[id_battello>0]), eti=.SD[id_battello>0,eti], M=T) ) ) , keyby=id_strato]
  pr_i=pr_i[,list(id_battello,pr_i)]
  
}

# calcolo fattore di correzione
if ( exists("cy") ) {
  setkey(cy,id_strato)
  ric=all[var=="ricavi", list(id_battello,id_strato,value)]
  setkey(ric,id_battello)
  setkey(pr_i,id_battello)
  #nomatch = 0 => esclude i battelli che vengono rimossi perché non inviati, ma presenti in strati che in cui ci sono almeno 2 inviati
  ric=pr_i[ric,nomatch=0][,ric_esp_nisea:=sum(value/pr_i),by=id_strato]
  setkey(ric,id_strato)
  ric=cy[ric]
  ric[is.na(ricavi), ricavi:=ric_esp_nisea] # corr_fact will be 1 for that
  ric[,corr_fact:=ricavi/ric_esp_nisea]
  setkey(ric,id_battello)
  # weight_with_correction = weight * corr_fact = 1/pr * corr_fact --> 
  # pr_with_correction= 1 / weight_with_correction  = 1/(1/pr * corr_fact) = pr / corr_fact
  pr_i[ric, pr_i:=pr_i*(1/corr_fact)]
  rm(ric)
}
  
setkey(pr_i,id_battello)
rm(flotta_temp)