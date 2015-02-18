
# aggiorna i sent in flotta
setkey(flotta, id_battello)
flotta[.( all[sent==1,unique(id_battello)] )  , sent:=1 ]
flotta[is.na(sent) , sent:=0]
# strati con zero sent e almeno 2 unità con sent=1
flotta[,remove_to_hv:=0]
setkey(flotta, id_strato,sent)
str_sent=flotta[,sum(sent), by=id_strato][V1>1,.(id_strato,sent=0)]
setkey(str_sent, id_strato,sent)
flotta[str_sent, remove_to_hv:=ifelse(id_battello>0,1,0)]

# calcola pr_i ####
flotta_temp=flotta[remove_to_hv==0,list(id_strato,lft,id_battello)]
setkey(flotta_temp, id_strato,lft)
flotta_temp[,eti:=1:nrow(.SD), by=id_strato]
strati_censimento=flotta_temp[,list(.N,n=sum(ifelse(id_battello>0,1,0))),keyby=id_strato][N-n==0,id_strato]
if(length(strati_censimento)>0) {
  
  pr_i=flotta_temp[!id_strato %in% strati_censimento,data.table(.SD[id_battello>0,.(lft,id_battello,eti)],pr_i=diag(hv_pij(lft,  n=nrow(.SD[id_battello>0]), eti=.SD[id_battello>0,eti], M=T) ) ) , keyby=id_strato]
  
} else {
  
  pr_i=flotta_temp[,data.table(.SD[id_battello>0,.(lft,id_battello,eti)],pr_i=diag(hv_pij(lft,  n=nrow(.SD[id_battello>0]), eti=.SD[id_battello>0,eti], M=T) ) ) , keyby=id_strato]
  
}
pr_i=pr_i[,list(id_battello,pr_i)]
# aggiungo censimenti
if(length(strati_censimento)>0) pr_i=rbindlist(list(pr_i,flotta_temp[id_strato %in% strati_censimento,.(id_battello, pr_i=1)]))
setkey(pr_i,id_battello)
rm(flotta_temp)