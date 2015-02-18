# con questo processo si considera pari a zero i values dei battelli non inviati, quindi si esegue l'espansione ricalcolando le pr_i che inizialmente sono Inf per i sent=0 & id_battello>0


# calcola pr_i ####
setkey(flotta, id_strato)
flotta_temp=flotta[.( selected_strata() )][,list(id_strato,lft,id_battello)]
# selected_strata=72
# flotta_temp=flotta[.(selected_strata)][,list(id_strato,lft,id_battello)]
setkey(flotta_temp, id_strato,lft)
flotta_temp[,eti:=1:nrow(.SD), by=id_strato]
strati_censimento=flotta_temp[,list(.N,n=sum(ifelse(id_battello>0,1,0))),keyby=id_strato][N-n==0,id_strato]
if(length(strati_censimento) >0) {
  
  pr_i_temp=flotta_temp[!id_strato %in% strati_censimento,data.table(.SD[id_battello>0,.(lft,id_battello,eti)],pr_i=diag(hv_pij(lft,  n=nrow(.SD[id_battello>0]), eti=.SD[id_battello>0,eti], M=T) ) ) , keyby=id_strato]
  
} else {
  
  pr_i_temp=flotta_temp[,data.table(.SD[id_battello>0,.(lft,id_battello,eti)],pr_i=diag(hv_pij(lft,  n=nrow(.SD[id_battello>0]), eti=.SD[id_battello>0,eti], M=T) ) ) , keyby=id_strato]
  
}
  
pr_i_temp=pr_i_temp[,list(id_battello,pr_i)]
# aggiungo censimenti
if(length(strati_censimento)>0) pr_i_temp=rbindlist(list(pr_i_temp,flotta_temp[id_strato %in% strati_censimento,.(id_battello, pr_i=1)]))
setkey(pr_i_temp, id_battello)

