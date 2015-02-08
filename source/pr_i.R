
# calcola peso battello e join con all ####
setkey(flotta, id_strato,lft)
flotta[,eti:=1:nrow(.SD), by=id_strato]
strati_censimento=flotta[,list(.N,n=sum(ifelse(id_battello>0,1,0))),keyby=id_strato][N-n==0,id_strato]
pr_i=flotta[!id_strato %in% strati_censimento,data.table(.SD[id_battello>0,.(lft,id_battello,eti)],pr_i=diag(hv_pij(lft,  n=nrow(.SD[id_battello>0]), eti=.SD[id_battello>0,eti], M=T) ) ) , keyby=id_strato]
pr_i=pr_i[,list(id_battello,pr_i)]
# aggiungo centsimenti
pr_i=rbindlist(list(pr_i,flotta[id_strato %in% strati_censimento,.(id_battello, pr_i=1)]))
