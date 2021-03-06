# con questo processo si considera pari a zero i values dei battelli non inviati, quindi si esegue l'espansione ricalcolando le pr_i che inizialmente sono Inf per i sent=0 & id_battello>0


# calcola pr_i ####
setkey(flotta, id_strato)
flotta_temp=flotta[.( as.numeric(input_strato_imp_m()) )][,list(id_strato,lft,id_battello)]

if(nrow(flotta_temp)>0) { # input_strato_imp_m() può essere nullo se l'utente non seleziona nulla. Non è vero per selected_strata() del tab outliers, dove se tutti i box sono nulli, equivalgono a selezionare tutto.
  
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
  
  # calcolo fattore di correzione
  if ( exists("cy") ) {
    setkey(cy,id_strato)
    ric=all[var=="ricavi", list(id_battello,id_strato,value)]
    setkey(ric,id_battello)
    setkey(pr_i_temp,id_battello)
    ric=pr_i_temp[ric][,ric_esp_nisea:=sum(value/pr_i),by=id_strato]
    setkey(ric,id_strato)
    ric=cy[ric]
    ric[is.na(ricavi), ricavi:=ric_esp_nisea] # corr_fact will be 1 for that
    ric[,corr_fact:=ricavi/ric_esp_nisea]
    setkey(ric,id_battello)
    # weight_with_correction = weight * corr_fact = 1/pr * corr_fact --> 
    # pr_with_correction= 1 / weight_with_correction  = 1/(1/pr * corr_fact) = pr / corr_fact
    pr_i_temp[ric, pr_i:=pr_i*(1/corr_fact)]
    rm(ric)
  }
  
  setkey(pr_i_temp, id_battello)
  
} else {
  pr_i_temp=data.table(id_battello=as.integer(0), pr_i=as.numeric(0) )
  setkey(pr_i_temp, id_battello)
}

