
universe_data=reactive({
  
  dep_imputation_tab()
  dep_imputation_manual_tab()
  
  out=all[giorni_mare>(input_check_gio()-1) & var %in% c('carbur','alcova','spcom','alcofi','spmanu','lavoro','ricavi'), 
          list(id_battello,id_strato,value_ok,value_or,var) ]
  
  if(input_exp_data()=='orig') out[,value_e:=value_or]  else out[,value_e:=value_ok] 

  setkey(out,id_battello)
  setkey(pr_i,id_battello)
  out=pr_i[out]
  
  # refresh di pr_i
  if( input_not_sent_as_0()==1 ) {
    # genero il pr_i_temp per il refresh dei soli strati selezionai in imputation
    source( paste(getwd(), "source/refresh_pr_i_exp_data.R", sep="/"),loc=T )
    setkey(out, id_battello)
    out[pr_i_temp, pr_i:=i.pr_i, nomatch=0]
  }
  
  out=out[,list(value_e=round(sum(value_e/pr_i),0)), keyby=list(id_strato,var) ]
  out=out[,dcast.data.table(.SD, id_strato ~ var,value.var = "value_e") ]
  out=out[,proflor:=round(ricavi-(carbur+alcova+spcom+alcofi+spmanu+lavoro),0)]
  
  out=out[,c('id_strato','carbur','alcova','spcom','alcofi','spmanu','lavoro','proflor','ricavi'),with=F]
  out
  
})

sample_data=reactive({
  
  dep_imputation_tab()
  dep_imputation_manual_tab()
  
  out=all[giorni_mare>(input_check_gio()-1) & var %in% c('carbur','alcova','spcom','alcofi','spmanu','lavoro','ricavi'), 
          list(id_battello,id_strato,value_ok,value_or,var) ]
  
  if(input_exp_data()=='orig') out[,value_e:=value_or]  else out[,value_e:=value_ok] 
  
  setkey(out,id_battello)
  setkey(pr_i,id_battello)
  out=pr_i[out]
  
  # refresh di pr_i
  if( input_not_sent_as_0()==1 ) {
    # genero il pr_i_temp per il refresh dei soli strati selezionai in imputation
    source( paste(getwd(), "source/refresh_pr_i_exp_data.R", sep="/"),loc=T )
    setkey(out, id_battello)
    out[pr_i_temp, pr_i:=i.pr_i, nomatch=0]
  }
  
  out[,uti:=ifelse(var=='ricavi',1,-1)]
  
  out=rbindlist(list(
                    out[,list(id_battello,id_strato,var,value=value_e,pr_i)],
                    out[,list(var='proflor',value=round(sum(value_e*uti),0),pr_i=mean(pr_i)), by=list(id_battello,id_strato)]
                    ),
                use.names = T
                ) 
  setorder(out,id_strato,id_battello,var)
  out
  
})