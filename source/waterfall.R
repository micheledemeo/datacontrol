
# waterfall italy ####
d_waterfall_italy=reactive({
  
  if( nrow(d_panel())>0 ){
    
    waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'),value=as.numeric(0))
    setkey(waterfall, var)
    
    waterfall_cost=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(var) ]
    setkey(waterfall_cost, var)
    
    waterfall_proflor=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor') ][,list(var,value)]
    setkey(waterfall_proflor, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o]
    setkey(waterfall, o)
    
    waterfall[,end:=cumsum(value)]
    waterfall[,start:=end-value]
        
  }
  
})

# waterfall per strato ####
d_waterfall_strata=reactive({
  
  if( nrow(d_panel())>0 ){
    
    waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'), id_strato=d_panel()[,unique(id_strato)],value=as.numeric(0) )
    setkey(waterfall, id_strato, var)
    
    waterfall_cost=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(id_strato,var) ]
    setkey(waterfall_cost, id_strato, var)
    
    waterfall_proflor=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor'), keyby=.(id_strato) ][,list(id_strato,var,value)]
    setkey(waterfall_proflor, id_strato, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o]
    setkey(waterfall, id_strato,o)
    
    waterfall[,end:=cumsum(value), keyby=.(id_strato)]
    waterfall[,start:=end-value, keyby=.(id_strato)]
    
  }
    
})

# waterfall per loa ####
d_waterfall_loa=reactive({
  
  if( nrow(d_panel())>0 ){
    
    waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'), codlft199=d_panel()[,unique(codlft199)],value=as.numeric(0) )
    setkey(waterfall, codlft199, var)
    
    waterfall_cost=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(codlft199,var) ]
    setkey(waterfall_cost, codlft199, var)
    
    waterfall_proflor=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor'), keyby=.(codlft199) ][,list(codlft199,var,value)]
    setkey(waterfall_proflor, codlft199, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o]
    setkey(waterfall, codlft199,o)
    
    waterfall[,end:=cumsum(value), keyby=.(codlft199)]
    waterfall[,start:=end-value, keyby=.(codlft199)]
    
  }
  
})

# waterfall per gear ####
d_waterfall_gear=reactive({
  
  if( nrow(d_panel())>0 ){
    
    waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'), codsis199=d_panel()[,unique(codsis199)],value=as.numeric(0) )
    setkey(waterfall, codsis199, var)
    
    waterfall_cost=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(codsis199,var) ]
    setkey(waterfall_cost, codsis199, var)
    
    waterfall_proflor=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor'), keyby=.(codsis199) ][,list(codsis199,var,value)]
    setkey(waterfall_proflor, codsis199, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o]
    setkey(waterfall, codsis199,o)
    
    waterfall[,end:=cumsum(value), keyby=.(codsis199)]
    waterfall[,start:=end-value, keyby=.(codsis199)]
    
  }
  
})

# waterfall per gear,loa ####
d_waterfall_gear_loa=reactive({
  
  if( nrow(d_panel())>0 ){
    
    waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'), codsis199=d_panel()[,unique(codsis199)],codlft199=d_panel()[,unique(codlft199)],value=as.numeric(0) )
    setkey(waterfall, codsis199,codlft199, var)
    
    waterfall_cost=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(codsis199,codlft199,var) ]
    setkey(waterfall_cost, codsis199,codlft199, var)
    
    waterfall_proflor=d_panel()[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor'), keyby=.(codsis199,codlft199) ][,list(codsis199,codlft199,var,value)]
    setkey(waterfall_proflor, codsis199,codlft199, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o]
    setkey(waterfall, codsis199,codlft199,o)
    
    waterfall[,end:=cumsum(value), keyby=.(codsis199,codlft199)]
    waterfall[,start:=end-value, keyby=.(codsis199,codlft199)]
    
  }
  
})


