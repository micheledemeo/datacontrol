
# waterfall italy ####
d_waterfall_italy=reactive({
  
  if( ok_rows() ){
    
    d_wat = 
      if(input_apply_weights()==TRUE) 
        d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(var,value=as.numeric(value/pr_i) ) ]
    else  
      d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(id_strato,var,value=as.numeric(value)) ]
    
    waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'),value=as.numeric(0))
    setkey(waterfall, var)
    
    waterfall_cost=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=sum( value) ), by=.(var) ]
    setkey(waterfall_cost, var)
    
    waterfall_proflor=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor') ][,list(var,value)]
    rm(d_wat)
    setkey(waterfall_proflor, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o] 
    setkey(waterfall, o)     
    waterfall[,end:=cumsum(value)]
    waterfall[,start:=end-value]
    
    # aggiungo yoy
    waterfall[,yoy:=""]
    yoy_temp=yoy[,list(yoy_value=sum(yoy_value)), keyby=var]
    setkey(waterfall, var)
    waterfall[yoy_temp, yoy:=paste0( ifelse(value/yoy_value<1,"-","+") , abs(round(value/yoy_value-1,0)), "%")]
    move_rect=waterfall[var=='ricavi',value/10]
    waterfall[,(c('end','start')):= list( end+move_rect , start+move_rect) ]
    setkey(waterfall, o)

    waterfall

  }
  
})

# waterfall per strato ####
d_waterfall_strata=reactive({
  
  if( ok_rows() ){
    
    d_wat = 
      if(input_apply_weights()==TRUE) 
        d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(id_strato,var,value=value/pr_i) ]
    else  
      d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(id_strato,var,value) ]
    
    waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'), id_strato=d_panel()[,unique(id_strato)],value=as.numeric(0) )
    setkey(waterfall, id_strato, var)
    
    waterfall_cost=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(id_strato,var) ]
    setkey(waterfall_cost, id_strato, var)
    
    waterfall_proflor=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor'), keyby=.(id_strato) ][,list(id_strato,var,value)]
    rm(d_wat)
    setkey(waterfall_proflor, id_strato, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o]
    setkey(waterfall, id_strato,o)    
    waterfall[,end:=cumsum(value), keyby=.(id_strato)]
    waterfall[,start:=end-value, keyby=.(id_strato)]
    
    # aggiungo yoy
    waterfall[,yoy:=""]
    yoy_temp=yoy[,list(yoy_value=sum(yoy_value)), keyby=.(id_strato,var)]
    setkey(waterfall, id_strato,var)
    waterfall[yoy_temp, yoy:=paste0( ifelse(value/yoy_value<1,"-","+") , abs(round(value/yoy_value-1,0)), "%")]
    move_rect=waterfall[var=='ricavi',list(moveup=value/3), keyby=id_strato]
    waterfall[move_rect,(c('end','start')):= list( end+moveup , start+moveup) ]
    setkey(waterfall, o)
    
    waterfall
    
  }
    
})

# waterfall per loa ####
d_waterfall_loa=reactive({
  
  if( ok_rows() ){
    
    d_wat = 
      if(input_apply_weights()==TRUE) 
        d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(codlft199,var,value=value/pr_i) ]
    else  
      d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(codlft199,var,value) ]
    
    waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'), codlft199=d_panel()[,unique(codlft199)],value=as.numeric(0) )
    setkey(waterfall, codlft199, var)
    
    waterfall_cost=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(codlft199,var) ]
    setkey(waterfall_cost, codlft199, var)
    
    waterfall_proflor=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor'), keyby=.(codlft199) ][,list(codlft199,var,value)]
    rm(d_wat)
    setkey(waterfall_proflor, codlft199, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o]
    setkey(waterfall, codlft199,o)
    
    waterfall[,end:=cumsum(value), keyby=.(codlft199)]
    waterfall[,start:=end-value, keyby=.(codlft199)]
    
    # aggiungo yoy
    waterfall[,yoy:=""]
    yoy_temp=yoy[,list(yoy_value=sum(yoy_value)), keyby=.(codlft199,var)]
    setkey(waterfall, codlft199,var)
    waterfall[yoy_temp, yoy:=paste0( ifelse(value/yoy_value<1,"-","+") , abs(round(value/yoy_value-1,0)), "%")]
    move_rect=waterfall[var=='ricavi',list(moveup=value/3), keyby=codlft199]
    waterfall[move_rect,(c('end','start')):= list( end+moveup , start+moveup) ]
    setkey(waterfall, o)
    
    waterfall
    
  }
  
})

# waterfall per gear ####
d_waterfall_gear=reactive({
  
  if( ok_rows() ){
    
    d_wat = 
      if(input_apply_weights()==TRUE) 
        d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(codsis199,var,value=value/pr_i) ]
    else  
      d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(codsis199,var,value) ]
    
    waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'), codsis199=d_panel()[,unique(codsis199)],value=as.numeric(0) )
    setkey(waterfall, codsis199, var)
    
    waterfall_cost=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(codsis199,var) ]
    setkey(waterfall_cost, codsis199, var)
    
    waterfall_proflor=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor'), keyby=.(codsis199) ][,list(codsis199,var,value)]
    rm(d_wat)
    setkey(waterfall_proflor, codsis199, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o]
    setkey(waterfall, codsis199,o)
    
    waterfall[,end:=cumsum(value), keyby=.(codsis199)]
    waterfall[,start:=end-value, keyby=.(codsis199)]
    
    # aggiungo yoy
    waterfall[,yoy:=""]
    yoy_temp=yoy[,list(yoy_value=sum(yoy_value)), keyby=.(codsis199,var)]
    setkey(waterfall, codsis199,var)
    waterfall[yoy_temp, yoy:=paste0( ifelse(value/yoy_value<1,"-","+") , abs(round(value/yoy_value-1,0)), "%")]
    move_rect=waterfall[var=='ricavi',list(moveup=value/3), keyby=codsis199]
    waterfall[move_rect,(c('end','start')):= list( end+moveup , start+moveup) ]
    setkey(waterfall, o)
    
    waterfall
    
  }
  
})

# waterfall per gear,loa ####
d_waterfall_gear_loa=reactive({
  
  if( ok_rows() ){
    
    d_wat = 
      if(input_apply_weights()==TRUE) 
        d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(codsis199,codlft199,var,value=value/pr_i) ]
    else  
      d_panel()[var %in% c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro') , list(codsis199,codlft199,var,value) ]
    
    waterfall=d_panel()[,CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'), value=as.numeric(0) ),list(codsis199,codlft199)]
    setkey(waterfall, codsis199,codlft199, var)
    
    waterfall_cost=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(codsis199,codlft199,var) ]
    setkey(waterfall_cost, codsis199,codlft199, var)
    
    waterfall_proflor=d_wat[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor'), keyby=.(codsis199,codlft199) ][,list(codsis199,codlft199,var,value)]
    rm(d_wat)
    setkey(waterfall_proflor, codsis199,codlft199, var)
    
    waterfall[waterfall_cost,value:=i.value]
    waterfall[waterfall_proflor,value:=i.value]
    
    o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
    setkey(waterfall,var)
    waterfall=waterfall[o]
    setkey(waterfall, codsis199,codlft199,o)
    
    waterfall[,end:=cumsum(value), keyby=.(codsis199,codlft199)]
    waterfall[,start:=end-value, keyby=.(codsis199,codlft199)]
    
    # aggiungo yoy
    waterfall[,yoy:=""]
    yoy_temp=yoy[,list(yoy_value=sum(yoy_value)), keyby=.(codsis199,codlft199,var)]
    setkey(waterfall, codsis199,codlft199,var)
    waterfall[yoy_temp, yoy:=paste0( ifelse(value/yoy_value<1,"-","+") , abs(round(value/yoy_value-1,0)), "%")]
    move_rect=waterfall[var=='ricavi',list(moveup=value/3), keyby=.(codsis199,codlft199)]
    waterfall[move_rect,(c('end','start')):= list( end+moveup , start+moveup) ]
    setkey(waterfall, o)
    
    waterfall
    
  }
  
})


