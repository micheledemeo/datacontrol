
waterfall=CJ(var=c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor'), id_strato=d[,unique(id_strato)],value=as.numeric(0) )
setkey(waterfall, id_strato, var)

waterfall_cost=d[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum( value)) ), by=.(id_strato,var) ]
setkey(waterfall_cost, id_strato, var)

waterfall_proflor=d[var %in%  c('ricavi','carbur','alcova','spcom','spmanu','alcofi','lavoro'), list(value=as.numeric(sum(  ifelse(var=='ricavi',value,-value)  )), var='proflor'), keyby=.(id_strato) ][,list(id_strato,var,value)]
setkey(waterfall_proflor, id_strato, var)

waterfall[waterfall_cost,value:=i.value]
waterfall[waterfall_proflor,value:=i.value]

o=data.table(var=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi'), o=c(1,2,3,4,5,6,7,8), key="var")
setkey(waterfall,var)
waterfall=waterfall[o]
setkey(waterfall, id_strato,o)

waterfall[,end:=cumsum(value), keyby=.(id_strato)]
waterfall[,start:=end-value, keyby=.(id_strato)]

waterfall[,ggplot(.SD, aes(var, fill=var)) +  geom_rect(aes(x = var, ymin = end, ymax = start, xmin=o-.45, xmax=o+.45)) + scale_x_discrete(limits=c('carbur','alcova','spcom','spmanu','alcofi','lavoro','proflor','ricavi')) +geom_text(aes(o, end, label=format(value,big.mark = "."))) +facet_wrap(~id_strato,scales = "free") +  theme(axis.ticks = element_blank(), axis.text.y = element_blank())  + ylab("") ]



