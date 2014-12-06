# data management
pastedir=function(...) paste(..., sep="/")
library(data.table)
library(ggplot2)
pri=fread(pastedir(getwd(),"input/pri.txt") )
camp=fread(pastedir(getwd(),"input/campionari.txt") )

setkey(camp, batcod)
setkey(pri, batcod)

camp=pri[,.(batcod, strato)][camp]

camp[cod_variable!='proflor' & strato==6, ggplot(.SD, aes(x=cod_variable, y= values, fill=cod_variable)) + geom_boxplot() + guides(fill=FALSE), .SDcols=c('cod_variable', 'values') ]

