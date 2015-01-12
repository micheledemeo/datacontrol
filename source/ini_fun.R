# inizialization for wd and ibraries
require(shiny)
require(data.table)
require(reshape2)
require(ggplot2)

# startup functions ####
fread_mysql = function( tbname, dbname="nisea" , user="nisea", psw="n1s34", csvname=strftime(Sys.time(),"%Y%m%d%H%M%S"), tempdir=getwd() ,mysqldir="C:/wamp/bin/mysql/mysql5.6.17/bin" ) {
  
  require(data.table)
  csvdir=paste(tempdir,csvname,sep = "/")
  csvdir_names=paste(tempdir,paste0(csvname,"_names"),sep = "/")
  #ifnull(inail_infortuni,'')
  
  if (file.exists(csvdir_names)) file.remove(csvdir_names)
  strsql=paste0("select * from  (SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = '",dbname,"' AND TABLE_NAME = '",tbname,"') as t1 INTO OUTFILE '",csvdir_names,"'")
  strbatch= paste0("cd ", mysqldir, " && ", "mysql -u ",user," --password=",psw," ",dbname, " -e ", '"', strsql,'"')
  shell(strbatch,intern = T)
  dnames=fread(csvdir_names,header = F)
  
  if (file.exists(csvdir)) file.remove(csvdir)
  strsql=paste0("select ", paste0("ifnull(",dnames$V1,",'')",collapse = ",") , " from ",tbname," INTO OUTFILE '",csvdir,"'")
  strbatch= paste0("cd ", mysqldir, " && ", "mysql -u ",user," --password=",psw," ",dbname, " -e ", '"', strsql,'"')
  shell(strbatch,intern = T)
  
  d=fread(csvdir,header = F)
  setnames(d, names(d), dnames$V1 )
  
  file.remove(csvdir)
  file.remove(csvdir_names)
  
  return(d)
  
}

pastedir=function(...) paste(..., sep="/")