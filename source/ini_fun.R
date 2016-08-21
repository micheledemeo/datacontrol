# redundant: # crea nicoda temp dir
temp_dir_nicoda=paste0(Sys.getenv("LOCALAPPDATA"),"\\Nicoda")
unlink(temp_dir_nicoda, recursive = T, force = T)
dir.create(temp_dir_nicoda)

wd=getwd()

# inizialization for wd and libraries
session_info=paste(Sys.info()['nodename'], Sys.info()['user'], strftime(Sys.time(),"%Y-%m-%d-%X"), sep="|")
require(shiny)
require(data.table)
require(reshape2)
require(ggplot2)

sum2=function(...,na.rm=T) sum(...,na.rm=na.rm)

# startup functions ####
fread_mysql = function( tbname , dbname="nisea" , user="nisea", psw="n1s34", csvname=strftime(Sys.time(),"%Y%m%d%H%M%S"), temp_dir=temp_dir_nicoda ,mysqldir="C:/wamp/bin/mysql/mysql5.6.17/bin",integer64=getOption("datatable.integer64")  ) {
  
  require(data.table)
  temp_dir=gsub("\\","/",temp_dir,fixed = T)
  csvdir=paste(temp_dir,csvname,sep = "/")
  csvdir_names=paste(temp_dir,paste0(csvname,"_names"),sep = "/")
  
  if (file.exists(csvdir_names)) file.remove(csvdir_names)
  strsql=paste0("select * from  (SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = '",dbname,"' AND TABLE_NAME = '",tbname,"') as t1 INTO OUTFILE '",csvdir_names,"'")
  strbatch= paste0("cd ", mysqldir, " && ", "mysql -u ",user," --password=",psw," ",dbname, " -e ", '"', strsql,'"')
  shell(strbatch,intern = T)
  dnames=fread(csvdir_names,header = F)
  
  if (file.exists(csvdir)) file.remove(csvdir)
  strsql=paste0("select ", paste0("ifnull(",dnames$V1,",'')",collapse = ",") , " from ",tbname," INTO OUTFILE '",csvdir,"'")
  strbatch= paste0("cd ", mysqldir, " && ", "mysql -u ",user," --password=",psw," ",dbname, " -e ", '"', strsql,'"')
  shell(strbatch,intern = T)
 
  if (length( readLines(csvdir,n = 1) )>0) {
    d=fread(csvdir,header = F, integer64=integer64,sep="\t")
    setnames(d, names(d), dnames$V1 )
    
  } else {
    d=data.table()[0]
  }
 
  return(d)
  
}

pastedir=function(...) paste(..., sep="/")
write.table.ok =  function(...) write.table(... , sep=";", row.names=FALSE,na="",quote=F)

source("C:/Program Files/R/R-3.1.1/_nisea/hvServer.R")

ftp=function(action="get",filename="nicodaYYYY.csv",year_local="",host="ftp.rilevazionecosti.org", user="niseabackup",psw="n1s3454t",temp_dir=temp_dir_nicoda) {
  
  if(action!="get") action="put"
  
  if (year_local!="") filename = gsub("YYYY",year_local,filename)
  
  # crea .bat
  writeLines( 
    c(paste0("cd ", temp_dir), 
      "ftp -s:ftp.txt"
    ), 
    con=paste0(temp_dir,"\\nicoda_hist.bat")  
  )
  
  # crea ftp.txt
  writeLines( 
    c(paste("open", host),
      user,
      psw,
      if (year_local!="") paste("cd",year_local),
      paste(action, filename),
      "bye"
    ),
    con=paste0(temp_dir,"\\ftp.txt")
  )
  
  # esegui bat
  system2(paste0(temp_dir,"\\nicoda_hist.bat"))
}

year_local=fread(pastedir(wd,"source/year"))$V1