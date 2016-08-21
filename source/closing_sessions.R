closed_strata=reactive({
  cl=hist[closing_session=="closed",.N,keyby=list(id_battello)][,N:=NULL]
  all[ id_battello %in% cl[,id_battello] ,.N,keyby=list(id_strato)][,id_strato]
})

# UI for closing sessions
output$strato_close=renderUI({ selectInput("strato_close", label = "Close strata:", choices = strato, selected = NULL, multiple = T ) })
output$strato_open=renderUI({ selectInput("strato_open", label = "Open strata and/or reset imputations:", choices = strato, selected = NULL, multiple = T ) })

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
  out[ ,is_closed:=as.character(NA)]
  out[.(closed_strata()),nomatch=0 ,is_closed:="YES"]
  out[is.na(is_closed),is_closed:="NO"]
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

observe({

  if (input$close_open_strata==T & ( length(input_strato_open())>0 | length(input_strato_close())>0 ) ) {

    withProgress(message = "Uploadig data to remote server:",{
      n=20
      up=all[id_strato %in% input_strato_close(),.(var,id_strato,id_battello,value_ok,value_or,parameter_ok,parameter_or,notes)][!id_strato %in% input_strato_open()]
      hist2=hist[!id_battello %in% strato_battello[id_strato %in% c(input_strato_close(),input_strato_open()),id_battello] ][,id:=NULL]

      if(nrow(up)>0) {
        setkey(up,id_battello)
        up=pr_i[up]
        up[is.na(pr_i), pr_i:=0] # qui non metto inf perché poi salvo nel db

        # refresh di pr_i
        if( input_not_sent_as_0()==1 ) {
          source( paste(getwd(), "source/refresh_pr_i_closing_sessions.R", sep="/"),loc=T )
          setkey(up, id_battello)
          up[pr_i_temp, pr_i:=i.pr_i, nomatch=0]
        }

        up[,input_exp_data_in_dt:=input_exp_data()]
        up=up[,list(id_battello,var,day=Sys.Date(),year=year_local,pr_i=round(pr_i,8),
                    hist_value=ifelse(input_exp_data_in_dt=='orig', value_or , value_ok  ),
                    hist_parameter=ifelse(input_exp_data_in_dt=='orig',parameter_or, parameter_ok ),
                    hist_notes=ifelse( is.na(notes)|notes=="" , paste(session_info,"no change in closing",sep="|") , notes),
                    closing_session="closed")
              ]


      } else { # riaperti tutti i closed, quindi nulla per upload. Caso raro, probabile all'inizio dei controlli quando ci sono pochi dati

        up=data.table(id_battello=0,var="control_var",day=Sys.Date(),year=year_local,pr_i=0,hist_value=0,hist_parameter=0,
                    hist_notes="control record", closing_session="open")

      }

      if(nrow(hist2)>0) up=rbindlist(list( up,hist2 ))

      write.table(up, paste0(temp_dir_nicoda,"\\nicoda",year_local,".csv"), sep=";", quote = FALSE, na = "", row.names = F,col.names = F)
      ftp(action="put",filename = "nicodaYYYY.csv",year_local = year_local)
      pid=data.table(system("tasklist /V",intern = T))[grepl("127.0.0.1:12345",V1),V1]
      pid=regmatches(pid, regexpr("\\d+(?=\\s*Console)",pid,perl=T))
      for (i in 1:n) {
        incProgress(1/n, detail =  sample(9000:70000,1) )
        Sys.sleep(.1)
      }
    }) # withProgress

    system(paste("taskkill /pid",pid),wait = T)
    Sys.sleep(2)
    browseURL("http://127.0.0.1:12345/")

  }

})

observe ({
  input$headtab
  updateCheckboxInput(session, 'close_open_strata', value = F)
})



