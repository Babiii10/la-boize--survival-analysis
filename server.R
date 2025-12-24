options(shiny.maxRequestSize=60*1024^2) 
source("global.R")
#options(xtable.include.colnames=T)
#options(xtable.include.rownames=T)

shinyServer(function(input, output,session) {
  #if(requireNamespace("superml", quietly = TRUE)) {
    #attachNamespace("superml")
  #}
  # output$theme_value = reactive({
  #   if (is.null(input$theme) || input$theme == "default") {
  #     return ("quartz")
  #   } else {
  #     return(input$theme)
  #   }
  # }
  
  # observe le switch / bouton
  # observeEvent(input$mode, {
  #   # si mode = "dark", on applique un thème sombre
  #   # sinon thème clair de base
  #   new_th <- if (input$mode == "dark") {
  #     bs_theme(bootswatch = "darkly")
  #   } else {
  #     bs_theme()  # thème par défaut (clair)
  #   }
  #   session$setCurrentTheme(new_th)
  # })
  
  #changer le theme 
  # observeEvent(input$theme_app, {
  #           session$setCurrentTheme(bs_theme(bootswatch = input$theme_app))
  # }, ignoreInit = TRUE)
  
  output$modelUploaded <- reactive({
    return(!is.null(input$modelfile))
  })
  outputOptions(output, 'modelUploaded', suspendWhenHidden=FALSE)
  
  output$fileUploaded <- reactive({
    return(!is.null(input$learningfile))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$image1<-renderImage({return (list(src="pictures/Logo I2MC.jpg", 
                                           contentType="image/jpeg",
                                           width=300,
                                           height=200,
                                           alt="I2MC logo"))},deleteFile = F)
  output$image2<-renderImage({return (list(src="pictures/rflabxx.png", 
                                           contentType="image/png",
                                           width=600,
                                           height=200,
                                           alt="RFlab logo"))},deleteFile = F)
  output$image3<-renderImage({return (list(src="pictures/structurdata2.jpg", 
                                           contentType="image/jpeg",
                                           width=600,
                                           height=300,
                                           alt="structure data"))},deleteFile = F)

  output$fileUploadedval <- reactive({
    return( !is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'fileUploadedval', suspendWhenHidden=FALSE)

  output$modelUploadedval <- reactive({
    return(!is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'modelUploadedval', suspendWhenHidden=FALSE)

  # Column selection logic - read columns from uploaded file before confirmation
  available_columns <- reactive({
    if(is.null(input$learningfile)) return(NULL)

    # Read file temporarily to get column names
    importparameters_temp <- list(
      "learningfile" = input$learningfile,
      "validationfile" = NULL,
      "modelfile" = NULL,
      "extension" = input$filetype,
      "NAstring" = input$NAstring,
      "sheetn" = input$sheetn,
      "skipn" = input$skipn,
      "dec" = input$dec,
      "sep" = input$sep,
      "transpose" = input$transpose,
      "zeroegalNA" = input$zeroegalNA,
      "confirmdatabutton" = 0,  # Not confirmed yet
      "invers" = FALSE
    )

    tryCatch({
      temp_data <- importfunction(importparameters_temp)
      if(!is.null(temp_data$learning)){
        return(colnames(temp_data$learning))
      }
    }, error = function(e) NULL)

    return(NULL)
  })

  output$columnsAvailable <- reactive({
    !is.null(available_columns()) && length(available_columns()) >= 2
  })
  outputOptions(output, 'columnsAvailable', suspendWhenHidden=FALSE)

  # Dynamic UI for column selectors
  output$time_col_selector <- renderUI({
    cols <- available_columns()
    if(is.null(cols)) return(NULL)

    # Try to auto-detect time column
    default_time <- NULL
    time_patterns <- c("time", "temps", "duration", "duree", "survival", "survie", "days", "jours", "months", "mois")
    for(pattern in time_patterns){
      matches <- grep(pattern, cols, ignore.case = TRUE)
      if(length(matches) > 0){
        default_time <- cols[matches[1]]
        break
      }
    }
    if(is.null(default_time) && length(cols) >= 1) default_time <- cols[1]

    selectInput("time_column", "Time Column:",
                choices = cols,
                selected = default_time)
  })

  output$status_col_selector <- renderUI({
    cols <- available_columns()
    if(is.null(cols)) return(NULL)

    # Try to auto-detect status column
    default_status <- NULL
    status_patterns <- c("status", "statut", "event", "evenement", "censor", "censure", "death", "mort", "deceased", "decede")
    for(pattern in status_patterns){
      matches <- grep(pattern, cols, ignore.case = TRUE)
      if(length(matches) > 0){
        default_status <- cols[matches[1]]
        break
      }
    }
    if(is.null(default_status) && length(cols) >= 2) default_status <- cols[2]

    selectInput("status_column", "Status Column:",
                choices = cols,
                selected = default_status)
  })

  output$id_col_selector <- renderUI({
    cols <- available_columns()
    if(is.null(cols)) return(NULL)

    # Try to auto-detect ID column
    default_id <- "None (use row names)"
    id_patterns <- c("id", "patient", "subject", "sujet", "sample", "echantillon", "individu", "individual")
    for(pattern in id_patterns){
      matches <- grep(pattern, cols, ignore.case = TRUE)
      if(length(matches) > 0){
        default_id <- cols[matches[1]]
        break
      }
    }

    selectInput("id_column", "ID Column (optional):",
                choices = c("None (use row names)", cols),
                selected = default_id)
  })
  
#Save state#############  
  state <- reactiveValues()
  observe({
    importparameters<<-list("learningfile"=input$learningfile,
                            "validationfile"=input$validationfile,
                            "modelfile"=input$modelfile,
                            "extension" = input$filetype,
                            "NAstring"=input$NAstring,
                            "sheetn"=input$sheetn,
                            "skipn"=input$skipn,
                            "dec"=input$dec,
                            "sep"=input$sep,
                            "transpose"=input$transpose,
                            "zeroegalNA"=input$zeroegalNA,
                            confirmdatabutton=input$confirmdatabutton)
    
    selectdataparameters<<-list("prctvalues"=input$prctvalues,
                                "selectmethod"=input$selectmethod,
                                "NAstructure"=input$NAstructure,
                                "structdata"=input$structdata,
                                "thresholdNAstructure"=input$thresholdNAstructure,
                                "maxvaluesgroupmin"=input$maxvaluesgroupmin,
                                "minvaluesgroupmax"=input$minvaluesgroupmax)
    
    transformdataparameters<<-list("log"=input$log,
                                   "logtype"=input$logtype,
                                   "standardization"=input$standardization,
                                   "arcsin"=input$arcsin,
                                   "rempNA"=input$rempNA)
    
    
    testparameters<<-list("SFtest"=input$SFtest,"test"=input$test,"adjustpv"=input$adjustpv,"thresholdpv"=input$thresholdpv,"thresholdFC"=input$thresholdFC)
    
    modelparameters<<-list("modeltype"=input$model,"invers"=input$invers,"thresholdmodel"=input$thresholdmodel,
                           "fs"=input$fs,"adjustval"=input$adjustval)
    parameters<-list("importparameters"=importparameters,"selectdataparameters"=selectdataparameters,
                     "transformdataparameters"=transformdataparameters,"testparameters"=testparameters,"modelparameters"=modelparameters)
    data<-DATA()
    selectdata<-SELECTDATA()
    transformdata<-TRANSFORMDATA()
    test<-TEST()
    model<-MODEL()
    settingstable<-statetable()
    isolate(state<<-list("parameters"=parameters,"data"=data,"selectdata"=selectdata,"transformdata"=transformdata,"test"=test,"model"=model,"settingstable"=settingstable)) 
  })
  
  output$savestate <- downloadHandler(
    filename <- function(){
      paste("model.RData")
    },
    content = function(file) { 
      save(state, file = file)
    }
  )
  observe({
    if(input$confirmdatabutton!=0 & !is.null(input$modelfile)){
      print("update")
      dataaaa<<-DATA()
      updateNumericInput(session, "prctvalues", value = DATA()$previousparameters$selectdataparameters$prctvalues)
      updateRadioButtons(session,"selectmethod",selected =  DATA()$previousparameters$selectdataparameters$selectmethod)
      updateCheckboxInput(session ,"NAstructure",value=DATA()$previousparameters$selectdataparameters$NAstructure)
      updateRadioButtons(session,"structdata",selected=DATA()$previousparametersselectdataparameters$parameters$structdata)
      updateNumericInput(session, "maxvaluesgroupmin", value = DATA()$previousparametersselectdataparameters$parameters$maxvaluesgroupmin)
      updateNumericInput(session, "minvaluesgroupmax", value = DATA()$previousparametersselectdataparameters$parameters$minvaluesgroupmax)
      updateNumericInput(session, "thresholdNAstructure", value = DATA()$previousparameters$selectdataparameters$thresholdNAstructure)
      
      updateRadioButtons(session,"rempNA",selected=DATA()$previousparameters$transformdataparameters$rempNA)
      updateCheckboxInput(session ,"log",value=DATA()$previousparameters$transformdataparameters$log)
      updateRadioButtons(session ,"logtype",selected=DATA()$previousparameters$transformdataparameters$logtype)
      updateCheckboxInput(session ,"standardization",value=DATA()$previousparameters$transformdataparameters$standardization)
      updateCheckboxInput(session ,"arcsin",value=DATA()$previousparameters$transformdataparameters$arcsin)
      
      #updateRadioButtons(session,"test",selected=DATA()$previousparameters$testparameters$test)
      #updateNumericInput(session, "thresholdFC", value = DATA()$previousparameters$testparameters$parameters$thresholdFC)
      #updateNumericInput(session, "thresholdpv", value = DATA()$previousparameters$testparameters$parameters$thresholdpv)
      #updateCheckboxInput(session ,"adjustpval",value=DATA()$previousparameters$testparameters$parameters$adjustpval)
      #updateCheckboxInput(session ,"SFtest",value=DATA()$previousparameters$testparameters$parameters$SFtest)

      updateRadioButtons(session,"test",selected=DATA()$previousparameters$testparameters$test)
      updateNumericInput(session, "thresholdFC", value = DATA()$previousparameters$testparameters$thresholdFC)
      updateNumericInput(session, "thresholdpv", value = DATA()$previousparameters$testparameters$thresholdpv)
      updateCheckboxInput(session ,"adjustpv",value=DATA()$previousparameters$testparameters$adjustpv)
      updateCheckboxInput(session ,"SFtest",value=DATA()$previousparameters$testparameters$SFtest)
      
      updateRadioButtons(session,"model",selected=DATA()$previousparameters$modelparameters$modeltype)
      updateNumericInput(session, "thresholdmodel", value = DATA()$previousparameters$modelparameters$thresholdmodel)
      updateCheckboxInput(session ,"fs",value=DATA()$previousparameters$modelparameters$fs)

      updateCheckboxInput(session ,"adjustval",value=DATA()$previousparameters$modelparameters$adjustval)
      updateCheckboxInput(session ,"invers",value=DATA()$previousparameters$modelparameters$invers)
      
    }
  })
  
  statetable<-reactive({
    table <- matrix(data = "",nrow = 20,ncol=11)
    if((input$confirmdatabutton!=0 & !is.null(input$modelfile))){
      learningfile <- DATA()$previousparameters$importparameters$learningfile
    }
    else{learningfile<-input$learningfile}

    table[1,1:9]<-c("#","Extensionfile","decimal character","separator character","NA string","sheet number","skip lines","consider NA as 0","transpose")
    table[2,1:9]<-c("import parameters",learningfile$type,input$dec,input$sep,input$NAstring,
                         input$sheetn,input$skipn,input$zeroegalNA,input$transpose)

    # For survival analysis: show events and censored counts instead of class levels
    table[3,]<-c("#","name learning file", "number of rows", "number of columns", "number of events",
             "number of censored","name validation file", "number of rows", "number of columns", "number of events",
             "number of censored")

    # Calculate events (status=1) and censored (status=0) for learning and validation sets
    n_events_learn <- if("status" %in% colnames(DATA()$LEARNING)) sum(DATA()$LEARNING$status == 1, na.rm = TRUE) else "N/A"
    n_censored_learn <- if("status" %in% colnames(DATA()$LEARNING)) sum(DATA()$LEARNING$status == 0, na.rm = TRUE) else "N/A"

    n_events_val <- if(!is.null(DATA()$VALIDATION) && "status" %in% colnames(DATA()$VALIDATION)) sum(DATA()$VALIDATION$status == 1, na.rm = TRUE) else "N/A"
    n_censored_val <- if(!is.null(DATA()$VALIDATION) && "status" %in% colnames(DATA()$VALIDATION)) sum(DATA()$VALIDATION$status == 0, na.rm = TRUE) else "N/A"

    table[4,]<-c("main results",learningfile$name,dim(DATA()$LEARNING)[1],dim(DATA()$LEARNING)[2],
                 n_events_learn, n_censored_learn,
                 nll(input$validationfile$name),nll(dim(DATA()$VALIDATION)[1]),
                 nll(dim(DATA()$VALIDATION)[2]),n_events_val, n_censored_val)
    table[5,1:8]<-c("#","percentage of values minimum","method of selection","select features structured","search structur in",
                     "threshold p-value of proportion test", "maximum % values of the min group","minimum % values of the max group")
    table[6,1:8]<-c("select parameters",selectdataparameters[[1]],selectdataparameters[[2]],selectdataparameters[[3]],
                    selectdataparameters[[4]],selectdataparameters[[5]],selectdataparameters[[6]],selectdataparameters[[7]])
    table[7,1:3]<-c("#","number of feature selected","number of feature structured")
    table[8,1:3]<-c("main results",dim(SELECTDATA()$LEARNINGSELECT)[2]-1,nll(dim(SELECTDATA()$STRUCTUREDFEATURES)[2]))
    table[9,1:5]<-c("#","remplace NA by","transformation log","strandardisation","arcsin transformation")
    if(transformdataparameters[[1]]=="FALSE"){logprint<-"FALSE"}
    else{logprint<-transformdataparameters[[2]]}
    table[10,1:5]<-c("transform parameters",transformdataparameters[[5]],logprint,transformdataparameters[[3]],transformdataparameters[[4]])
    table[11,1]<-c("#")
    table[12,1]<-c("main results")
    table[13,1:5]<-c("#","test","use Bonferroni adjustment","threshold of significativity","Fold change threshold")
    table[14,1:5]<-c("test parameters",input$test,input$adjustpv,input$thresholdpv,input$thresholdFC)
    table[15,1:2]<-c("#","number of differently expressed features")
    table[16,1:2]<-c("main results",dim(TEST()$LEARNINGDIFF)[2]-1)

    if(input$model!="nomodel"){
      table[17,1:6]<-c("#","model type","cut-off of the model","feature selection","apply model on validation","invers groups")
      table[18,1:6]<-c("model parameters",input$model,input$thresholdmodel,
                       input$fs,input$adjustval,input$invers)
      
      # table[17,1:6]<-c("#","model type","cut-off of the model (Youden)","feature selection",
      #                  "apply model on validation","invers groups")
      
      cat('MODEL()$modelparameters$thresholdmodel \n')
      print(MODEL()$modelparameters$thresholdmodel)
      # table[18,1:6]<-c("model parameters",input$model,
      #                  round(MODEL()$modelparameters$thresholdmodel, 3),
      #                  input$fs,input$adjustval,input$invers)
      
      # For survival analysis: use C-index and IBS instead of AUC/sensitivity/specificity
      table[19,1:6]<-c("#","number of features","C-index learning","IBS learning","C-index validation","IBS validation")

      # Extract survival metrics from MODEL() - they should be calculated during model building
      table[20,1:3]<-c("main results",
                       dim(MODEL()$DATALEARNINGMODEL$learningmodel)[2]-1,
                       round(MODEL()$DATALEARNINGMODEL$reslearningmodel$cindex_learning, digits = 3))

      # Add IBS if available
      if(!is.null(MODEL()$DATALEARNINGMODEL$reslearningmodel$ibs_learning)){
        table[20,4]<-round(MODEL()$DATALEARNINGMODEL$reslearningmodel$ibs_learning, digits = 3)
      } else {
        table[20,4]<-"N/A"
      }

      if(input$adjustval){
        table[20,5]<-round(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$cindex_validation, digits = 3)
        if(!is.null(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$ibs_validation)){
          table[20,6]<-round(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$ibs_validation, digits = 3)
        } else {
          table[20,6]<-"N/A"
        }
      }
    }
    return(table)
    
  }) 
  
  output$savestatetable<- downloadHandler(
    filename = function() { paste('settingstable', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   statetable(), file,cnames=F,rnames=F) })
  
############
  output$namefilelearn<-renderText({
    namelearn<-input$learningfile$name
  })
  output$dim1learn<-renderText({
    di1<-dim(x = DATA()$LEARNING)[1]  
  })
  output$dim2learn<-renderText({
    di2<-dim(x = DATA()$LEARNING)[2]-1  
  })
  output$namefileval<-renderText({
    nameval<-input$validationfile$name
  })  
  output$dim1val<-renderText({
    di1<-dim(x = DATA()$VALIDATION)[1]  
  })
  output$dim2val<-renderText({
    di2<-dim(x = DATA()$VALIDATION)[2]
  })

  # Display status levels for event and censored
  output$event<-renderText({
    if(!is.null(DATA()$LEARNING) && "status" %in% colnames(DATA()$LEARNING)){
      unique_status <- sort(unique(DATA()$LEARNING[,"status"]))
      if(length(unique_status) >= 1){
        if(input$invers){
          return(as.character(unique_status[2]))
        } else {
          return(as.character(unique_status[1]))
        }
      }
    }
    return("")
  })

  output$censored<-renderText({
    if(!is.null(DATA()$LEARNING) && "status" %in% colnames(DATA()$LEARNING)){
      unique_status <- sort(unique(DATA()$LEARNING[,"status"]))
      if(length(unique_status) >= 2){
        if(input$invers){
          return(as.character(unique_status[1]))
        } else {
          return(as.character(unique_status[2]))
        }
      }
    }
    return("")
  })

  # Display class distribution summary
  output$class_summary<-renderText({
    if(!is.null(DATA()$LEARNING) && "status" %in% colnames(DATA()$LEARNING)){
      status_counts <- table(DATA()$LEARNING[,"status"])
      if(length(status_counts) == 2){
        event_count <- sum(DATA()$LEARNING[,"status"] == 1, na.rm = TRUE)
        censored_count <- sum(DATA()$LEARNING[,"status"] == 0, na.rm = TRUE)
        total <- event_count + censored_count
        return(paste0("Events: ", event_count, " (", round(100*event_count/total, 1), "%) | ",
                     "Censored: ", censored_count, " (", round(100*censored_count/total, 1), "%)"))
      }
    }
    return("")
  })

  #si erreur envoyÃÂÃÂ© pb import
  DATA<-reactive({
     # Require that either a learning file or a model file is uploaded before proceeding


     importparameters<<-list("learningfile"=input$learningfile,"validationfile"=input$validationfile,"modelfile"=input$modelfile,"extension" = input$filetype,
                            "NAstring"=input$NAstring,"sheetn"=input$sheetn,"skipn"=input$skipn,"dec"=input$dec,"sep"=input$sep,
                            "transpose"=input$transpose,"zeroegalNA"=input$zeroegalNA,confirmdatabutton=input$confirmdatabutton,invers=input$invers,
                            "time_column"=input$time_column,"status_column"=input$status_column,"id_column"=input$id_column)

     out<-tryCatch(importfunction(importparameters),error=function(e) e )
#      if(any(class(out)=="error"))print("error")
#      else{resimport<-out}
     validate(need(any(class(out)!="error"),"error import"))
     resimport<<-out
      #resimport<-importfunction(importparameters)
    list(LEARNING=resimport$learning,
         VALIDATION=resimport$validation,
        previousparameters=resimport$previousparameters
#          LEVELS=resimport$lev
         )
  })
  
  output$JDDlearn=renderDataTable({
    learning<-DATA()$LEARNING
    validate(need(!is.null(learning),"problem import"))
    colmin<-min(ncol(learning),100)
    rowmin<-min(nrow(learning),100)
    cbind(Names=rownames(learning[1:rowmin,1:colmin]),learning[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10))
  
  output$downloaddataJDDlearn <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$LEARNING, file) })
  
  
  output$JDDval=renderDataTable({
    validation<-DATA()$VALIDATION
    validate(need(!is.null(validation),"problem import"))
    colmin<-min(ncol(validation),100)
    rowmin<-min(nrow(validation),100)
    cbind(Names=rownames(validation[1:rowmin,1:colmin]),validation[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10)) 
  
  output$downloaddataJDDval <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$VALIDATION, file) })


#################
SELECTDATA<-reactive({
  selectdataparameters<<-list("prctvalues"=input$prctvalues,"selectmethod"=input$selectmethod,"NAstructure"=input$NAstructure,"structdata"=input$structdata,
                              "thresholdNAstructure"=input$thresholdNAstructure,"maxvaluesgroupmin"=input$maxvaluesgroupmin,"minvaluesgroupmax"=input$minvaluesgroupmax)
  validate(need(selectdataparameters$prctvalues>=0 &selectdataparameters$prctvalues<=100,"%  NA has to be between 0 and 100"))
  validate(need(input$minvaluesgroupmax>=0 &input$minvaluesgroupmax<=100 & input$maxvaluesgroupmin>=0 &input$maxvaluesgroupmin<=100,"% threshold has to be between 0 and 100"),
           need(input$thresholdNAstructure>0,input$thresholdNAstructure<1,"threshold of the pvalue has to be between 0 and 1"))
  learning<<-DATA()$LEARNING
  validate(need(input$confirmdatabutton!=0,"Importation of datas has to be confirmed"))

  # Validate survival data structure (time and status columns)
  validate(
    need("time" %in% colnames(learning), "Data must contain a 'time' column"),
    need("status" %in% colnames(learning), "Data must contain a 'status' column"),
    need(is.numeric(learning$time), "Time column must be numeric"),
    need(all(learning$status %in% c(0, 1, NA)), "Status column must contain only 0 (censored) or 1 (event)")
  )
  resselectdata<<-selectdatafunction(learning = learning,selectdataparameters = selectdataparameters)
  list(LEARNINGSELECT=resselectdata$learningselect,STRUCTUREDFEATURES=resselectdata$structuredfeatures,DATASTRUCTUREDFEATURES=resselectdata$datastructuredfeatures,selectdataparameters)
})
#####
#Selection Output
#####
  output$downloaddataselect<- downloadHandler(
    filename = function() { paste('Dataselect', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(SELECTDATA()$LEARNINGSELECT, file)
    }
  )
  
output$nvarselect=renderText({
    di1<-dim(x = SELECTDATA()$LEARNINGSELECT)[2]-1  
  })
  
output$heatmapNA<-renderPlot({
  learningselect<-SELECTDATA()$LEARNINGSELECT
  heatmapNA(toto =learningselect)
})
output$downloadplotheatmapNA = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot =    heatmapNA(toto =SELECTDATA()$LEARNINGSELECT), 
           device = input$paramdownplot)
  },
  contentType=NA)

output$downloaddataheatmapNA <- downloadHandler(
  filename = function() { paste('dataset distribution of NA', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(as.data.frame(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F)), file)
  }
)

# observe({
#   req(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F))
#   print(class(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F)))
# })

output$plotNA<-renderPlot({
  learningselect<-SELECTDATA()$LEARNINGSELECT
  learning<-DATA()$LEARNING
  distributionvalues(toto = learning,prctvaluesselect =input$prctvalues/100,nvar = ncol(learningselect) ,ggplot =  T)  
})


output$downloadplotNA = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot =         distributionvalues(toto = DATA()$LEARNING,prctvaluesselect =input$prctvalues/100,nvar = ncol(SELECTDATA()$LEARNINGSELECT) ,ggplot =  T), 
           device = input$paramdownplot)},contentType=NA)

output$downloaddataplotNA <- downloadHandler( 
  filename = function() {
    paste('dataset', '.',input$paramdowntable, sep='') 
    },
  content = function(file) {
    downloaddataset(distributionvalues(toto = DATA()$LEARNING,prctvaluesselect =input$prctvalues/100,nvar = ncol(SELECTDATA()$LEARNINGSELECT) ,ggplot =  T,graph = F)  , file)
  }
)

output$nstructuredfeatures<-renderText({
  ncol(SELECTDATA()$STRUCTUREDFEATURES)
})
output$heatmapNAstructure<-renderPlot({
  group<<-DATA()$LEARNING[,1]
  structuredfeatures<<-SELECTDATA()$STRUCTUREDFEATURES
  heatmapNA(toto=cbind(group,structuredfeatures))            
  #else{errorplot(text = " No NA's structure")}
})
  
output$downloadstructur = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot = heatmapNA(cbind(DATA()$LEARNING[,1],SELECTDATA()$STRUCTUREDFEATURES)), 
           device = input$paramdownplot)
  },
  contentType=NA)

output$downloaddatastructur <- downloadHandler( 
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(SELECTDATA()$STRUCTUREDFEATURES, file)
  }
) 
  
#####  
TRANSFORMDATA<-reactive({
  learningselect<<-SELECTDATA()$LEARNINGSELECT
  structuredfeatures<<-SELECTDATA()$STRUCTUREDFEATURES
  datastructuresfeatures<<-SELECTDATA()$DATASTRUCTUREDFEATURES
  transformdataparameters<<-list("log"=input$log,"logtype"=input$logtype,"standardization"=input$standardization,"arcsin"=input$arcsin,"rempNA"=input$rempNA)
  validate(need(ncol(learningselect)>0,"No select dataset"))
  if(transformdataparameters$rempNA%in%c("pca","missforest")){
    validate(need(min(apply(X = learningselect,MARGIN = 2,FUN = function(x){sum(!is.na(x))}))>1,"not enough data for pca estimation"))
  } 
  learningtransform<-transformdatafunction(learningselect = learningselect,structuredfeatures = structuredfeatures,
                                      datastructuresfeatures =   datastructuresfeatures,transformdataparameters = transformdataparameters)

  list(LEARNINGTRANSFORM=learningtransform,transformdataparameters=transformdataparameters)
})

##
output$downloaddatatransform<- downloadHandler(
  filename = function() { paste('Transformdata', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(TRANSFORMDATA()$LEARNINGTRANSFORM, file)
  }
)

output$plotheatmaptransformdata<-renderPlot({
  learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
  heatmapplot(toto =learningtransform,ggplot = T,scale=F)
})

output$downloadplotheatmap = downloadHandler(
  filename = function() { 
    paste0('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot =    heatmapplot(toto =TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot = T,scale=F), 
           device = input$paramdownplot)},
  contentType=NA)

output$downloaddataheatmap <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(as.data.frame(heatmapplot(toto =TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot = T,scale=F,graph=F)), file)
  })

output$plotmds<-renderPlot({
  learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
  mdsplot(toto = learningtransform,ggplot=T)
})
output$downloadplotmds = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =        mdsplot(toto = TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot=T),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatamds <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(    mdsplot(toto = TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot=T,graph=F), file)
  })


output$plothist<-renderPlot({
  learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
  histplot(toto=learningtransform)
})
output$downloadplothist = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =         histplot(toto=TRANSFORMDATA()$LEARNINGTRANSFORM),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatahist <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(histplot(toto=TRANSFORMDATA()$LEARNINGTRANSFORM,graph=F), file)
  })

#########
TEST<-reactive({
  # Get lambda and alpha parameters for multivariate methods
  lambda_param <- NULL
  alpha_param <- 0.5
  if(input$test %in% c("lasso","elasticnet","ridge")){
    if(!input$autolambda){
      lambda_param <- input$lambdaselection
    }
    if(input$test == "elasticnet"){
      alpha_param <- input$alphaselection
    }
  }

  # Get clustering + elasticnet parameters
  n_clusters_param <- NULL
  n_bootstrap_param <- NULL
  min_selection_freq_param <- NULL
  preprocess_param <- NULL
  min_patients_param <- NULL

  if(input$test == "clustEnet"){
    n_clusters_param <- input$nclusters
    n_bootstrap_param <- input$nbootstrap
    alpha_param <- input$alphaclustenet
    min_selection_freq_param <- input$minselectionfreq
    preprocess_param <- input$preprocessclustenet
    min_patients_param <- 20  # Fixed value, could be made configurable if needed
  }

  testparameters<<-list("SFtest"=input$SFtest,"test"=input$test,"adjustpval"=input$adjustpv,"thresholdpv"=input$thresholdpv,
                        "thresholdFC"=input$thresholdFC,"invers"=input$invers,
                        "lambda"=lambda_param,"alpha"=alpha_param,
                        "n_clusters"=n_clusters_param,"n_bootstrap"=n_bootstrap_param,
                        "min_selection_freq"=min_selection_freq_param,
                        "preprocess"=preprocess_param,"min_patients"=min_patients_param)
  learningtransform<<-TRANSFORMDATA()$LEARNINGTRANSFORM
  restest<<-testfunction(tabtransform = learningtransform,testparameters = testparameters )
  validate(need(testparameters$thresholdFC>=0,"threshold Foldchange has to be positive"))
  validate(need(testparameters$thresholdpv>=0 &testparameters$thresholdpv<=1,"p-value has to be between 0 and 1"))

  list(LEARNINGDIFF=restest$tabdiff,DATATEST=restest$datatest,HYPOTHESISTEST=restest$hypothesistest,
       USEDDATA=restest$useddata,testparameters=restest$testparameters,
       MULTIVARIATERESULTS=restest$multivariateresults)

})
##
output$downloadddatadiff<- downloadHandler(
  filename = function() { paste('Datadiff', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(TEST()$LEARNINGDIFF, file)
  }
)
output$downloaddatastatistics<- downloadHandler(
  filename = function() { paste('Datastatistics', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(TEST()$DATATEST, file)
  }
)
output$positif<-renderText({
  res<-levels(DATA()$LEARNING[,1])[1]
})
output$negatif<-renderText({
  res<-levels(DATA()$LEARNING[,1])[2]
})
output$volcanoplot <- renderPlot({
  datatest<<-TEST()$DATATEST
  useddata<<-TEST()$USEDDATA
  colnames(useddata)[3]<-colnames(datatest)[5]
  volcanoplot(logFC =useddata[,3],pval = useddata$pval,thresholdFC = input$thresholdFC,thresholdpv = (input$thresholdpv ),completedata=useddata[,1:3] )
})
output$downloadvolcanoplot = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = volcanoplot(logFC =TEST()$USEDDATA$logFC,pval = TEST()$USEDDATA$pval,thresholdFC = input$thresholdFC,
                                    thresholdpv = input$thresholdpv ,completedata=TEST()$DATATEST ) ,  device = input$paramdownplot)},
  contentType=NA)
output$downloaddatavolcanoplot<- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(volcanoplot(logFC =TEST()$USEDDATA$logFC,pval = TEST()$USEDDATA$pval,thresholdFC = input$thresholdFC,
                                    thresholdpv = (input$thresholdpv ),completedata=TEST()$DATATEST,graph=F ), file) })
output$nvarselect2<-renderText({
  di1<-dim(x = SELECTDATA()$LEARNINGSELECT)[2]-1  
})  
output$nbdiff<-renderText({
  nbdiff = positive(ncol(TEST()$LEARNINGDIFF)-1)
})


output$barplottest <- renderPlot({
  learningdiff<<-TEST()$LEARNINGDIFF
  useddata<<-TEST()$USEDDATA
  if(nrow(learningdiff)!=0){barplottest(feature=useddata$names,logFC=useddata$logFC,levels=levels(learningdiff[,1]),pval=useddata$pval,mean1=useddata$mean1,mean2=useddata$mean2,thresholdpv=input$thresholdpv,
                                             thresholdFC=input$thresholdFC,graph=T,maintitle="Mean by group for differentially expressed variables")
}
  else{errorplot(text = " No differently expressed ")}
  
})
output$downloadbarplottest = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = barplottest(feature=TEST()$USEDDATA$names,logFC=TEST()$USEDDATA$logFC,levels=levels(TEST()$LEARNINGDIFF[,1]),pval=TEST()$USEDDATA$pval,mean1=TEST()$USEDDATA$mean1,mean2=TEST()$USEDDATA$mean2,thresholdpv=input$thresholdpv,
                                    thresholdFC=input$thresholdFC,graph=T,maintitle="Mean by group for differentially expressed variables"),  device = input$paramdownplot)},
  contentType=NA)
output$downloaddatabarplottest <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(barplottest(feature=TEST()$USEDDATA$names,logFC=TEST()$USEDDATA$logFC,levels=levels(TEST()$LEARNINGDIFF[,1]),pval=TEST()$USEDDATA$pval,mean1=TEST()$USEDDATA$mean1,mean2=TEST()$USEDDATA$mean2,thresholdpv=input$thresholdpv,
                                thresholdFC=input$thresholdFC,maintitle="Mean by group for differentially expressed variables",graph=F), file) })

# output$dataconditiontest=renderDataTable({
#   hypothesistest<-TEST()$hypothesistest},options = list("orderClasses" = F,
#                                                         "responsive" = F,
#                                                         "pageLength" = 10))
output$plottestSF=renderPlot({
  hypothesistest<-TEST()$HYPOTHESISTEST   
  barplottestSF(hypothesistest)
})
output$downloadplottestSF = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =   barplottestSF(TEST()$HYPOTHESISTEST  ),  device = input$paramdownplot)},
  contentType=NA)
output$downloaddatatestSF <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(  barplottestSF(TEST()$HYPOTHESISTEST ,graph=F), file) })

# Multivariate selection results outputs
output$nbmultivariateselected<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults)){
    length(multivariateresults$selected_vars)
  } else {
    0
  }
})

output$optimallambda<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$lambda)){
    format(multivariateresults$lambda, scientific = TRUE, digits = 4)
  } else {
    "N/A"
  }
})

output$lambda1se<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$lambda_1se)){
    format(multivariateresults$lambda_1se, scientific = TRUE, digits = 4)
  } else {
    "N/A"
  }
})

output$alphaused<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$alpha)){
    format(multivariateresults$alpha, digits = 3)
  } else {
    "N/A"
  }
})

output$multivariateresultstable<-renderDataTable({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && nrow(multivariateresults$results) > 0){
    results <- multivariateresults$results
    results$coefficient <- round(results$coefficient, 4)
    results$AUC <- round(results$AUC, 3)
    results$FoldChange <- round(results$FoldChange, 3)
    results$logFoldChange <- round(results$logFoldChange, 3)
    results$mean_group1 <- round(results$mean_group1, 3)
    results$mean_group2 <- round(results$mean_group2, 3)
    results
  } else {
    data.frame()
  }
},options = list("orderClasses" = F, "responsive" = F, "pageLength" = 10))

output$downloadmultivariateresults <- downloadHandler(
  filename = function() { paste('multivariate_results', '.',input$paramdowntable, sep='') },
  content = function(file) {
    multivariateresults <- TEST()$MULTIVARIATERESULTS
    if(!is.null(multivariateresults)){
      downloaddataset(multivariateresults$results, file)
    }
  }
)

# Clustering + ElasticNet results outputs
output$nbclustenetselected<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$method) && multivariateresults$method == "clustEnet"){
    length(multivariateresults$selected_vars)
  } else {
    0
  }
})

output$clustenetnclusters<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$clust_result)){
    multivariateresults$clust_result$n_clusters
  } else {
    "N/A"
  }
})

output$clustenetnbootstrap<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$clust_result)){
    multivariateresults$clust_result$n_bootstrap
  } else {
    "N/A"
  }
})

output$clustenetalphaused<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$clust_result)){
    format(multivariateresults$clust_result$alpha, digits = 3)
  } else {
    "N/A"
  }
})

output$clustenetminfreq<-renderText({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$clust_result)){
    format(multivariateresults$clust_result$min_selection_freq, digits = 2)
  } else {
    "N/A"
  }
})

output$clustenetresultstable<-renderDataTable({
  multivariateresults <- TEST()$MULTIVARIATERESULTS
  if(!is.null(multivariateresults) && !is.null(multivariateresults$method) &&
     multivariateresults$method == "clustEnet" && nrow(multivariateresults$results) > 0){
    results <- multivariateresults$results
    results$SelectionFrequency <- round(results$SelectionFrequency, 3)
    results$AUC <- round(results$AUC, 3)
    results$FoldChange <- round(results$FoldChange, 3)
    results$logFoldChange <- round(results$logFoldChange, 3)
    results$mean_group1 <- round(results$mean_group1, 3)
    results$mean_group2 <- round(results$mean_group2, 3)
    results
  } else {
    data.frame()
  }
},options = list("orderClasses" = F, "responsive" = F, "pageLength" = 10))

output$downloadclustenetresults <- downloadHandler(
  filename = function() { paste('clustenet_results', '.',input$paramdowntable, sep='') },
  content = function(file) {
    multivariateresults <- TEST()$MULTIVARIATERESULTS
    if(!is.null(multivariateresults) && multivariateresults$method == "clustEnet"){
      downloaddataset(multivariateresults$results, file)
    }
  }
)


######
MODEL<-reactive({
  if(input$test=="notest"){learningmodel<<-TRANSFORMDATA()$LEARNINGTRANSFORM}
  else{learningmodel<<-TEST()$LEARNINGDIFF}
  validation<<-DATA()$VALIDATION
  datastructuresfeatures<<-SELECTDATA()$DATASTRUCTUREDFEATURES
  transformdataparameters<<-TRANSFORMDATA()$transformdataparameters
  learningselect<-SELECTDATA()$LEARNINGSELECT
  # Get hyperparameters for all models
  alpha_model <- NULL
  lambda_model <- NULL
  ntree_model <- 1000
  autotunerf_param <- TRUE
  mtry_model <- NULL
  autotunesvm_param <- TRUE
  cost_model <- NULL
  gamma_model <- NULL
  kernel_model <- NULL
  autotunexgb_param <- TRUE
  nrounds_model <- NULL
  maxdepth_model <- NULL
  eta_model <- NULL

  # ElasticNet parameters - based on tuning method
  if(input$model == "elasticnet"){
    tuning_method_en <- if(!is.null(input$tuning_method_en)) input$tuning_method_en else "traditional"
    if(tuning_method_en == "manual" || tuning_method_en == "traditional"){
      alpha_model <- input$alphamodel
    }
    if(tuning_method_en == "manual"){
      lambda_model <- input$lambdamodel
    }
  }

  # Random Forest parameters - based on tuning method
  if(input$model == "randomforest"){
    tuning_method_rf <- if(!is.null(input$tuning_method_rf)) input$tuning_method_rf else "traditional"
    ntree_model <- input$ntreerf
    autotunerf_param <- (tuning_method_rf != "manual")
    if(tuning_method_rf == "manual"){
      mtry_model <- input$mtryrf
    }
  }

  # SVM parameters - no change, still using checkbox
  if(input$model == "svm"){
    autotunesvm_param <- input$autotunesvm
    if(!input$autotunesvm){
      cost_model <- input$costsvm
      gamma_model <- input$gammasvm
      kernel_model <- input$kernelsvm
    }
  }

  # XGBoost parameters - based on tuning method
  if(input$model == "xgboost"){
    tuning_method_xgb <- if(!is.null(input$tuning_method_xgb)) input$tuning_method_xgb else "traditional"
    autotunexgb_param <- (tuning_method_xgb != "manual")
    if(tuning_method_xgb == "manual"){
      nrounds_model <- input$nroundsxgb
      maxdepth_model <- input$maxdepthxgb
      eta_model <- input$etaxgb
    }
  }

  # LightGBM parameters - no change
  autotunelgb_param <- TRUE
  nrounds_lgb_model <- NULL
  num_leaves_model <- NULL
  learning_rate_lgb_model <- NULL

  if(input$model == "lightgbm"){
    autotunelgb_param <- input$autotunelgb
    if(!input$autotunelgb){
      nrounds_lgb_model <- input$nroundslgb
      num_leaves_model <- input$numleaves
      learning_rate_lgb_model <- input$learningratelgb
    }
  }

  # KNN parameters - based on tuning method
  autotuneknn_param <- TRUE
  k_neighbors_model <- NULL

  if(input$model == "knn"){
    tuning_method_knn <- if(!is.null(input$tuning_method_knn)) input$tuning_method_knn else "traditional"
    autotuneknn_param <- (tuning_method_knn != "manual")
    if(tuning_method_knn == "manual"){
      k_neighbors_model <- input$kneighbors
    }
  }

  # Determine if GridSearchCV should be used based on tuning method
  use_gridsearch_param <- FALSE
  if(input$model == "randomforest" && !is.null(input$tuning_method_rf) && input$tuning_method_rf == "gridsearch"){
    use_gridsearch_param <- TRUE
  } else if(input$model == "xgboost" && !is.null(input$tuning_method_xgb) && input$tuning_method_xgb == "gridsearch"){
    use_gridsearch_param <- TRUE
  } else if(input$model == "elasticnet" && !is.null(input$tuning_method_en) && input$tuning_method_en == "gridsearch"){
    use_gridsearch_param <- TRUE
  } else if(input$model == "naivebayes" && !is.null(input$tuning_method_nb) && input$tuning_method_nb == "gridsearch"){
    use_gridsearch_param <- TRUE
  } else if(input$model == "knn" && !is.null(input$tuning_method_knn) && input$tuning_method_knn == "gridsearch"){
    use_gridsearch_param <- TRUE
  }

  modelparameters<<-list("modeltype"=input$model,"invers"=F,"thresholdmodel"=input$thresholdmodel,
                         "fs"=input$fs,"adjustval"=input$adjustval,
                         "use_gridsearch"=use_gridsearch_param,
                         "alpha"=alpha_model,"lambda"=lambda_model,
                         "ntree"=ntree_model,"autotunerf"=autotunerf_param,"mtry"=mtry_model,
                         "autotunesvm"=autotunesvm_param,"cost"=cost_model,"gamma"=gamma_model,
                         "kernel"= kernel_model , #ifelse(is.null(kernel_model),"radial",kernel_model),
                         "autotunexgb"=autotunexgb_param,"nrounds"=nrounds_model,
                         "max_depth"=maxdepth_model,"eta"=eta_model,
                         "autotunelgb"=autotunelgb_param,"nrounds_lgb"=nrounds_lgb_model,
                         "num_leaves"=num_leaves_model,"learning_rate_lgb"=learning_rate_lgb_model,
                         "autotuneknn"=autotuneknn_param,"k_neighbors"=k_neighbors_model)
  print(ncol(learningmodel))
  validate(need(ncol(learningmodel)>1,"Not enough features"))


  resmodel<<-modelfunction(learningmodel = learningmodel,validation = validation,
                           modelparameters = modelparameters,
                           transformdataparameters = transformdataparameters,
                           datastructuresfeatures =  datastructuresfeatures,
                           learningselect = learningselect)
  
 list("DATALEARNINGMODEL"=resmodel$datalearningmodel,"MODEL"=resmodel$model,
      "DATAVALIDATIONMODEL"=resmodel$datavalidationmodel,
      "GROUPS"=resmodel$groups,"modelparameters"=resmodel$modelparameters)
  
  })


# Threshold observer for survival models
# Note: For survival models, threshold is used for risk score dichotomization
observe({
  if (input$model %in% c("cox", "rsf", "coxlasso", "coxelasticnet", "coxridge")) {
    updateNumericInput(session, "thresholdmodel", value = 0)  # Default threshold for risk scores
  }
  # REMOVED: Classification models (svm, randomforest, elasticnet, xgboost, lightgbm, naivebayes, knn)
  # These models do not handle censored survival data correctly
})

# Display optimal hyperparameters for survival models
output$modelalpha<-renderText({
  if(input$model %in% c("coxelasticnet") && !is.null(MODEL()$MODEL)){
    format(MODEL()$MODEL$alpha, digits = 3)
  } else {
    "N/A"
  }
})

output$modellambda<-renderText({
  if(input$model %in% c("coxelasticnet", "coxlasso", "coxridge") && !is.null(MODEL()$MODEL)){
    format(MODEL()$MODEL$optimal_lambda, scientific = TRUE, digits = 4)
  } else {
    "N/A"
  }
})

output$modellambda1se<-renderText({
  if(input$model %in% c("coxelasticnet", "coxlasso", "coxridge") && !is.null(MODEL()$MODEL) && !is.null(MODEL()$MODEL$lambda_1se)){
    format(MODEL()$MODEL$lambda_1se, scientific = TRUE, digits = 4)
  } else {
    "N/A"
  }
})

output$modelnonzerocoef<-renderText({
  if(input$model %in% c("coxelasticnet", "coxlasso", "coxridge") && !is.null(MODEL()$MODEL)){
    coef_matrix <- as.matrix(coef(MODEL()$MODEL, s=MODEL()$MODEL$lambda))
    sum(coef_matrix[-1,1] != 0)
  } else {
    "N/A"
  }
})

# REMOVED: Classification model outputs (SVM, XGBoost, LightGBM, KNN)
# These models do not support censored survival data
# output$svmcost, output$svmgamma, output$svmkernel - REMOVED (SVM not supported)
# output$xgbnrounds, output$xgbmaxdepth, output$xgbeta, output$xgbminchild - REMOVED (XGBoost not supported)
# output$lgbnrounds, output$lgbnumleaves, output$lgblearningrate - REMOVED (LightGBM not supported)
# output$knnk - REMOVED (KNN not supported)

# Random Survival Forest hyperparameters (for RSF model)
output$rfmtry<-renderText({
  if(input$model=="rsf" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$optimal_mtry
  } else {
    "N/A"
  }
})

output$rfntree<-renderText({
  if(input$model=="rsf" && !is.null(MODEL()$MODEL)){
    MODEL()$MODEL$ntree_used
  } else {
    "N/A"
  }
})

# COMMENTED OUT: Classification model hyperparameter outputs
# These models (KNN, XGBoost, LightGBM) do not support censored survival data
# and have been removed from the application

# output$optiTuning_K = renderText({ ... })  # KNN - REMOVED
# output$xgbnrounds, output$xgbmaxdepth, output$xgbeta, output$xgbminchild - XGBoost - REMOVED
# output$lgbnrounds, output$lgbnumleaves, output$lgblearningrate - LightGBM - REMOVED
# output$knnk - KNN - REMOVED 


####
output$downloaddatalearning <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   MODEL()$DATALEARNINGMODEL$learningmodel, file) })


output$plotmodeldecouvroc <- renderPlot({
  datalearningmodel<<-MODEL()$DATALEARNINGMODEL
  if(!is.null(datalearningmodel$reslearningmodel$riskscores)){
    # Create risk groups based on median risk score
    risk_scores <- datalearningmodel$reslearningmodel$riskscores
    risk_groups <- ifelse(risk_scores >= median(risk_scores, na.rm = TRUE), "High risk", "Low risk")

    # Plot Kaplan-Meier curve
    km_plot <- plot_kaplan_meier(
      time = datalearningmodel$learningmodel$time,
      status = datalearningmodel$learningmodel$status,
      risk_groups = risk_groups,
      title = "Kaplan-Meier Curve - Learning Set",
      show_risk_table = TRUE,
      show_conf_int = TRUE
    )

    # Return the plot
    if(!is.null(km_plot)){
      print(km_plot)
    }
  } else {
    # Fallback: simple plot if no risk scores
    plot(1, type = "n", main = "No model available", xlab = "", ylab = "")
  }
})
output$youndendecouv<-renderTable({
  datalearningmodel<<-MODEL()$DATALEARNINGMODEL

  # For survival models: show survival statistics
  if(!is.null(datalearningmodel$reslearningmodel$riskscores)){
    time_data <- datalearningmodel$learningmodel$time
    status_data <- datalearningmodel$learningmodel$status
    risk_scores <- datalearningmodel$reslearningmodel$riskscores

    # Calculate survival statistics
    n_total <- length(status_data)
    n_events <- sum(status_data == 1, na.rm = TRUE)
    n_censored <- sum(status_data == 0, na.rm = TRUE)
    median_time <- median(time_data, na.rm = TRUE)
    risk_median <- median(risk_scores, na.rm = TRUE)

    surv_stats <- data.frame(
      Value = c(
        n_total,
        n_events,
        n_censored,
        round(median_time, 2),
        round(risk_median, 3)
      )
    )
    rownames(surv_stats) <- c("Total samples", "Events", "Censored", "Median time", "Median risk score")
    surv_stats
  } else {
    # For classification models: show younden statistics
    resyounden<-younden(datalearningmodel$reslearningmodel$classlearning, datalearningmodel$reslearningmodel$scorelearning)
    resyounden<-data.frame(resyounden)
    colnames(resyounden)<-c("")
    rownames(resyounden)<-c("younden","sensibility younden","specificity younden","threshold younden")
    resyounden
  }
},include.rownames=TRUE)
 
output$downloadplotdecouvroc = downloadHandler(
  filename = function() {paste('km_plot_learning','.',input$paramdownplot, sep='')},
  content = function(file) {
    datalearningmodel <- MODEL()$DATALEARNINGMODEL
    if(!is.null(datalearningmodel$reslearningmodel$riskscores)){
      # Create risk groups
      risk_scores <- datalearningmodel$reslearningmodel$riskscores
      risk_groups <- ifelse(risk_scores >= median(risk_scores, na.rm = TRUE), "High risk", "Low risk")

      # Generate KM plot
      km_plot <- plot_kaplan_meier(
        time = datalearningmodel$learningmodel$time,
        status = datalearningmodel$learningmodel$status,
        risk_groups = risk_groups,
        title = "Kaplan-Meier Curve - Learning Set"
      )

      # ggsurvplot returns a list with $plot element
      if(!is.null(km_plot) && !is.null(km_plot$plot)){
        ggsave(file, plot = km_plot$plot, device = input$paramdownplot, width = 10, height = 8)
      }
    }
  },
  contentType=NA)

output$downloaddatadecouvroc <- downloadHandler(
  filename = function() { paste('km_data_learning', '.',input$paramdowntable, sep='') },
  content = function(file) {
    datalearningmodel <- MODEL()$DATALEARNINGMODEL
    if(!is.null(datalearningmodel$reslearningmodel$riskscores)){
      # Create data export with risk scores and survival info
      export_data <- data.frame(
        Sample = rownames(datalearningmodel$learningmodel),
        Time = datalearningmodel$learningmodel$time,
        Status = datalearningmodel$learningmodel$status,
        Risk_Score = datalearningmodel$reslearningmodel$riskscores,
        Risk_Group = ifelse(datalearningmodel$reslearningmodel$riskscores >=
                           median(datalearningmodel$reslearningmodel$riskscores, na.rm = TRUE),
                           "High risk", "Low risk")
      )
      downloaddataset(export_data, file)
    }
  })

output$plotmodeldecouvbp <- renderPlot({
  datalearningmodel<<-MODEL()$DATALEARNINGMODEL
  scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                 threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T,printnames=input$shownames1)
})
output$downloadplotmodeldecouvbp = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatamodeldecouvbp <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = F), file) })
output$nbselectmodel<-renderText({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  ncol(datalearningmodel$learningmodel)-1
})

output$tabmodeldecouv<-renderTable({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL

  # Check if this is a survival model (has riskscores)
  if(!is.null(datalearningmodel$reslearningmodel$riskscores)){
    # Display risk score quantiles for survival models
    risk_scores <- datalearningmodel$reslearningmodel$riskscores
    quantiles <- quantile(risk_scores, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

    risk_summary <- data.frame(
      Statistic = c("Minimum", "Q1 (25%)", "Median (50%)", "Q3 (75%)", "Maximum", "Mean", "SD"),
      Value = c(
        quantiles[1],
        quantiles[2],
        quantiles[3],
        quantiles[4],
        quantiles[5],
        mean(risk_scores, na.rm = TRUE),
        sd(risk_scores, na.rm = TRUE)
      )
    )
    risk_summary
  } else {
    # Display confusion matrix for classification models
    as.data.frame.matrix(table(datalearningmodel$reslearningmodel$predictclasslearning,
                               datalearningmodel$reslearningmodel$classlearning))
  }
},include.rownames=FALSE)

output$cindexdecouv<-renderText({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  if(!is.null(datalearningmodel$reslearningmodel$riskscores)){
    cindex <- calculate_cindex(predicted_risk = datalearningmodel$reslearningmodel$riskscores,
                               time = datalearningmodel$learningmodel$time,
                               status = datalearningmodel$learningmodel$status)
    round(cindex, 3)
  } else {
    "N/A"
  }
})

output$ibsdecouv<-renderText({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  if(!is.null(MODEL()$MODEL)){
    tryCatch({
      ibs <- calculate_ibs(model = MODEL()$MODEL,
                          data = datalearningmodel$learningmodel,
                          time_col = "time",
                          status_col = "status")
      round(ibs, 3)
    }, error = function(e) {
      "N/A"
    })
  } else {
    "N/A"
  }
})

# ===============================
# TEMPORAL CLASSIFICATION OUTPUTS - LEARNING SET
# ===============================

# Calculate and store temporal metrics for learning set
temporal_metrics_learning <- reactive({
  datalearningmodel <- MODEL()$DATALEARNINGMODEL

  if(!is.null(datalearningmodel) && !is.null(datalearningmodel$reslearningmodel$riskscores)){
    # Detect model type
    model <- MODEL()$MODEL
    model_type <- "cox"  # Default

    if(!is.null(model)){
      if(inherits(model, "ranger")){
        model_type <- "rsf"
      } else if(inherits(model, "cv.glmnet")){
        model_type <- "coxnet"
      }
    }

    # Calculate temporal metrics
    temporal_results <- calculate_temporal_metrics(
      model = model,
      data = datalearningmodel$learningmodel,
      time_col = "time",
      status_col = "status",
      time_points = NULL,  # Auto-selected
      model_type = model_type
    )

    return(temporal_results)
  } else {
    return(NULL)
  }
})

# Plot temporal classification metrics (AUC/Sens/Spec over time)
output$plot_temporal_classif_learning <- renderPlot({
  temporal_metrics <- temporal_metrics_learning()

  if(!is.null(temporal_metrics)){
    p <- plot_temporal_classification_metrics(
      temporal_metrics,
      title = "Time-Dependent Classification Metrics - Learning Set"
    )

    if(!is.null(p)){
      print(p)
    }
  } else {
    plot(1, type = "n", main = "No temporal metrics available", xlab = "", ylab = "")
  }
})

# Download handler for temporal plot
output$download_temporal_plot_learning <- downloadHandler(
  filename = function() { paste('temporal_metrics_learning', '.', input$paramdownplot, sep='') },
  content = function(file) {
    temporal_metrics <- temporal_metrics_learning()
    if(!is.null(temporal_metrics)){
      p <- plot_temporal_classification_metrics(temporal_metrics)
      if(!is.null(p)){
        ggsave(file, plot = p, device = input$paramdownplot, width = 10, height = 6)
      }
    }
  },
  contentType = NA
)

# Table of temporal metrics
output$table_temporal_metrics_learning <- renderTable({
  temporal_metrics <- temporal_metrics_learning()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$metrics_df)){
    metrics_df <- temporal_metrics$metrics_df
    # Round values for display
    metrics_df$time <- round(metrics_df$time, 2)
    metrics_df$AUC <- round(metrics_df$AUC, 3)
    metrics_df$Sensitivity <- round(metrics_df$Sensitivity, 3)
    metrics_df$Specificity <- round(metrics_df$Specificity, 3)
    metrics_df$Threshold <- round(metrics_df$Threshold, 3)

    # Include confidence intervals if available
    if("AUC_CI_lower" %in% colnames(metrics_df)){
      metrics_df$AUC_CI_lower <- round(metrics_df$AUC_CI_lower, 3)
      metrics_df$AUC_CI_upper <- round(metrics_df$AUC_CI_upper, 3)
      # Create AUC_CI column for better display
      metrics_df$AUC_95CI <- paste0("[", metrics_df$AUC_CI_lower, ", ", metrics_df$AUC_CI_upper, "]")
      # Reorder columns
      metrics_df <- metrics_df[, c("time", "N_patients", "N_excluded", "AUC", "AUC_95CI", "Sensitivity", "Specificity", "Threshold")]
    } else {
      metrics_df <- metrics_df[, c("time", "N_patients", "N_excluded", "AUC", "Sensitivity", "Specificity", "Threshold")]
    }

    return(metrics_df)
  } else {
    return(NULL)
  }
}, include.rownames = FALSE)

# Confusion matrix at a specific time point (median time)
output$confusion_matrix_learning <- renderTable({
  temporal_metrics <- temporal_metrics_learning()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$detailed_results)){
    # Use median time point
    n_times <- length(temporal_metrics$detailed_results)
    median_idx <- ceiling(n_times / 2)

    result_at_time <- temporal_metrics$detailed_results[[median_idx]]

    if(!is.null(result_at_time) && !is.null(result_at_time$confusion_matrix)){
      cm <- result_at_time$confusion_matrix
      time_point <- round(result_at_time$time, 2)

      # Convert to data frame with labels
      cm_df <- as.data.frame.matrix(cm)
      cm_df <- cbind(Predicted = rownames(cm_df), cm_df)
      colnames(cm_df) <- c("Predicted", "Event", "Event-free")

      return(cm_df)
    }
  }
  return(NULL)
}, include.rownames = FALSE)

# Display survival statistics (pure survival part)
output$survival_stats_learning <- renderTable({
  datalearningmodel <- MODEL()$DATALEARNINGMODEL

  if(!is.null(datalearningmodel) && !is.null(datalearningmodel$reslearningmodel$riskscores)){
    model <- MODEL()$MODEL
    model_type <- "cox"

    if(!is.null(model)){
      if(inherits(model, "ranger")){
        model_type <- "rsf"
      } else if(inherits(model, "cv.glmnet")){
        model_type <- "coxnet"
      }
    }

    stats <- display_survival_statistics(
      model = model,
      data = datalearningmodel$learningmodel,
      time_col = "time",
      status_col = "status",
      model_type = model_type,
      risk_scores = datalearningmodel$reslearningmodel$riskscores
    )

    if(!is.null(stats) && !is.null(stats$median_by_group)){
      stats_df <- stats$median_by_group
      stats_df$Median_Survival <- round(stats_df$Median_Survival, 2)
      return(stats_df)
    }
  }
  return(NULL)
}, include.rownames = FALSE)

# ===============================
# TEMPORAL CLASSIFICATION OUTPUTS - VALIDATION SET
# ===============================

# Calculate and store temporal metrics for validation set
# CRITICAL: Uses thresholds from TRAINING set (not recalculated)
temporal_metrics_validation <- reactive({
  datavalidationmodel <- MODEL()$DATAVALIDATIONMODEL

  if(!is.null(datavalidationmodel) && !is.null(datavalidationmodel$resvalidationmodel$riskscores)){
    model <- MODEL()$MODEL
    model_type <- "cox"

    if(!is.null(model)){
      if(inherits(model, "ranger")){
        model_type <- "rsf"
      } else if(inherits(model, "cv.glmnet")){
        model_type <- "coxnet"
      }
    }

    # Get training metrics to extract thresholds and time points
    training_metrics <- temporal_metrics_learning()

    # Extract thresholds and time points from training set
    training_thresholds <- NULL
    training_time_points <- NULL

    if(!is.null(training_metrics)){
      training_thresholds <- training_metrics$thresholds
      training_time_points <- training_metrics$time_points
    }

    # Calculate validation metrics using TRAINING thresholds
    temporal_results <- calculate_temporal_metrics(
      model = model,
      data = datavalidationmodel$validationmodel,
      time_col = "time",
      status_col = "status",
      time_points = training_time_points,  # Use same time points as training
      model_type = model_type,
      training_thresholds = training_thresholds,  # Apply training thresholds
      compute_ci = TRUE  # Compute CI for validation AUC
    )

    return(temporal_results)
  } else {
    return(NULL)
  }
})

# Plot temporal classification metrics for validation
output$plot_temporal_classif_validation <- renderPlot({
  temporal_metrics <- temporal_metrics_validation()

  if(!is.null(temporal_metrics)){
    p <- plot_temporal_classification_metrics(
      temporal_metrics,
      title = "Time-Dependent Classification Metrics - Validation Set"
    )

    if(!is.null(p)){
      print(p)
    }
  } else {
    plot(1, type = "n", main = "No temporal metrics available", xlab = "", ylab = "")
  }
})

# Download handler for temporal plot - validation
output$download_temporal_plot_validation <- downloadHandler(
  filename = function() { paste('temporal_metrics_validation', '.', input$paramdownplot, sep='') },
  content = function(file) {
    temporal_metrics <- temporal_metrics_validation()
    if(!is.null(temporal_metrics)){
      p <- plot_temporal_classification_metrics(temporal_metrics)
      if(!is.null(p)){
        ggsave(file, plot = p, device = input$paramdownplot, width = 10, height = 6)
      }
    }
  },
  contentType = NA
)

# Table of temporal metrics - validation
output$table_temporal_metrics_validation <- renderTable({
  temporal_metrics <- temporal_metrics_validation()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$metrics_df)){
    metrics_df <- temporal_metrics$metrics_df
    # Round values for display
    metrics_df$time <- round(metrics_df$time, 2)
    metrics_df$AUC <- round(metrics_df$AUC, 3)
    metrics_df$Sensitivity <- round(metrics_df$Sensitivity, 3)
    metrics_df$Specificity <- round(metrics_df$Specificity, 3)
    metrics_df$Threshold <- round(metrics_df$Threshold, 3)

    # Include confidence intervals if available
    if("AUC_CI_lower" %in% colnames(metrics_df)){
      metrics_df$AUC_CI_lower <- round(metrics_df$AUC_CI_lower, 3)
      metrics_df$AUC_CI_upper <- round(metrics_df$AUC_CI_upper, 3)
      # Create AUC_CI column for better display
      metrics_df$AUC_95CI <- paste0("[", metrics_df$AUC_CI_lower, ", ", metrics_df$AUC_CI_upper, "]")
      # Reorder columns - NOTE: Threshold is from TRAINING set
      metrics_df <- metrics_df[, c("time", "N_patients", "N_excluded", "AUC", "AUC_95CI", "Sensitivity", "Specificity", "Threshold")]
    } else {
      metrics_df <- metrics_df[, c("time", "N_patients", "N_excluded", "AUC", "Sensitivity", "Specificity", "Threshold")]
    }

    return(metrics_df)
  } else {
    return(NULL)
  }
}, include.rownames = FALSE)

# Confusion matrix at median time - validation
output$confusion_matrix_validation <- renderTable({
  temporal_metrics <- temporal_metrics_validation()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$detailed_results)){
    n_times <- length(temporal_metrics$detailed_results)
    median_idx <- ceiling(n_times / 2)

    result_at_time <- temporal_metrics$detailed_results[[median_idx]]

    if(!is.null(result_at_time) && !is.null(result_at_time$confusion_matrix)){
      cm <- result_at_time$confusion_matrix

      cm_df <- as.data.frame.matrix(cm)
      cm_df <- cbind(Predicted = rownames(cm_df), cm_df)
      colnames(cm_df) <- c("Predicted", "Event", "Event-free")

      return(cm_df)
    }
  }
  return(NULL)
}, include.rownames = FALSE)

# Download comprehensive results as Excel
output$download_complete_results <- downloadHandler(
  filename = function() { paste('survival_results_', Sys.Date(), '.xlsx', sep='') },
  content = function(file) {
    # Get model info
    model <- MODEL()$MODEL
    model_type <- "cox"
    if(!is.null(model) && inherits(model, "ranger")){
      model_type <- "rsf"
    } else if(!is.null(model) && inherits(model, "cv.glmnet")){
      model_type <- "coxnet"
    }

    model_info <- list(
      model_type = model_type,
      date = Sys.Date()
    )

    # Export results
    export_survival_results(
      temporal_metrics_learning = temporal_metrics_learning(),
      temporal_metrics_validation = temporal_metrics_validation(),
      model_info = model_info,
      filename = file
    )
  },
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

# Download temporal metrics as CSV
output$download_temporal_csv <- downloadHandler(
  filename = function() { paste('temporal_metrics_', Sys.Date(), '.csv', sep='') },
  content = function(file) {
    export_results_csv(
      temporal_metrics_learning = temporal_metrics_learning(),
      temporal_metrics_validation = temporal_metrics_validation(),
      filename = file
    )
  },
  contentType = "text/csv"
)

# Display survival statistics - validation
output$survival_stats_validation <- renderTable({
  datavalidationmodel <- MODEL()$DATAVALIDATIONMODEL

  if(!is.null(datavalidationmodel) && !is.null(datavalidationmodel$resvalidationmodel$riskscores)){
    model <- MODEL()$MODEL
    model_type <- "cox"

    if(!is.null(model)){
      if(inherits(model, "ranger")){
        model_type <- "rsf"
      } else if(inherits(model, "cv.glmnet")){
        model_type <- "coxnet"
      }
    }

    stats <- display_survival_statistics(
      model = model,
      data = datavalidationmodel$validationmodel,
      time_col = "time",
      status_col = "status",
      model_type = model_type,
      risk_scores = datavalidationmodel$resvalidationmodel$riskscores
    )

    if(!is.null(stats) && !is.null(stats$median_by_group)){
      stats_df <- stats$median_by_group
      stats_df$Median_Survival <- round(stats_df$Median_Survival, 2)
      return(stats_df)
    }
  }
  return(NULL)
}, include.rownames = FALSE)


output$downloaddatavalidation <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset( data.frame("Class"=MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$validationmodel,check.names = F), file) })


output$plotmodelvalroc <- renderPlot({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  if(!is.null(datavalidationmodel) && !is.null(datavalidationmodel$resvalidationmodel$riskscores)){
    # Create risk groups based on median risk score
    risk_scores <- datavalidationmodel$resvalidationmodel$riskscores
    risk_groups <- ifelse(risk_scores >= median(risk_scores, na.rm = TRUE), "High risk", "Low risk")

    # Plot Kaplan-Meier curve
    km_plot <- plot_kaplan_meier(
      time = datavalidationmodel$validationmodel$time,
      status = datavalidationmodel$validationmodel$status,
      risk_groups = risk_groups,
      title = "Kaplan-Meier Curve - Validation Set",
      show_risk_table = TRUE,
      show_conf_int = TRUE
    )

    # Return the plot
    if(!is.null(km_plot)){
      print(km_plot)
    }
  } else {
    # Fallback: simple plot if no validation data
    plot(1, type = "n", main = "No validation data available", xlab = "", ylab = "")
  }
})

output$downloadplotvalroc = downloadHandler(
  filename = function() {paste('km_plot_validation','.',input$paramdownplot, sep='')},
  content = function(file) {
    datavalidationmodel <- MODEL()$DATAVALIDATIONMODEL
    if(!is.null(datavalidationmodel) && !is.null(datavalidationmodel$resvalidationmodel$riskscores)){
      # Create risk groups
      risk_scores <- datavalidationmodel$resvalidationmodel$riskscores
      risk_groups <- ifelse(risk_scores >= median(risk_scores, na.rm = TRUE), "High risk", "Low risk")

      # Generate KM plot
      km_plot <- plot_kaplan_meier(
        time = datavalidationmodel$validationmodel$time,
        status = datavalidationmodel$validationmodel$status,
        risk_groups = risk_groups,
        title = "Kaplan-Meier Curve - Validation Set"
      )

      # ggsurvplot returns a list with $plot element
      if(!is.null(km_plot) && !is.null(km_plot$plot)){
        ggsave(file, plot = km_plot$plot, device = input$paramdownplot, width = 10, height = 8)
      }
    }
  },
  contentType=NA)

output$downloaddatavalroc <- downloadHandler(
  filename = function() { paste('km_data_validation', '.',input$paramdowntable, sep='') },
  content = function(file) {
    datavalidationmodel <- MODEL()$DATAVALIDATIONMODEL
    if(!is.null(datavalidationmodel) && !is.null(datavalidationmodel$resvalidationmodel$riskscores)){
      # Create data export with risk scores and survival info
      export_data <- data.frame(
        Sample = rownames(datavalidationmodel$validationmodel),
        Time = datavalidationmodel$validationmodel$time,
        Status = datavalidationmodel$validationmodel$status,
        Risk_Score = datavalidationmodel$resvalidationmodel$riskscores,
        Risk_Group = ifelse(datavalidationmodel$resvalidationmodel$riskscores >=
                           median(datavalidationmodel$resvalidationmodel$riskscores, na.rm = TRUE),
                           "High risk", "Low risk")
      )
      downloaddataset(export_data, file)
    }
  })

output$plotmodelvalbp <- renderPlot({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  scoremodelplot(class = datavalidationmodel$resvalidationmodel$classval ,score =datavalidationmodel$resvalidationmodel$scoreval,names=rownames(datavalidationmodel$resvalidationmodel),
                 threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T,printnames=input$shownames1)
})

output$downloadplotmodelvalbp = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =scoremodelplot(class = MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval ,score =MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,names=rownames(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatamodelvalbp <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   scoremodelplot(class = MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval ,score =MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,names=rownames(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = F), file) })

output$youndenval<-renderTable({
  datavalidationmodel<<-MODEL()$DATAVALIDATIONMODEL

  # For survival models: show survival statistics
  if(!is.null(datavalidationmodel) && !is.null(datavalidationmodel$resvalidationmodel$riskscores)){
    time_data <- datavalidationmodel$validationmodel$time
    status_data <- datavalidationmodel$validationmodel$status
    risk_scores <- datavalidationmodel$resvalidationmodel$riskscores

    # Calculate survival statistics
    n_total <- length(status_data)
    n_events <- sum(status_data == 1, na.rm = TRUE)
    n_censored <- sum(status_data == 0, na.rm = TRUE)
    median_time <- median(time_data, na.rm = TRUE)
    risk_median <- median(risk_scores, na.rm = TRUE)

    surv_stats <- data.frame(
      Value = c(
        n_total,
        n_events,
        n_censored,
        round(median_time, 2),
        round(risk_median, 3)
      )
    )
    rownames(surv_stats) <- c("Total samples", "Events", "Censored", "Median time", "Median risk score")
    surv_stats
  } else {
    # For classification models: show younden statistics
    resyounden<-younden(datavalidationmodel$resvalidationmodel$classval,datavalidationmodel$resvalidationmodel$scoreval )
    resyounden<-data.frame(resyounden)
    colnames(resyounden)<-c("")
    rownames(resyounden)<-c("younden","sensibility younden","specificity younden","threshold younden")
    resyounden
  }
},include.rownames=TRUE)

output$tabmodelval<-renderTable({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL

  # For survival models: show risk score quantiles
  if(!is.null(datavalidationmodel) && !is.null(datavalidationmodel$resvalidationmodel$riskscores)){
    risk_scores <- datavalidationmodel$resvalidationmodel$riskscores
    quantiles <- quantile(risk_scores, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

    risk_summary <- data.frame(
      Statistic = c("Minimum", "Q1 (25%)", "Median (50%)", "Q3 (75%)", "Maximum", "Mean", "SD"),
      Value = c(
        quantiles[1],
        quantiles[2],
        quantiles[3],
        quantiles[4],
        quantiles[5],
        mean(risk_scores, na.rm = TRUE),
        sd(risk_scores, na.rm = TRUE)
      )
    )

    risk_summary
  } else {
    # For classification models: show confusion matrix
    as.data.frame.matrix(table(datavalidationmodel$resvalidationmodel$predictclassval, datavalidationmodel$resvalidationmodel$classval))
  }
},include.rownames=TRUE)
output$cindexval<-renderText({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  if(!is.null(datavalidationmodel) && !is.null(datavalidationmodel$resvalidationmodel$riskscores)){
    cindex <- calculate_cindex(predicted_risk = datavalidationmodel$resvalidationmodel$riskscores,
                               time = datavalidationmodel$validationmodel$time,
                               status = datavalidationmodel$validationmodel$status)
    round(cindex, 3)
  } else {
    "N/A"
  }
})

output$ibsval<-renderText({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  if(!is.null(datavalidationmodel) && !is.null(MODEL()$MODEL)){
    tryCatch({
      ibs <- calculate_ibs(model = MODEL()$MODEL,
                          data = datavalidationmodel$validationmodel,
                          time_col = "time",
                          status_col = "status")
      round(ibs, 3)
    }, error = function(e) {
      "N/A"
    })
  } else {
    "N/A"
  }
})
####Detail of the model
output$summarymodel<-renderPrint({
  model<-print(MODEL()$MODEL)
})
output$plotimportance<-renderPlot({
  model<<-MODEL()$MODEL
  learningmodel<<-MODEL()$DATALEARNINGMODEL$learningmodel
  modeltype<<-input$model
  importanceplot(model = model,learningmodel = learningmodel,modeltype =modeltype,graph=T )
})
output$downloadplotimportance = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =  importanceplot(model = MODEL()$MODEL,learningmodel = MODEL()$DATALEARNINGMODEL$learningmodel,modeltype =input$model,graph=T ),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddataplotimportance <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(     importanceplot(model = MODEL()$MODEL,learningmodel = MODEL()$DATALEARNINGMODEL$learningmodel,modeltype =input$model,graph=F ), file) })

####Test prameters
output$testNAstructure<- reactive({
  if("TRUE"%in%input$NAstructuretest ){test<-as.logical(TRUE)}
  else{test<-as.logical(FALSE)}
  return(test)
})
outputOptions(output, 'testNAstructure', suspendWhenHidden=FALSE)

TESTPARAMETERS <- eventReactive(input$tunetest, { 
  prctvaluestest<-seq(input$prctvaluestest[1],input$prctvaluestest[2],by = 5)
  listparameters<<-list("prctvalues"=prctvaluestest,
                        "selectmethod"=input$selectmethodtest,
                        "NAstructure"=as.logical(input$NAstructuretest),
                        "thresholdNAstructure"=input$thresholdNAstructuretest,
                        "structdata"=input$structdatatest,
                        "maxvaluesgroupmin"=input$maxvaluesgroupmintest,
                        "minvaluesgroupmax"=input$minvaluesgroupmaxtest,
                        "rempNA"=input$rempNAtest,
                        "log"=as.logical(input$logtest),
                        "logtype"=input$logtypetest,
                        "standardization"=as.logical(input$standardizationtest),
                        "arcsin"=as.logical(input$arcsintest),"test"=input$testtest,
                        "adjustpv"=as.logical(input$adjustpvtest),
                        "thresholdpv"=input$thresholdpvtest,
                        "thresholdFC"=input$thresholdFCtest,
                        "model"=input$modeltest,
                        "thresholdmodel"=0,"fs"=as.logical(input$fstest),
                        "threshold_method"=input$threshold_method_test, 
                        "tuning_method"=input$tuning_method_test)
    length(listparameters$prctvalues)
    validate(need( sum(do.call(rbind, lapply(listparameters, FUN=function(x){length(x)==0})))==0,"One of the parameters is empty"))
    tabparameters<<-constructparameters(listparameters)
    # Set initial thresholds for probabilistic models
    # Note: If threshold_method != "fixed", these values will be recalculated
    # in testparametersfunction(). The 0.5 here serves as:
    # - Final threshold if threshold_method = "fixed" (default for probabilistic models)
    # - Initial placeholder if threshold_method = "youden" or "equiprob" (will be optimized)
    
    if(input$threshold_method_test == "fixed"){
      # No optimization: 0.5 is the final threshold for probabilistic models
      cat("✓ Using fixed thresholds: 0.5 for probabilistic models, 0 for SVM\n")
    } else if(input$threshold_method_test == "youden"){
      # Youden optimization enabled: 0.5 is a placeholder, will be recalculated
      cat("✓ Threshold optimization enabled: Youden method (maximize sensitivity + specificity)\n")
      cat("  Initial threshold: 0.5 (placeholder, will be optimized)\n")
      cat("\n")
      cat("⚠️  IMPORTANT NOTE about threshold optimization in Test Parameters:\n")
      cat("   - The threshold is optimized on TRAINING data for each parameter combination\n")
      cat("   - This is CORRECT methodology: fit threshold on train, apply to validation\n")
      cat("   - However, when comparing many combinations, the best validation result may be\n")
      cat("     slightly optimistic due to multiple testing (similar to hyperparameter tuning)\n")
      cat("   - Recommendation: Use these results to SELECT the best configuration,\n")
      cat("     then RE-VALIDATE on independent test data if available\n")
      cat("\n")
    } else if(input$threshold_method_test == "equiprob"){
      # Equiprobability optimization enabled: 0.5 is a placeholder, will be recalculated
      cat("✓ Threshold optimization enabled: Equiprobability method (minimize |FP-FN|)\n")
      cat("  Initial threshold: 0.5 (placeholder, will be optimized)\n")
      cat("\n")
      cat("⚠️  IMPORTANT NOTE about threshold optimization in Test Parameters:\n")
      cat("   - The threshold is optimized on TRAINING data for each parameter combination\n")
      cat("   - This is CORRECT methodology: fit threshold on train, apply to validation\n")
      cat("   - However, when comparing many combinations, the best validation result may be\n")
      cat("     slightly optimistic due to multiple testing (similar to hyperparameter tuning)\n")
      cat("   - Recommendation: Use these results to SELECT the best configuration,\n")
      cat("     then RE-VALIDATE on independent test data if available\n")
      cat("\n")
    }
    
    tabparameters$thresholdmodel[which(tabparameters$model=="randomforest")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="elasticnet")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="xgboost")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="lightgbm")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="knn")]<-0.5
    tabparameters$thresholdmodel[which(tabparameters$model=="naivebayes")]<-0.5
    
    validation<<-DATA()$VALIDATION
    learning<<-DATA()$LEARNING
    tabparametersresults<<-testparametersfunction(learning,validation,tabparameters)
    #clean useless columns
    if(length(which(apply(X = tabparametersresults,MARGIN=2,function(x){sum(is.na(x))})==nrow(tabparametersresults)))!=0){
      tabparametersresults<-tabparametersresults[,-which(apply(X = tabparametersresults,MARGIN=2,function(x){sum(is.na(x))})==nrow(tabparametersresults))]}
    return(tabparametersresults)

#     if(sum(listparameters$NAstructure)==0){tabparametersresults<-
#       tabparametersresults[,-c("thresholdNAstructure","structdata")]
#     }
    
                       
  })
# output$testtabparameters<- reactive({
#   if(!tabparameters ){test<-as.logical(FALSE)}
#   else{test<-as.logical(TRUE)}
#   return(test)
# })
# outputOptions(output, 'testNAstructure', suspendWhenHidden=FALSE)

output$tabtestparameters<-renderDataTable({
  resparameters<<-TESTPARAMETERS()
  cbind(Names=rownames(resparameters),resparameters)},
  options = list(    "orderClasses" = F,
                     "responsive" = F,
                     "pageLength" = 100
            #          ,rowCallback = I('
            # function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {$("td:eq(1)", nRow).css("color", "red");}'
                                                        # )
            )
            )


output$downloadtabtestparameters <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   TESTPARAMETERS(), file) })




# Nouveaux graphiques améliorés
output$plottestparametersthreshold = renderPlot({
  resparameters<<-TESTPARAMETERS()
  plot_threshold_performance(dataset_test_params = resparameters)
})

output$downloadplottestparametersthreshold = downloadHandler(
  filename = function() {paste('graph_threshold_performance','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = plot_threshold_performance(dataset_test_params = TESTPARAMETERS()),  
           device = input$paramdownplot)},
  contentType=NA)

output$plottestparametersoverfitting = renderPlot({
  resparameters<<-TESTPARAMETERS()
  plot_overfitting(dataset_test_params = resparameters)
})

output$downloadplottestparametersoverfitting = downloadHandler(
  filename = function() {paste('graph_overfitting','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = plot_overfitting(dataset_test_params = TESTPARAMETERS()),  
           device = input$paramdownplot)},
  contentType=NA)


# Fonction améliorée avec filtrage, tri et intervalles de confiance
plotbarstest =  function(dataset_test_params, type, filter_invalid = FALSE, show_ci = FALSE){
  # Filtrer les résultats non valides (AUC < 0.5 ou NA)
  if(filter_invalid){
    dataset_test_params <- dataset_test_params %>%
      filter(
        (`auc learning` > 0.5 | is.na(`auc learning`)),
        (`auc validation` > 0.5 | is.na(`auc validation`)),
        !is.na(`threshold used`) | is.na(`threshold used`)
      )
  }
  
  if(type ==  'learning'){
    new_dataset  =  dataset_test_params %>% 
      group_by(model, test) %>%
      summarise(
        mean_auc_learning = mean(`auc learning`, na.rm = TRUE),
        se_auc_learning = sd(`auc learning`, na.rm = TRUE) / sqrt(n()),
        mean_sensibility_learning = mean(`sensibility learning`, na.rm = TRUE),
        se_sensibility_learning = sd(`sensibility learning`, na.rm = TRUE) / sqrt(n()),
        mean_specificity_learning = mean(`specificity learning`, na.rm = TRUE),
        se_specificity_learning = sd(`specificity learning`, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop',
        count = n()
      )
    
    # Trier les modèles par performance moyenne (AUC)
    model_order <- new_dataset %>%
      group_by(model) %>%
      summarise(mean_perf = mean(mean_auc_learning, na.rm = TRUE)) %>%
      arrange(desc(mean_perf)) %>%
      pull(model)
    
    # Convertir le jeu de données en format long
    data_long <- pivot_longer(new_dataset, 
                              cols = starts_with("mean_"), 
                              names_to = "metric", 
                              values_to = "value")
    
    se_long <- pivot_longer(new_dataset,
                            cols = starts_with("se_"),
                            names_to = "metric_se",
                            values_to = "se")
    
    # Joindre les erreurs standard
    data_long$se <- se_long$se[match(
      paste(data_long$model, data_long$test, gsub("mean_", "", data_long$metric)),
      paste(se_long$model, se_long$test, gsub("se_", "", se_long$metric_se))
    )]
    
    data_long = data_long  %>% mutate(metric = recode(metric,
                                                      "mean_auc_learning" = "AUC Learning",
                                                      "mean_sensibility_learning" = "Sensitivity Learning",
                                                      "mean_specificity_learning" = "Specificity Learning")
    )
    
    # Trier les modèles
    data_long$model <- factor(data_long$model, levels = model_order)
    
  } else if(type == 'validation'){
    new_dataset  =  dataset_test_params %>% 
      group_by(model, test) %>%
      summarise(
        mean_auc_validation = mean(`auc validation`, na.rm = TRUE),
        se_auc_validation = sd(`auc validation`, na.rm = TRUE) / sqrt(n()),
        mean_sensibility_validation = mean(`sensibility validation`, na.rm = TRUE),
        se_sensibility_validation = sd(`sensibility validation`, na.rm = TRUE) / sqrt(n()),
        mean_specificity_validation = mean(`specificity validation`, na.rm = TRUE),
        se_specificity_validation = sd(`specificity validation`, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop',
        count = n()
      )
    
    # Trier les modèles par performance moyenne (AUC)
    model_order <- new_dataset %>%
      group_by(model) %>%
      summarise(mean_perf = mean(mean_auc_validation, na.rm = TRUE)) %>%
      arrange(desc(mean_perf)) %>%
      pull(model)
    
    # Convertir le jeu de données en format long
    data_long <- pivot_longer(new_dataset, 
                              cols = starts_with("mean_"), 
                              names_to = "metric", 
                              values_to = "value")
    
    se_long <- pivot_longer(new_dataset,
                            cols = starts_with("se_"),
                            names_to = "metric_se",
                            values_to = "se")
    
    # Joindre les erreurs standard
    data_long$se <- se_long$se[match(
      paste(data_long$model, data_long$test, gsub("mean_", "", data_long$metric)),
      paste(se_long$model, se_long$test, gsub("se_", "", se_long$metric_se))
    )]
    
    data_long = data_long  %>% mutate(metric = recode(metric,
                                                      "mean_auc_validation" = "AUC Validation",
                                                      "mean_sensibility_validation" = "Sensitivity Validation",
                                                      "mean_specificity_validation" = "Specificity Validation"
    )
    )
    
    # Trier les modèles
    data_long$model <- factor(data_long$model, levels = model_order)
    
  }else if (type == 'both'){
    new_dataset  =  dataset_test_params %>% 
      group_by(model, test) %>%
      summarise(
        mean_auc_validation = mean(`auc validation`, na.rm = TRUE),
        se_auc_validation = sd(`auc validation`, na.rm = TRUE) / sqrt(n()),
        mean_sensibility_validation = mean(`sensibility validation`, na.rm = TRUE),
        se_sensibility_validation = sd(`sensibility validation`, na.rm = TRUE) / sqrt(n()),
        mean_specificity_validation = mean(`specificity validation`, na.rm = TRUE),
        se_specificity_validation = sd(`specificity validation`, na.rm = TRUE) / sqrt(n()),
        mean_auc_learning = mean(`auc learning`, na.rm = TRUE),
        se_auc_learning = sd(`auc learning`, na.rm = TRUE) / sqrt(n()),
        mean_sensibility_learning = mean(`sensibility learning`, na.rm = TRUE),
        se_sensibility_learning = sd(`sensibility learning`, na.rm = TRUE) / sqrt(n()),
        mean_specificity_learning = mean(`specificity learning`, na.rm = TRUE),
        se_specificity_learning = sd(`specificity learning`, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop',
        count = n()
      )
    
    # Trier les modèles par performance moyenne (AUC validation, ou learning si validation NA)
    model_order <- new_dataset %>%
      group_by(model) %>%
      summarise(mean_perf = mean(ifelse(is.na(mean_auc_validation), mean_auc_learning, mean_auc_validation), na.rm = TRUE)) %>%
      arrange(desc(mean_perf)) %>%
      pull(model)
    
    # Convertir le jeu de données en format long
    data_long <- pivot_longer(new_dataset, 
                              cols = starts_with("mean_"), 
                              names_to = "metric", 
                              values_to = "value")
    
    se_long <- pivot_longer(new_dataset,
                            cols = starts_with("se_"),
                            names_to = "metric_se",
                            values_to = "se")
    
    # Joindre les erreurs standard
    data_long$se <- se_long$se[match(
      paste(data_long$model, data_long$test, gsub("mean_", "", data_long$metric)),
      paste(se_long$model, se_long$test, gsub("se_", "", se_long$metric_se))
    )]
    
    data_long = data_long  %>% mutate(metric = recode(metric,
                                                      "mean_auc_validation" = "AUC Validation",
                                                      "mean_sensibility_validation" = "Sensitivity Validation",
                                                      "mean_specificity_validation" = "Specificity Validation",
                                                      "mean_auc_learning" = "AUC Learning",
                                                      "mean_sensibility_learning" = "Sensitivity Learning",
                                                      "mean_specificity_learning" = "Specificity Learning")
    )
    
    # Trier les modèles
    data_long$model <- factor(data_long$model, levels = model_order)
  }
  
  # if(type == 'both'){
  #   scale_fill_manual(fill = c("AUC Learning" = "#E41A1C",
  #                              "Sensitivity Learning" = "#377EB8",
  #                              "Specificity Learning" = "#4DAF4A",
  #                              "AUC Validation" = "#E41A1C",
  #                              "Sensitivity Validation" = "#377EB8",
  #                              "Specificity Validation" = "#4DAF4A"))  
  # }else if(type == 'learning'){
  #   scale_fill_manual(fill = c("AUC Learning" = "#E41A1C",
  #                              "Sensitivity Learning" = "#377EB8",
  #                              "Specificity Learning" = "#4DAF4A")) 
  # }else if(type == 'validation'){
  #   scale_fill_manual(fill = c("AUC Validation" = "#E41A1C",
  #                              "Sensitivity Validation" = "#377EB8",
  #                              "Specificity Validation" = "#4DAF4A")) 
  # }
  
  # Créer le graphique à barres avec intervalles de confiance
  p <- ggplot(data_long, aes(x = model, y = value, fill = metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = round(value*100, 1)), 
              position = position_dodge(width = 0.8), 
              vjust = -0.5, size = 4) 
  
  # Ajouter les intervalles de confiance si demandé
  if(show_ci && !all(is.na(data_long$se))){
    p <- p + geom_errorbar(aes(ymin = value - 1.96*se, ymax = value + 1.96*se),
                           position = position_dodge(width = 0.9), 
                           width = 0.2, alpha = 0.7)
  }
  
  p <- p +
    facet_wrap(~ test, ncol = 2) +
    labs(x = "Models (sorted by performance)", 
         y = "Scores", 
         title = "Comparison of metrics by model and by test") +
    theme_minimal() +
    theme(axis.text.x = element_text(size =  12,face = 'bold', angle = 45, hjust = 1),
          axis.text.y =  element_text(size =  12,face = 'bold'),
          plot.title = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          strip.text = element_text(size = 12, face = "bold"),
          legend.text = element_text(size =10, face = 'bold'),
          legend.title = element_text(size =12, face = 'bold')
    ) + 
    scale_fill_brewer(palette = "Set1")
  
  return(p)
}

# Nouvelle fonction : Graphique seuil vs performance
plot_threshold_performance = function(dataset_test_params, filter_invalid = TRUE){
  # Filtrer les résultats non valides
  if(filter_invalid){
    dataset_test_params <- dataset_test_params %>%
      filter(
        (`auc learning` > 0.5 | is.na(`auc learning`)),
        (`auc validation` > 0.5 | is.na(`auc validation`)),
        !is.na(`threshold used`)
      )
  }
  
  # Filtrer les valeurs NA
  dataset_clean <- dataset_test_params %>%
    filter(!is.na(`threshold used`), 
           !is.na(`auc validation`) | !is.na(`auc learning`))
  
  if(nrow(dataset_clean) == 0){
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot", size = 6) +
             theme_void())
  }
  
  # Créer le graphique
  p <- ggplot(dataset_clean, aes(x = `threshold used`, y = `auc validation`, color = model)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.2) +
    facet_wrap(~ test, ncol = 2) +
    labs(x = "Optimal Threshold", 
         y = "AUC Validation",
         title = "Relationship between optimal threshold and validation performance",
         color = "Model") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, face = 'bold'),
          axis.text.y = element_text(size = 10, face = 'bold'),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 11, face = "bold"))
  
  return(p)
}

# Nouvelle fonction : Graphique overfitting
plot_overfitting = function(dataset_test_params, filter_invalid = TRUE){
  # Filtrer les résultats non valides
  if(filter_invalid){
    dataset_test_params <- dataset_test_params %>%
      filter(
        (`auc learning` > 0.5 | is.na(`auc learning`)),
        (`auc validation` > 0.5 | is.na(`auc validation`))
      )
  }
  
  # Calculer les différences (overfitting)
  dataset_overfit <- dataset_test_params %>%
    filter(!is.na(`auc learning`), !is.na(`auc validation`)) %>%
    mutate(
      overfitting_auc = `auc learning` - `auc validation`,
      overfitting_sens = `sensibility learning` - `sensibility validation`,
      overfitting_spec = `specificity learning` - `specificity validation`
    ) %>%
    select(model, test, overfitting_auc, overfitting_sens, overfitting_spec)
  
  if(nrow(dataset_overfit) == 0){
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No valid data to plot", size = 6) +
             theme_void())
  }
  
  # Calculer moyennes et erreurs standard par groupe
  dataset_summary <- dataset_overfit %>%
    pivot_longer(cols = starts_with("overfitting_"), 
                 names_to = "metric", 
                 values_to = "overfitting") %>%
    group_by(model, test, metric) %>%
    summarise(
      mean_overfitting = mean(overfitting, na.rm = TRUE),
      se_overfitting = sd(overfitting, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    ) %>%
    mutate(metric = recode(metric,
                           "overfitting_auc" = "AUC Overfitting",
                           "overfitting_sens" = "Sensitivity Overfitting",
                           "overfitting_spec" = "Specificity Overfitting"))
  
  # Trier les modèles par overfitting moyen (AUC)
  model_order <- dataset_summary %>%
    filter(metric == "AUC Overfitting") %>%
    group_by(model) %>%
    summarise(mean_overfit = mean(mean_overfitting, na.rm = TRUE)) %>%
    arrange(desc(mean_overfit)) %>%
    pull(model)
  
  dataset_summary$model <- factor(dataset_summary$model, levels = model_order)
  
  # Créer le graphique
  p <- ggplot(dataset_summary, aes(x = model, y = mean_overfitting, fill = model)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = mean_overfitting - 1.96*se_overfitting,
                      ymax = mean_overfitting + 1.96*se_overfitting),
                  position = position_dodge(width = 0.9),
                  width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    facet_grid(metric ~ test) +
    labs(x = "Models (sorted by AUC overfitting)", 
         y = "Mean Overfitting (Learning - Validation)",
         title = "Model overfitting analysis (positive = overfitting)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, face = 'bold', angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10, face = 'bold'),
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size = 10, face = "bold"),
          legend.position = "none")
  
  return(p)
}


# # PARTIE LEARNING
output$plottestparameterslearning = renderPlot({
  resparameters<<-TESTPARAMETERS()
  plotbarstest(dataset_test_params = resparameters, type ='learning')
})

output$downloadplottestparametersvalidation = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =   plotbarstest(dataset_test_params = TESTPARAMETERS() , type ='learning' ),
           device = input$paramdownplot)},
  contentType=NA)


# PARTIE VALIDATION
output$plottestparametersvalidation =  renderPlot({
  resparameters<<-TESTPARAMETERS()
  plotbarstest(dataset_test_params = resparameters, type ='validation')
})

output$downloadplottestparameterslearning = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =   plotbarstest(dataset_test_params = TESTPARAMETERS() , type ='validation' ),
           device = input$paramdownplot)},
  contentType=NA)

# PARTIE GLOBAL
output$plottestparametersboth =  renderPlot({
  resparameters<<-TESTPARAMETERS()
  plotbarstest(dataset_test_params = resparameters, type ='both')
})


output$downloadplottestparametersboth = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =   plotbarstest(dataset_test_params = TESTPARAMETERS() , type ='both' ),
           device = input$paramdownplot)},
  contentType=NA)

# 
# plotbarstest =  function(dataset_test_params, type){
#   # library(dplyr)
#   if(type ==  'learning'){
#     new_dataset  =  dataset_test_params %>% 
#       group_by(model, test) %>%
#       summarise(mean_auc_learning = mean(`auc learning`, na.rm = TRUE),
#                 mean_sensibility_learning = mean(`sensibility learning`, na.rm = TRUE),
#                 mean_specificity_learning = mean(`specificity learning`, na.rm = TRUE),
#                 .groups = 'drop',
#                 count = n())
#     
#     # Convertir le jeu de données en format long
#     data_long <- pivot_longer(new_dataset, 
#                               cols = starts_with("mean_"), 
#                               names_to = "metric", 
#                               values_to = "value")
#     
#     data_long = data_long  %>% mutate(metric = recode(metric,
#                                                       "mean_auc_learning" = "AUC Learning",
#                                                       "mean_sensibility_learning" = "Sensitivity Learning",
#                                                       "mean_specificity_learning" = "Specificity Learning")
#     )
#     
#   } else if(type == 'validation'){
#     new_dataset  =  dataset_test_params %>% 
#       group_by(model, test) %>%
#       summarise(mean_auc_validation = mean(`auc validation`, na.rm = TRUE),
#                 mean_sensibility_validation = mean(`sensibility validation`, na.rm = TRUE),
#                 mean_specificity_validation = mean(`specificityvalidation`, na.rm = TRUE),
#                 .groups = 'drop',
#                 count = n())
#     
#     # Convertir le jeu de données en format long
#     data_long <- pivot_longer(new_dataset, 
#                               cols = starts_with("mean_"), 
#                               names_to = "metric", 
#                               values_to = "value")
#     
#     data_long = data_long  %>% mutate(metric = recode(metric,
#                                                       "mean_auc_validation" = "AUC Validation",
#                                                       "mean_sensibility_validation" = "Sensitivity Validation",
#                                                       "mean_specificity_validation" = "Specificity Validation"
#                                                       )
#     )
#     
#   }else if (type == 'both'){
#     new_dataset  =  dataset_test_params %>% 
#       group_by(model, test) %>%
#       summarise(mean_auc_validation = mean(`auc validation`, na.rm = TRUE),
#                 mean_sensibility_validation = mean(`sensibility validation`, na.rm = TRUE),
#                 mean_specificity_validation = mean(`specificityvalidation`, na.rm = TRUE),
#                 mean_auc_learning = mean(`auc learning`, na.rm = TRUE),
#                 mean_sensibility_learning = mean(`sensibility learning`, na.rm = TRUE),
#                 mean_specificity_learning = mean(`specificity learning`, na.rm = TRUE),
#                 .groups = 'drop',
#                 count = n())
#     
#     # Convertir le jeu de données en format long
#     data_long <- pivot_longer(new_dataset, 
#                               cols = starts_with("mean_"), 
#                               names_to = "metric", 
#                               values_to = "value")
#     
#     data_long = data_long  %>% mutate(metric = recode(metric,
#                                                       "mean_auc_validation" = "AUC Validation",
#                                                       "mean_sensibility_validation" = "Sensitivity Validation",
#                                                       "mean_specificity_validation" = "Specificity Validation",
#                                                       "mean_auc_learning" = "AUC Learning",
#                                                       "mean_sensibility_learning" = "Sensitivity Learning",
#                                                       "mean_specificity_learning" = "Specificity Learning")
#     )
#     
#   }
#   
#   # Créer le graphique à barres
#   # ggplot(data_long, aes(x = interaction(model, test), y = value, fill = metric)) +
#   #   geom_bar(stat = "identity", position = "dodge") +
#   #   labs(x = "Modèle et Test", y = "Valeur", title = "Comparaison des métriques par modèle et test") +
#   #   theme_minimal() +
#   #   theme(axis.text.x = element_text(size =  12,face = 'bold'),
#   #         axis.text.y =  element_text(size =  12,face = 'bold'),
#   #         plot.title = element_text(size = 14, face = "bold"),
#   #         axis.title.x = element_text(size = 13, face = "bold"),
#   #         axis.title.y = element_text(size = 13, face = "bold")
#   #   ) +
#   #   scale_fill_brewer(palette = "Set1") +
#   #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   
#   
#   # Créer le graphique à barres avec des facettes pour chaque test
#   ggplot(data_long, aes(x = model, y = value, fill = metric)) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
#     geom_text(aes(label = round(value*100, 1)), 
#               position = position_dodge(width = 0.8), 
#               vjust = -0.5) + 
#     facet_wrap(~ test, ncol = 2) +
#     labs(x = "Models", y = "Scores", title = "Comparaison des métriques par modèle et test") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(size =  12,face = 'bold'),
#           axis.text.y =  element_text(size =  12,face = 'bold'),
#           plot.title = element_text(size = 14, face = "bold"),
#           axis.title.x = element_text(size = 13, face = "bold"),
#           axis.title.y = element_text(size = 13, face = "bold"),
#           strip.text = element_text(size = 12, face = "bold"),
#           legend.text = element_text(size =10, face = 'bold'),
#           legend.title = element_text(size =12, face = 'bold')
#     ) + 
#     scale_fill_brewer(palette = "Set1") +
#     # scale_fill_manual(values = custom_colors) +
#     theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
#
# }

##########################
# New Survival Classif Tab Outputs
##########################

# Tables for confusion matrices - Training Set (for new tab)
output$confusion_matrix_learning_tab <- renderTable({
  temporal_metrics <- temporal_metrics_learning()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$detailed_results)){
    n_times <- length(temporal_metrics$detailed_results)
    median_idx <- ceiling(n_times / 2)

    result_at_time <- temporal_metrics$detailed_results[[median_idx]]

    if(!is.null(result_at_time) && !is.null(result_at_time$confusion_matrix)){
      cm <- result_at_time$confusion_matrix

      cm_df <- as.data.frame.matrix(cm)
      cm_df <- cbind(Predicted = rownames(cm_df), cm_df)
      colnames(cm_df) <- c("Predicted", "Event", "Event-free")

      return(cm_df)
    }
  }
  return(NULL)
}, include.rownames = FALSE)

# Tables for confusion matrices - Validation Set (for new tab)
output$confusion_matrix_validation_tab <- renderTable({
  temporal_metrics <- temporal_metrics_validation()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$detailed_results)){
    n_times <- length(temporal_metrics$detailed_results)
    median_idx <- ceiling(n_times / 2)

    result_at_time <- temporal_metrics$detailed_results[[median_idx]]

    if(!is.null(result_at_time) && !is.null(result_at_time$confusion_matrix)){
      cm <- result_at_time$confusion_matrix

      cm_df <- as.data.frame.matrix(cm)
      cm_df <- cbind(Predicted = rownames(cm_df), cm_df)
      colnames(cm_df) <- c("Predicted", "Event", "Event-free")

      return(cm_df)
    }
  }
  return(NULL)
}, include.rownames = FALSE)

# Tables for temporal metrics - Training Set (for new tab)
output$table_temporal_metrics_learning_tab <- renderTable({
  temporal_metrics <- temporal_metrics_learning()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$metrics_df)){
    metrics_df <- temporal_metrics$metrics_df
    # Round values for display
    metrics_df$time <- round(metrics_df$time, 2)
    metrics_df$AUC <- round(metrics_df$AUC, 3)
    metrics_df$Sensitivity <- round(metrics_df$Sensitivity, 3)
    metrics_df$Specificity <- round(metrics_df$Specificity, 3)
    metrics_df$Threshold <- round(metrics_df$Threshold, 3)

    # Select key columns
    metrics_df <- metrics_df[, c("time", "AUC", "Sensitivity", "Specificity", "Threshold")]

    return(metrics_df)
  } else {
    return(NULL)
  }
}, include.rownames = FALSE)

# Tables for temporal metrics - Validation Set (for new tab)
output$table_temporal_metrics_validation_tab <- renderTable({
  temporal_metrics <- temporal_metrics_validation()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$metrics_df)){
    metrics_df <- temporal_metrics$metrics_df
    # Round values for display
    metrics_df$time <- round(metrics_df$time, 2)
    metrics_df$AUC <- round(metrics_df$AUC, 3)
    metrics_df$Sensitivity <- round(metrics_df$Sensitivity, 3)
    metrics_df$Specificity <- round(metrics_df$Specificity, 3)
    metrics_df$Threshold <- round(metrics_df$Threshold, 3)

    # Select key columns - Note: Threshold is from TRAINING
    metrics_df <- metrics_df[, c("time", "AUC", "Sensitivity", "Specificity", "Threshold")]

    return(metrics_df)
  } else {
    return(NULL)
  }
}, include.rownames = FALSE)

# Plot timeROC curve - Training Set
output$plot_timeROC_learning <- renderPlot({
  temporal_metrics <- temporal_metrics_learning()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$timeROC_obj)){
    # Get median time point
    n_times <- length(temporal_metrics$detailed_results)
    median_idx <- ceiling(n_times / 2)

    p <- plot_timeROC_curve(
      temporal_metrics$timeROC_obj,
      time_point_index = median_idx,
      title = "Time-Dependent ROC Curve - Training Set"
    )

    if(!is.null(p)){
      print(p)
    } else {
      plot(1, type = "n", main = "No ROC curve available", xlab = "", ylab = "")
    }
  } else {
    plot(1, type = "n", main = "No temporal metrics available", xlab = "", ylab = "")
  }
})

# Download handler for timeROC plot - Training
output$download_timeROC_learning <- downloadHandler(
  filename = function() { paste('timeROC_learning', '.', input$paramdownplot, sep='') },
  content = function(file) {
    temporal_metrics <- temporal_metrics_learning()
    if(!is.null(temporal_metrics) && !is.null(temporal_metrics$timeROC_obj)){
      n_times <- length(temporal_metrics$detailed_results)
      median_idx <- ceiling(n_times / 2)

      p <- plot_timeROC_curve(
        temporal_metrics$timeROC_obj,
        time_point_index = median_idx,
        title = "Time-Dependent ROC Curve - Training Set"
      )

      if(!is.null(p)){
        ggsave(file, plot = p, device = input$paramdownplot, width = 8, height = 8)
      }
    }
  },
  contentType = NA
)

# Plot timeROC curve - Validation Set
output$plot_timeROC_validation <- renderPlot({
  temporal_metrics <- temporal_metrics_validation()

  if(!is.null(temporal_metrics) && !is.null(temporal_metrics$timeROC_obj)){
    # Get median time point
    n_times <- length(temporal_metrics$detailed_results)
    median_idx <- ceiling(n_times / 2)

    p <- plot_timeROC_curve(
      temporal_metrics$timeROC_obj,
      time_point_index = median_idx,
      title = "Time-Dependent ROC Curve - Validation Set"
    )

    if(!is.null(p)){
      print(p)
    } else {
      plot(1, type = "n", main = "No ROC curve available", xlab = "", ylab = "")
    }
  } else {
    plot(1, type = "n", main = "No temporal metrics available", xlab = "", ylab = "")
  }
})

# Download handler for timeROC plot - Validation
output$download_timeROC_validation <- downloadHandler(
  filename = function() { paste('timeROC_validation', '.', input$paramdownplot, sep='') },
  content = function(file) {
    temporal_metrics <- temporal_metrics_validation()
    if(!is.null(temporal_metrics) && !is.null(temporal_metrics$timeROC_obj)){
      n_times <- length(temporal_metrics$detailed_results)
      median_idx <- ceiling(n_times / 2)

      p <- plot_timeROC_curve(
        temporal_metrics$timeROC_obj,
        time_point_index = median_idx,
        title = "Time-Dependent ROC Curve - Validation Set"
      )

      if(!is.null(p)){
        ggsave(file, plot = p, device = input$paramdownplot, width = 8, height = 8)
      }
    }
  },
  contentType = NA
)

# Plot risk scatter with Youden threshold - Training Set
output$plot_scatter_youden_learning <- renderPlot({
  temporal_metrics <- temporal_metrics_learning()
  datalearningmodel <- MODEL()$DATALEARNINGMODEL

  if(!is.null(temporal_metrics) && !is.null(datalearningmodel)){
    p <- plot_risk_density_with_threshold(
      temporal_metrics,
      title = "Risk Score Distribution - Training Set"
    )

    if(!is.null(p)){
      print(p)
    } else {
      plot(1, type = "n", main = "No scatter plot available", xlab = "", ylab = "")
    }
  } else {
    plot(1, type = "n", main = "No temporal metrics available", xlab = "", ylab = "")
  }
})

# Download handler for scatter plot - Training
output$download_scatter_learning <- downloadHandler(
  filename = function() { paste('scatter_youden_learning', '.', input$paramdownplot, sep='') },
  content = function(file) {
    temporal_metrics <- temporal_metrics_learning()
    datalearningmodel <- MODEL()$DATALEARNINGMODEL

    if(!is.null(temporal_metrics) && !is.null(datalearningmodel)){
      p <- plot_risk_density_with_threshold(
        temporal_metrics,
        title = "Risk Score Distribution - Training Set"
      )

      if(!is.null(p)){
        ggsave(file, plot = p, device = input$paramdownplot, width = 10, height = 6)
      }
    }
  },
  contentType = NA
)

# Plot risk scatter with Youden threshold - Validation Set
output$plot_scatter_youden_validation <- renderPlot({
  temporal_metrics <- temporal_metrics_validation()
  datavalidationmodel <- MODEL()$DATAVALIDATIONMODEL

  if(!is.null(temporal_metrics) && !is.null(datavalidationmodel)){
    p <- plot_risk_density_with_threshold(
      temporal_metrics,
      title = "Risk Score Distribution - Validation Set"
    )

    if(!is.null(p)){
      print(p)
    } else {
      plot(1, type = "n", main = "No scatter plot available", xlab = "", ylab = "")
    }
  } else {
    plot(1, type = "n", main = "No temporal metrics available", xlab = "", ylab = "")
  }
})

# Download handler for scatter plot - Validation
output$download_scatter_validation <- downloadHandler(
  filename = function() { paste('scatter_youden_validation', '.', input$paramdownplot, sep='') },
  content = function(file) {
    temporal_metrics <- temporal_metrics_validation()
    datavalidationmodel <- MODEL()$DATAVALIDATIONMODEL

    if(!is.null(temporal_metrics) && !is.null(datavalidationmodel)){
      p <- plot_risk_density_with_threshold(
        temporal_metrics,
        title = "Risk Score Distribution - Validation Set"
      )

      if(!is.null(p)){
        ggsave(file, plot = p, device = input$paramdownplot, width = 10, height = 6)
      }
    }
  },
  contentType = NA
)

# Download complete classification results (reuse existing function)
output$download_classif_complete <- downloadHandler(
  filename = function() { paste('survival_classif_results_', Sys.Date(), '.xlsx', sep='') },
  content = function(file) {
    # Get model info
    model <- MODEL()$MODEL
    model_type <- "cox"
    if(!is.null(model) && inherits(model, "ranger")){
      model_type <- "rsf"
    } else if(!is.null(model) && inherits(model, "cv.glmnet")){
      model_type <- "coxnet"
    }

    model_info <- list(
      model_type = model_type,
      date = Sys.Date()
    )

    # Export results
    export_survival_results(
      temporal_metrics_learning = temporal_metrics_learning(),
      temporal_metrics_validation = temporal_metrics_validation(),
      model_info = model_info,
      filename = file
    )
  },
  contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

# Download classification CSV (reuse existing function)
output$download_classif_csv <- downloadHandler(
  filename = function() { paste('survival_classif_metrics_', Sys.Date(), '.csv', sep='') },
  content = function(file) {
    export_results_csv(
      temporal_metrics_learning = temporal_metrics_learning(),
      temporal_metrics_validation = temporal_metrics_validation(),
      filename = file
    )
  },
  contentType = "text/csv"
)

})

# 
