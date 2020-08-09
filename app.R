
rm(list=ls())
dev.null <- gc()

library(shinydashboard)
library(shiny)
library(caret)
library(h2o)
library(data.table)
library(DT)

library(readxl)
library(httr)
packageVersion("readxl")
library(xlsx)


h2o.init(nthreads=1,max_mem_size = "1G")

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("PREDICE EUROMILLONES", tabName="predice", icon=icon("train"), selected=TRUE)
              )
)

body <- dashboardBody(
  tabItems(
    
    #--------------------------------------------
    # TRAIN MODEL TAB
    #--------------------------------------------
    tabItem(tabName = "predice",
            
            fluidRow(
              box(
                width = 6,
                color = "light-blue",
                status = "primary",
                background = "light-blue",
                title = "PREDICE EUROMILLONES",
                actionButton('pred', 
                             label = 'Haz Click Para Predecir',
                             icon("cog",lib="glyphicon")))
               ),
            
            h2(),
            
            fluidRow(
              valueBoxOutput("N1"),
              valueBoxOutput("N2"),
              valueBoxOutput("N3"),
              valueBoxOutput("N4"),
              valueBoxOutput("N5"),
              valueBoxOutput("E1"),
              valueBoxOutput("E2")
            )
            
    )
    #--------------------------------------------
    # 
    #--------------------------------------------

    
  )
)  

ui <- dashboardPage(
  
  dashboardHeader(title = "PREDICE EUROMILLONES"),
  sidebar,
  body,
  fluidPage(
    passwordInput("password", "Password:"),
    actionButton("go", "Go"),
    verbatimTextOutput("value")
  )
)

server <- shinyServer(function(input, output){
  
  library(shinydashboard)
  library(shiny)
  library(caret)
  library(h2o)
  library(data.table)
  library(xlsx)
  library(DT)
  
  set.seed(122)
  
  output$value <- renderText({
    req(input$go)
    isolate(input$password)
  })

  #reactive elements
  Values <- reactiveValues(
    N1=NULL, N2=NULL, N3=NULL, N4=NULL, N5=NULL, E1=NULL, E2=NULL)
  
  #------------------------------------------------
  # Buttom to train model
  #------------------------------------------------
  observeEvent(input$pred, {
    
    showModal(modalDialog(
      title = "BUSCANDO COMBINACION GANADORA",
      "Espera mientras la inteligencia artificial hace la busqueda :)",
      easyClose = TRUE,
      footer = NULL
    ))

    
    ###############################################################
    # PREDICT LOTTO
    ###############################################################
    
    ######################
    # Load Data
    ######################
    
    url1<-'https://docs.google.com/spreadsheet/pub?key=0AhqMeY8ZOrNKdEFUQ3VaTHVpU29UZ3l4emFQaVZub3c&output=xls'
    
    GET(url1, write_disk(tf <- tempfile(fileext = ".xls")))
    
    df <- read.xlsx(tf, 1)
    
    ##########################
    # Organize data
    ##########################
    dt <- as.data.table(df)
    
    colnames(dt)
    
    setnames(dt,c("COMB..GANADORA","NA.","NA..1","NA..2","NA..3","NA..4","ESTRELLAS","NA..5"),
             c("N1","N2","N3","N4","N5","DELETE","E1","E2"))
    
    
    dt[,DELETE:=NULL]
    
    dt[,N:=.I]
    dt <- dt[order(N, decreasing=T),]
    dt[,N:=NULL]
    
    dt <- dt[complete.cases(dt), ]
    
    dt[,N1:=as.numeric(levels(N1))[N1]]
    dt[,N2:=as.numeric(levels(N2))[N2]]
    dt[,N3:=as.numeric(levels(N3))[N3]]
    dt[,N4:=as.numeric(levels(N4))[N4]]
    dt[,N5:=as.numeric(levels(N5))[N5]]
    dt[,E1:=as.numeric(E1)]
    dt[,E2:=as.numeric(E2)]
    
    dt[,N1:=N1/100]
    dt[,N2:=N2/100]
    dt[,N3:=N3/100]
    dt[,N4:=N4/100]
    dt[,N5:=N5/100]
    dt[,E1:=E1/100]
    dt[,E2:=E2/100]
    
    count <- 0 
    countmax <- nrow(dt)
    
    # Reorder Numbers if needed
    
    needed <- FALSE
    
    if(needed)
    {
      while(count<countmax)
      {
        count <- count + 1
        
        dt.temp <- dt[1]
        
        dt <- dt[-1]
        
        v <- c(dt.temp[1,N1],dt.temp[1,N2],dt.temp[1,N3],dt.temp[1,N4],dt.temp[1,N5],
               dt.temp[1,E1],dt.temp[1,E2],dt.temp[1,FECHA])
        
        vs <- sort(v, decreasing = FALSE)
        
        dt.n <- data.table(N1=vs[1],
                           N2=vs[2],
                           N3=vs[3],
                           N4=vs[4],
                           N5=vs[5],
                           E1=vs[6],
                           E2=vs[7],
                           FECHA=vs[8])
        
        if(exists("dt.eur"))
        {
          dt.eur <- rbind(dt.eur,dt.n)
        } else {
          dt.eur <- copy(dt.n)
        }
        
      }
      
    } else {
      
      dt.eur <- copy(dt)
    }
    
    dt.eur <- dt.eur[complete.cases(dt.eur), ]
    
    head(dt.eur)
    
    summary(dt.eur)
    
    ds <- as.data.table(dt.eur)
    
    
    ###########################
    # Define feats delay
    ###########################
    dt.temp <- copy(ds)
    
    dt.temp[,target_1:=dt.temp$N1]
    dt.temp[,target_2:=dt.temp$N2]
    dt.temp[,target_3:=dt.temp$N3]
    dt.temp[,target_4:=dt.temp$N4]
    dt.temp[,target_5:=dt.temp$N5]
    dt.temp[,target_e1:=dt.temp$E1]
    dt.temp[,target_e2:=dt.temp$E2]
    
    dt.temp <- dt.temp[,c("target_1","target_2","target_3","target_4","target_5",
                          "target_e1","target_e2"),with=FALSE]
    
    dt.target <- dt.temp[5:nrow(dt.temp)]
    
    dt.temp <- copy(ds)
    dt.temp[,N1_feat1:=dt.temp$N1]
    dt.temp[,N2_feat1:=dt.temp$N2]
    dt.temp[,N3_feat1:=dt.temp$N3]
    dt.temp[,N4_feat1:=dt.temp$N4]
    dt.temp[,N5_feat1:=dt.temp$N5]
    dt.temp[,E1_feat1:=dt.temp$E1]
    dt.temp[,E2_feat1:=dt.temp$E2]
    dt.temp <- dt.temp[,c("N1_feat1","N2_feat1","N3_feat1","N4_feat1","N5_feat1",
                          "E1_feat1","E2_feat1"),with=FALSE]
    n1 <- dt.temp[1:nrow(dt.temp)]
    
    dt.temp <- copy(ds)
    dt.temp[,N1_feat2:=dt.temp$N1]
    dt.temp[,N2_feat2:=dt.temp$N2]
    dt.temp[,N3_feat2:=dt.temp$N3]
    dt.temp[,N4_feat2:=dt.temp$N4]
    dt.temp[,N5_feat2:=dt.temp$N5]
    dt.temp[,E1_feat2:=dt.temp$E1]
    dt.temp[,E2_feat2:=dt.temp$E2]
    dt.temp <- dt.temp[,c("N1_feat2","N2_feat2","N3_feat2","N4_feat2","N5_feat2",
                          "E1_feat2","E2_feat2"),with=FALSE]
    n2 <- dt.temp[2:nrow(dt.temp)]
    
    dt.temp <- copy(ds)
    dt.temp[,N1_feat3:=dt.temp$N1]
    dt.temp[,N2_feat3:=dt.temp$N2]
    dt.temp[,N3_feat3:=dt.temp$N3]
    dt.temp[,N4_feat3:=dt.temp$N4]
    dt.temp[,N5_feat3:=dt.temp$N5]
    dt.temp[,E1_feat3:=dt.temp$E1]
    dt.temp[,E2_feat3:=dt.temp$E2]
    dt.temp <- dt.temp[,c("N1_feat3","N2_feat3","N3_feat3","N4_feat3","N5_feat3",
                          "E1_feat3","E2_feat3"),with=FALSE]
    n3 <- dt.temp[3:nrow(dt.temp)]
    
    dt.temp <- copy(ds)
    dt.temp[,N1_feat4:=dt.temp$N1]
    dt.temp[,N2_feat4:=dt.temp$N2]
    dt.temp[,N3_feat4:=dt.temp$N3]
    dt.temp[,N4_feat4:=dt.temp$N4]
    dt.temp[,N5_feat4:=dt.temp$N5]
    dt.temp[,E1_feat4:=dt.temp$E1]
    dt.temp[,E2_feat4:=dt.temp$E2]
    dt.temp <- dt.temp[,c("N1_feat4","N2_feat4","N3_feat4","N4_feat4","N5_feat4",
                          "E1_feat4","E2_feat4"),with=FALSE]
    n4 <- dt.temp[4:nrow(dt.temp)]
    
    ###########################
    # define train datasets
    ###########################
    dt.data <- cbind(n1,n2,n3,n4,dt.target)
    
    #---------------------
    # train dataset
    #---------------------
    dt.train <- copy(dt.data)
    dh <- as.h2o(dt.train)
    
    # split in training and validation 
    splits <- h2o.splitFrame(dh, c(0.6,0.2),seed=1234)   
    train <- h2o.assign(splits[[1]], "train.hex")   
    valid <- h2o.assign(splits[[2]], "valid.hex")  
    #test <- h2o.assign(splits[[3]], "test.hex")  
    
    #---------------------
    # test dataset
    #---------------------
    dt.pred <- dt.train[nrow(dt.train),]
    
    # predict N1
    dt.pred_N1 <- dt.pred[,c("N1_feat1","N1_feat2","N1_feat3","N1_feat4","target_1"),with=FALSE]
    dt.pred_N1[,N1_feat1:=N1_feat2]
    dt.pred_N1[,N1_feat2:=N1_feat3]
    dt.pred_N1[,N1_feat3:=N1_feat4]
    dt.pred_N1[,N1_feat4:=target_1]
    dt.pred_N1[,target_1:=NULL]
    dh.n1 <- as.h2o(dt.pred_N1)
    
    # predict N2
    dt.pred_N2 <- dt.pred[,c("N2_feat1","N2_feat2","N2_feat3","N2_feat4","target_2"),with=FALSE]
    dt.pred_N2[,N2_feat1:=N2_feat2]
    dt.pred_N2[,N2_feat2:=N2_feat3]
    dt.pred_N2[,N2_feat3:=N2_feat4]
    dt.pred_N2[,N2_feat4:=target_2]
    dt.pred_N2[,target_2:=NULL]
    dh.n2 <- as.h2o(dt.pred_N2)
    
    # predict N3
    dt.pred_N3 <- dt.pred[,c("N3_feat1","N3_feat2","N3_feat3","N3_feat4","target_3"),with=FALSE]
    dt.pred_N3[,N3_feat1:=N3_feat2]
    dt.pred_N3[,N3_feat2:=N3_feat3]
    dt.pred_N3[,N3_feat3:=N3_feat4]
    dt.pred_N3[,N3_feat4:=target_3]
    dt.pred_N3[,target_3:=NULL]
    dh.n3 <- as.h2o(dt.pred_N3)
    
    # predict N4
    dt.pred_N4 <- dt.pred[,c("N4_feat1","N4_feat2","N4_feat3","N4_feat4","target_4"),with=FALSE]
    dt.pred_N4[,N4_feat1:=N4_feat2]
    dt.pred_N4[,N4_feat2:=N4_feat3]
    dt.pred_N4[,N4_feat3:=N4_feat4]
    dt.pred_N4[,N4_feat4:=target_4]
    dt.pred_N4[,target_4:=NULL]
    dh.n4 <- as.h2o(dt.pred_N4)
    
    # predict N5
    dt.pred_N5 <- dt.pred[,c("N5_feat1","N5_feat2","N5_feat3","N5_feat4","target_5"),with=FALSE]
    dt.pred_N5[,N5_feat1:=N5_feat2]
    dt.pred_N5[,N5_feat2:=N5_feat3]
    dt.pred_N5[,N5_feat3:=N5_feat4]
    dt.pred_N5[,N5_feat4:=target_5]
    dt.pred_N5[,target_5:=NULL]
    dh.n5 <- as.h2o(dt.pred_N5)
    
    # predict E1
    dt.pred_E1 <- dt.pred[,c("E1_feat1","E1_feat2","E1_feat3","E1_feat4","target_e1"),with=FALSE]
    dt.pred_E1[,E1_feat1:=E1_feat2]
    dt.pred_E1[,E1_feat2:=E1_feat3]
    dt.pred_E1[,E1_feat3:=E1_feat4]
    dt.pred_E1[,E1_feat4:=target_e1]
    dt.pred_E1[,target_e1:=NULL]
    dh.e1 <- as.h2o(dt.pred_E1)
    
    # predict E2
    dt.pred_E2 <- dt.pred[,c("E2_feat1","E2_feat2","E2_feat3","E2_feat4","target_e2"),with=FALSE]
    dt.pred_E2[,E2_feat1:=E2_feat2]
    dt.pred_E2[,E2_feat2:=E2_feat3]
    dt.pred_E2[,E2_feat3:=E2_feat4]
    dt.pred_E2[,E2_feat4:=target_e2]
    dt.pred_E2[,target_e2:=NULL]
    dh.e2 <- as.h2o(dt.pred_E2)
    
    showModal(modalDialog(
      title = "BUSCANDO COMBINACION GANADORA",
      "Proceso completado al 5%",
      easyClose = TRUE,
      footer = NULL
    ))
    
    
    patterns_max <- 7
    count <- 0
    Lotto <- list()
    
    while(count < patterns_max)
    {
      count <- count + 1
      
      if(count == 1)
      {
        predictors <- c("N1_feat1","N1_feat2","N1_feat3","N1_feat4")
        target <- "target_1"
        dt.lotto.pred <- copy(dh.n1)
      } else if(count == 2){
        predictors <- c("N2_feat1","N2_feat2","N2_feat3","N2_feat4")
        target <- "target_2"
        dt.lotto.pred <- copy(dh.n2)
      } else if(count == 3){
        predictors <- c("N3_feat1","N3_feat2","N3_feat3","N3_feat4")
        target <- "target_3"
        dt.lotto.pred <- copy(dh.n3)
      } else if(count == 4){
        predictors <- c("N4_feat1","N4_feat2","N4_feat3","N4_feat4")
        target <- "target_4"
        dt.lotto.pred <- copy(dh.n4)
      } else if(count == 5){
        predictors <- c("N5_feat1","N5_feat2","N5_feat3","N5_feat4")
        target <- "target_5"  
        dt.lotto.pred <- copy(dh.n5)
      } else if(count == 6){
        predictors <- c("E1_feat1","E1_feat2","E1_feat3","E1_feat4")
        target <- "target_e1"  
        dt.lotto.pred <- copy(dh.e1)
      } else {
        predictors <- c("E2_feat1","E2_feat2","E2_feat3","E2_feat4")
        target <- "target_e2"  
        dt.lotto.pred <- copy(dh.e2)
      }
      
      
      ##########################
      # Train the model
      ##########################
      set.seed(1234)
  
      model <- h2o.deeplearning(
        training_frame = train,        ## the H2O frame for training
        validation_frame = valid,      ## the H2O frame for validation (not required)
        x=predictors,                  ## the predictor columns, by column index
        y=target, 
        hidden = c(20, 5, 20, 5, 20),  ## the target index (what we are predicting)
        epochs = 600,
        model_id = "euro_millones",
        activation = "Tanh",
        seed = 1234) 

      
      valor <- paste0("Proceso completado al ", round(count*100/7,0), "%")
      
      showModal(modalDialog(
        title = "BUSCANDO COMBINACION GANADORA",
        valor,
        easyClose = TRUE,
        footer = NULL
      ))
      

      ##########################
      # Predict the number
      ##########################
      finalRf_predictions<-h2o.predict(
        object = model
        ,newdata = dt.lotto.pred)
      
      prediction <- as.data.table(finalRf_predictions)
      
      
      Lotto[[count]] <- round(prediction*100,0)
      
    } # while end
    
    Lotto_Final <- data.table(N1=Lotto[[1]],
                              N2=Lotto[[2]],
                              N3=Lotto[[3]],
                              N4=Lotto[[4]],
                              N5=Lotto[[5]],
                              E1=Lotto[[6]],
                              E2=Lotto[[7]]
    )
    
    
    ###############################################################
    # PREDICT LOTTO COMPLETADO
    ###############################################################

    Values$N1 <- Lotto_Final[[1]]
    Values$N2 <- Lotto_Final[[2]]
    Values$N3 <- Lotto_Final[[3]]
    Values$N4 <- Lotto_Final[[4]]
    Values$N5 <- Lotto_Final[[5]]
    Values$E1 <- Lotto_Final[[6]]
    Values$E2 <- Lotto_Final[[7]]
    
    showModal(modalDialog(
      title = "Predicciones Completadas",
      "MUCHA SUERTE!!! :)",
      easyClose = TRUE,
      footer = NULL
    ))
    
  })
  #------------------------------------------------
  # 
  #------------------------------------------------


  #------------------------------------------------
  # 
  #------------------------------------------------

  #------------------------------------------------
  # Info Accuary Box
  #------------------------------------------------
  output$N1 <- renderValueBox({
    valueBox(
      paste0(Values$N1, " "), "N1", icon = icon("result"),
      color = "yellow"
    )
  })
  
  output$N2 <- renderValueBox({
    valueBox(
      paste0(Values$N2, " "), "N2", icon = icon("result"),
      color = "yellow"
    )
  })
  
  output$N3 <- renderValueBox({
    valueBox(
      paste0(Values$N3, " "), "N3", icon = icon("result"),
      color = "yellow"
    )
  })
  
  output$N4 <- renderValueBox({
    valueBox(
      paste0(Values$N4, " "), "N4", icon = icon("result"),
      color = "yellow"
    )
  })
  
  output$N5 <- renderValueBox({
    valueBox(
      paste0(Values$N5, " "), "N5", icon = icon("result"),
      color = "yellow"
    )
  })
  
  output$E1 <- renderValueBox({
    valueBox(
      paste0(Values$E1, " "), "E1", icon = icon("result"),
      color = "red"
    )
  })
  
  output$E2 <- renderValueBox({
    valueBox(
      paste0(Values$E2, " "), "E2", icon = icon("result"),
      color = "red"
    )
  })
  
  #------------------------------------------------
  # 
  #------------------------------------------------
  
  

})

shinyApp(ui, server)