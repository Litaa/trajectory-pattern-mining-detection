library(shiny) #v
library(cluster) 
library(fpc) 
library(sp)
library(ggplot2)
library(dplyr)
library(splitr) 
library(RPostgreSQL)
library(here)

shinyServer(function(input, output, session)
{
  #CONNECT DATABASE
  pg = dbDriver("PostgreSQL")
  con = dbConnect(pg,user="postgres", password="admin",
                  host="localhost", port=5432, dbname="hotspot_australia")
  
  selectedData <- reactive({
    switch(input$date_Period,
           aus_oct = myobj <- dbGetQuery(con, "select * from oct_aus"),
           aus_nov = myobj <- dbGetQuery(con, "select * from nov_aus"),
           aus_dec = myobj <- dbGetQuery(con, "select * from dec_aus")
           )
  })
  
  
  selectedDataInput <- reactive({
    switch(input$date_PeriodInput,
           aus_oct = myobj <- dbGetQuery(con, "select * from oct_aus"),
           aus_nov = myobj <- dbGetQuery(con, "select * from nov_aus"),
           aus_dec = myobj <- dbGetQuery(con, "select * from dec_aus"))
  })
  
  title_periode <- reactive({
    switch(input$date_Period,
           aus_oct = capt <- 'Australia, October 2019',
           aus_nov = capt <- 'Australia, November 2019',
           aus_dec = capt <- 'Australia, December 2019')
    
    return(capt);
  })
  
  title_periodeInput <- reactive({
    switch(input$date_PeriodInput,
           aus_oct = capt <- 'Australia, October 2019',
           aus_nov = capt <- 'Australia, November 2019',
           aus_dec = capt <- 'Australia, December 2019')
    return(capt);
  })
  
  
  pop_about <- reactive({
    showModal(modalDialog(
      title = "Application Information",
      HTML("1. This application used to visualize Haje Trajectory Pattern Mining Australia in end of year 2019.<br> 
      2. User should be input longitude and latitude from Australia area. <br>
      3. There are 3 main menu in this application.<br>
      \t a. Initial point from hotspot area: user should be select period from detected hotspot area.<br>
      \t b. Initial point from user input: user can input longitude and latitude of hotspot area.<br>
            If number of longitude and latitude doesn't exist in the database, system will display pop up warning message.<br>
            If input initial point more than one, and one of initial point exist in database, system will display pop up warning message that carried information about number of longitude
           and latitude which exist on database<br>
        c. Initial point from CSV file: user can input and visualize trajectory from csv file of hotspot area <br>
      
           "),
      easyClose = FALSE
    ))
  })
  
  output$popup_about <- renderPrint({
    isolate(pop_about())
    
  })
  
  #------------------------------------------------------------------------------------------ 
  #                                     MENU HYSPLIT TRAJECTORY
  #------------------------------------------------------------------------------------------ 
  
  HYSPLIT_Traj <- function(datafile) {
    trajectory <- NULL
    N <- nrow(datafile)
    for (i in 1:N) {
      trajectory1 <-
        hysplit_trajectory(
          lat = datafile$latitude[i],
          lon = datafile$longitude[i],
          height = 10,
          duration = 72,
          days = as.character(as.Date(datafile$acq_date[1],"%Y-%m-%d")),
          daily_hours = 0,
          direction = "forward",
          met_type = "gdas1",
          vert_motion = c(0),
          model_height = 10000,
          extended_met = FALSE,
          met_dir = "E:/Data Meteorologi") #directory data GDAS
      trajectory = rbind(trajectory, trajectory1)
      trajectory1 = NULL
    }
    trajectory_new <- unique(trajectory)
    new_receptor <- rep(1:N, each = 73) #each = duration + 1
    trajectory_new$receptor <- new_receptor
    trajectory_new
  }

  # #Sub menu Simulation
  
  global <- reactiveValues(numVal = 1, numMin = 0, numMax = 3)

  numVal <- reactive({
    if(!is.null(input$jum_init)){
      if(input$jum_init < global$numMin) return(global$numMin)
      if(input$jum_init > global$numMax) return(global$numMax)
      return(input$jum_init)
    }else {#if (input$jum_init==0){
      return(global$numVal)
    }
  })

  output$numInput <- renderUI(numericInput("jum_init","Initial Point (Hotspot) max 3 points", min = global$numMin, max = global$numMax, value = numVal()))


  #----------------------------------------------------------
  create_ui <- reactive({
    input$jum_init

    longitude <- vector("list", input$jum_init)
    for(i in 1:(input$jum_init)){
      longitude[[i]] <- list(br(),
                        numericInput(paste0("latitude[",i, "]"),
                              label=paste0("input latitude", i),
                              value = "",min = -25.274, max=2.125, step=0.001),
                        numericInput(paste0("longitude[",i,"]"),
                              label=paste0("input longitude", i),
                              value = "", min=98.910, max=133.775, step=0.001))
    }
    return(longitude)
  })

  output$Dynamic <- renderUI({
    validate(
      need(input$jum_init!=0, "number of point that entered must be greater than 0")
    )

    create_ui()


  })


  #---------------------- FUNGSI HYSPLIT2
  
  HYSPLIT_Traj2 <- function(datafile2) {
    trajectory <- NULL
    for (i in 1:input$jum_init) {
      trajectory1 <-
        hysplit_trajectory(
          lat = input[[paste0("latitude[", i, "]")]],
          lon = input[[paste0("longitude[", i, "]")]],
          height = 10,
          duration = 72,
          days = as.character(as.Date(datafile2$acq_date[1],"%Y-%m-%d")),
          daily_hours = 0,
          direction = "forward",
          met_type = "gdas1",
          vert_motion = c(0),
          model_height = 10000,
          extended_met = FALSE,
          met_dir = 'E:/Data Meteorologi')
      trajectory = rbind(trajectory, trajectory1)
      trajectory1 = NULL
    }
    trajectory_new <- unique(trajectory)
    new_receptor <- rep(1:input$jum_init, each = 73) #each = duration + 1
    trajectory_new$receptor <- new_receptor
    trajectory_new
  }
  
   #-------------------------------------------------------------------------
   #POPUP WARNING
  
  popupWarning <- reactive ({

    my_vector <- vector("list",input$jum_init)
    my_vector2 <- vector("list",input$jum_init)

    for (i in 1:input$jum_init) {
      my_vector[[i]] <- list(
        lat2 <- input[[paste0("latitude[", i, "]")]]
      )
      my_vector2[[i]] <- list(
        lon2 <- input[[paste0("longitude[", i, "]")]]
      )

    }

    new_vector <- as.numeric(unlist(my_vector))
    new_vector2 <- as.numeric(unlist(my_vector2))

    datafilebaru <- selectedDataInput()
    dateSelect <- as.character(datafilebaru$acq_date)
    

    for (i in 1:input$jum_init) {
      datanew <- 
        dbGetQuery(con,  paste("SELECT * FROM join_table WHERE latitude IN  (", paste(new_vector, collapse = ", "), ") and longitude IN  (", paste(new_vector2, collapse = ", "),")"))
    }
    datanew2 <- nrow(as.data.frame(datanew))
    valueSelect <- (as.data.frame(datanew))
    valuelat <- valueSelect$latitude
    valuelon <- valueSelect$longitude

    if(datanew2==0){
      observeEvent(input$go_sim2, {
        showModal(modalDialog(
          title = "Important message",
          "Position is not included in the hotspot sequential pattern",
          easyClose = FALSE
        ))
      })
    }else{
      observeEvent(input$go_sim2, {
        showModal(modalDialog(
          title = "Important message",
          "Position longitude ", paste(valuelon, collapse = ", ")," and latitude ", paste(valuelat, collapse = ", "), "
          included in the hotspot sequential pattern",
          easyClose = FALSE
        ))
      })
    }


    })

  output$Dynamic2 <- renderPrint({
    input$go_sim2
    isolate(popupWarning())

  })
  
   #----------------------------- Input fail CSV------------------------------
   #--------------------------------------------------------------------------
  observeEvent(input$input_action, {

    infile <- input$datafilecsv

    datacsv <- as.data.frame(read.csv(infile$datapath,header=TRUE,sep=","))

    progress <- shiny::Progress$new(session, min=1, max=5)
    on.exit(progress$close())
    progress$set(message = 'Pre-Process in progress')

    for (i in 1:5) {
      progress$set(value = i)
      Sys.sleep(1)
    }
  })

  selectedDataCsv <- reactive({
    infile <- input$datafilecsv

    datacsv <- as.data.frame(read.csv(infile$datapath,header=TRUE,sep=","))

  })

  checkCsv <- reactive({
    datacheckstatus <- selectedDataCsv()
    N3 <- nrow(as.data.frame(datacheckstatus))

    loncheck <- datacheckstatus$longitude
    latcheck <- datacheckstatus$latitude
    datecheck <- datacheckstatus$acq_date

    for (i in 1:N3) {
      datacheck <- dbGetQuery(con,  paste("SELECT * FROM join_table WHERE latitude IN  (", paste(latcheck, collapse = ", "), ") and longitude IN  (", paste(loncheck, collapse = ", "),") and acq_date='",datecheck,"'"))
    }
  
    datacsvexist<- as.data.frame(datacheck)

    for (i in 1:N3){

      if (any(datacsvexist$longitude == datacheckstatus[i,]$longitude & datacsvexist$latitude == datacheckstatus[i,]$latitude)){
        datacheckstatus[i,4] <- as.character("exist")
      }
      else{
        datacheckstatus[i,4] <- as.character("doesn't exist")
      }

    }
    names(datacheckstatus)[4]<- paste("status_on_sekuens_data")

    datacheckstatusnew <- as.data.frame(datacheckstatus)

  })

  tabelcheck <- reactive({
    datatablecheck <- checkCsv()

    colnames(datatablecheck) <- c('date','logitude','latitude','status_on_sekuens_data','confidence')
    list(datatablecheck = datatablecheck)
  })


  output$checkstatus <- reactive({

    input$go_sim3
    isolate(output$checkstatus <- renderDataTable(
      tabelcheck()$datatablecheck, options = list(paging = TRUE,
                                                  searching = FALSE,
                                                  searchable = FALSE,
                                                  pageLength = 10,
                                                  sort = TRUE)
    ))

  })

  # #---------------------- FUNGSI HYSPLIT3
  #
  HYSPLIT_Traj3 <- function(datafile3) {
    trajectory <- NULL
    N2 <- nrow(datafile3)
    for (i in 1:N2) {
      trajectory1 <-
        hysplit_trajectory(
          lat = datafile3$latitude[i],
          lon = datafile3$longitude[i],
          height = 10,
          duration = 72,
          days = as.character(as.Date(datafile3$acq_date[1],"%Y-%m-%d")),
          daily_hours = 0,
          direction = "forward",
          met_type = "gdas1",
          vert_motion = c(0),
          model_height = 10000,
          extended_met = FALSE,
          met_dir = 'E:/Data Meteorologi')
      trajectory = rbind(trajectory, trajectory1)
      trajectory1 = NULL
    }
    trajectory_new <- unique(trajectory)
    new_receptor <- rep(1:N2, each = 73)
    trajectory_new$receptor <- new_receptor
    trajectory_new
  }


  #--------------------- #Membuat tabel hasil simulasi-------------------
 

  tabel_traj <- reactive({
    datafile <- selectedData()
    hasil_sim_traj = HYSPLIT_Traj(datafile)
    colnames(hasil_sim_traj) <- c('run','receptor','hour_along','traj_dt','lat','lon','height','traj_dt_i','lat_i',
                                   'lon_i','height_i','pressure','theta','air_temp','rainfall','mixdepth','rh',
                                   'sp_humidity','h2o_mixrate','terr_msl','sun_flux')
    list(hasil_sim_traj = hasil_sim_traj)
  })

  #--user input
  tabel_traj2 <- reactive({
    datafile2 <- selectedDataInput()
    hasil_sim_traj2 = HYSPLIT_Traj2(datafile2)
    colnames(hasil_sim_traj2) <- c('run','receptor','hour_along','traj_dt','lat','lon','height','traj_dt_i','lat_i',
                                   'lon_i','height_i','pressure','theta','air_temp','rainfall','mixdepth','rh',
                                   'sp_humidity','h2o_mixrate','terr_msl','sun_flux')
    list(hasil_sim_traj2 = hasil_sim_traj2)
  })

  #--file csv
  tabel_traj3 <- reactive({
    datafile3 <- selectedDataCsv()
    hasil_sim_traj3 = HYSPLIT_Traj3(datafile3)
    colnames(hasil_sim_traj3) <- c('run','receptor','hour_along','traj_dt','lat','lon','height','traj_dt_i','lat_i',
                                  'lon_i','height_i','pressure','theta','air_temp','rainfall','mixdepth','rh',
                                  'sp_humidity','h2o_mixrate','terr_msl','sun_flux')
    list(hasil_sim_traj3 = hasil_sim_traj3)
  })

  #------------------------output------------------------------


  output$caption_traj <- renderText({
    if (input$date_Period == "")
      paste("Haze Trajectory Simulation using HYSPLIT Model")
    else
      paste("Haze Trajectory Simulation using HYSPLIT Model in ",title_periode()," ");
  })

  output$caption_traj2 <- renderText({
    if (input$date_PeriodInput == "")
      paste("Haze Trajectory Simulation using HYSPLIT Model, on Initial Point from User's Input")
    else
      paste("Haze Trajectory Simulation using HYSPLIT Model, on Initial Point from User's Input ",title_periodeInput()," ");
  })

  output$caption_traj3 <- renderText({
    paste("Haze Trajectory Simulation using HYSPLIT Model, on Initial Point from CSV File");
  })

  output$titlecheck <- renderText({
    input$go_sim3
    paste("Data from CSV File");
  })
  output$titleSimulation <- renderText({
    input$go_sim3
    paste("Result of Simulation");
  })

  #Output of Tab Panel Simulation in data Simulation Trajectory
  output$simulation_traj <- renderPrint({
    input$go_sim
    if (input$go_sim == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Simulation Results of Trajectory in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$simulation_traj <- renderDataTable(
      tabel_traj()$hasil_sim_traj, options = list(paging = TRUE,
                                                  searching = FALSE,
                                                  searchable = FALSE,
                                                  pageLength = 10,
                                                  sort = TRUE)
    ))
    
  })
  
  output$simulation_trajinput <- renderPrint({
    input$go_sim2
    if (input$go_sim2 == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Simulation Results of Trajectory in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$simulation_trajinput <- renderDataTable(
      tabel_traj2()$hasil_sim_traj2, options = list(paging = TRUE,
                                                    searching = FALSE,
                                                    searchable = FALSE,
                                                    pageLength = 10,
                                                    sort = TRUE)
    ))
    
  })
  
  output$simulation_trajcsv <- renderPrint({
    input$go_sim3
    if (input$go_sim3 == 0)
      return("Please click 'Start  Simulation' button to see the result")
    else
      #PROGRESS BAR
      progress <- shiny::Progress$new(session, min = 1, max = 2)
    on.exit(progress$close())
    progress$set(message = 'Description of Simulation Results of Trajectory in progress')
    
    for (i in 1:2) {
      progress$set(value = i)
      Sys.sleep(1)
    }
    isolate(output$simulation_trajcsv <- renderDataTable(
      tabel_traj3()$hasil_sim_traj3, options = list(paging = TRUE,
                                                    searching = FALSE,
                                                    searchable = FALSE,
                                                    pageLength = 10,
                                                    sort = TRUE)
    ))
    
  })

  # #------------------------ PLOTTING

  plot_leaflet_traj <- reactive({
    dataplot <- tabel_traj()$hasil_sim_traj
    dataplot$color <- as.character(dataplot$receptor)
    dataplot$color[dataplot$color == "1"] <- "#F44336" #"red"
    dataplot$color[dataplot$color == "2"] <- "#4CAF50" #green
    dataplot$color[dataplot$color == "3"] <- "aqua"
    dataplot$color[dataplot$color == "4"] <- "#2196F3" #blue
    dataplot$color[dataplot$color == "5"] <- "#FF9800" #orange
    dataplot$color[dataplot$color == "6"] <- "#FFEB3B" #yellow
    dataplot$color[dataplot$color == "7"] <- "#673AB7" #deep purple
    dataplot$color[dataplot$color == "8"] <- "#f1c40f" #sunflower
    dataplot$color[dataplot$color == "9"] <- "#795548" #brown
    dataplot$color[dataplot$color == "10"] <- "#8BC34A" #light green
    dataplot$color[dataplot$color == "11"] <- "#BA68C8" #purple
    dataplot$color[dataplot$color == "12"] <- "#607D8B" #blue grey
    dataplot$color[dataplot$color == "13"] <- "#009688" #teal
    dataplot$color[dataplot$color == "14"] <- "#E91E63" #pink
    dataplot$color[dataplot$color == "15"] <- "#3F51B5" #indigo

    basedreceptor <- (paste("Receptor : ", dataplot$receptor, "<br/>"))
    longitudepaste <- (paste("Longitude : ", dataplot$lon, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$lat, "<br/>"))
    heightpaste <- (paste("Height: ", dataplot$height, "<br/>"))
    pressurepaste <- (paste("Pressure : ", dataplot$pressure, "<br/>"))
    datepaste <- (paste("Date : ", dataplot$traj_dt, "<br/>"))
    popup1 <- (paste(basedreceptor,longitudepaste, latitudepaste, heightpaste, pressurepaste, datepaste, sep = ""))

    mymap = leaflet()

    mymap <-
      addProviderTiles(
        mymap,
        "OpenStreetMap",
        group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))


    mymap=addCircleMarkers(mymap, lng = dataplot$lon, lat = dataplot$lat, color = dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = 1.5, popup = popup1)

    leafIcons <- icons(iconUrl = "http://findicons.com/files/icons/2203/alcohol/32/burn.png",18,18)
    point_indicator<-subset(dataplot,dataplot$hour_along==0)
    mymap = addMarkers(mymap, lng = point_indicator$lon, lat = point_indicator$lat, icon = leafIcons)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
    html_legend2 <- "<img src='http://findicons.com/files/icons/2203/alcohol/32/burn.png'>hotspot"
    mymap <- addControl(mymap,html = html_legend2, position = "bottomleft")
  })

  plot_leaflet_trajinput <- reactive({
    dataplot <- tabel_traj2()$hasil_sim_traj2
    dataplot$color <- as.character(dataplot$receptor)
    dataplot$color[dataplot$color == "1"] <- "#F44336" #"red"
    dataplot$color[dataplot$color == "2"] <- "#4CAF50" #green
    dataplot$color[dataplot$color == "3"] <- "aqua"
    dataplot$color[dataplot$color == "4"] <- "#2196F3" #blue
    dataplot$color[dataplot$color == "5"] <- "#FF9800" #orange
    dataplot$color[dataplot$color == "6"] <- "#FFEB3B" #yellow
    dataplot$color[dataplot$color == "7"] <- "#673AB7" #deep purple
    dataplot$color[dataplot$color == "8"] <- "black"
    dataplot$color[dataplot$color == "9"] <- "#795548" #brown
    dataplot$color[dataplot$color == "10"] <- "#8BC34A" #light green
    dataplot$color[dataplot$color == "11"] <- "#BA68C8" #purple
    dataplot$color[dataplot$color == "12"] <- "#607D8B" #blue grey
    dataplot$color[dataplot$color == "13"] <- "#009688" #teal
    dataplot$color[dataplot$color == "14"] <- "#E91E63" #pink
    dataplot$color[dataplot$color == "15"] <- "#3F51B5" #indigo

    basedreceptor <- (paste("Receptor : ", dataplot$receptor, "<br/>"))
    longitudepaste <- (paste("Longitude : ", dataplot$lon, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$lat, "<br/>"))
    heightpaste <- (paste("Height: ", dataplot$height, "<br/>"))
    pressurepaste <- (paste("Pressure : ", dataplot$pressure, "<br/>"))
    datepaste <- (paste("Date : ", dataplot$traj_dt, "<br/>"))
    popup1 <- (paste(basedreceptor,longitudepaste, latitudepaste, heightpaste, pressurepaste, datepaste, sep = ""))

    mymap = leaflet()

    mymap <-
      addProviderTiles(
        mymap,
        "OpenStreetMap",
        group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))


    mymap=addCircleMarkers(mymap, lng = dataplot$lon, lat = dataplot$lat, color = dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = 1.5, popup = popup1)

    leafIcons <- icons(iconUrl = "http://findicons.com/files/icons/2203/alcohol/32/burn.png",18,18)
    point_indicator<-subset(dataplot,dataplot$hour_along==0)
    mymap = addMarkers(mymap, lng = point_indicator$lon, lat = point_indicator$lat, icon = leafIcons)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
    html_legend2 <- "<img src='http://findicons.com/files/icons/2203/alcohol/32/burn.png'>hotspot"
    mymap <- addControl(mymap,html = html_legend2, position = "bottomleft")
  })

  plot_leaflet_trajcsv <- reactive({
    dataplot <- tabel_traj3()$hasil_sim_traj3
    dataplot$color <- as.character(dataplot$receptor)
    dataplot$color[dataplot$color == "1"] <- "#F44336" #"red"
    dataplot$color[dataplot$color == "2"] <- "#4CAF50" #green
    dataplot$color[dataplot$color == "3"] <- "aqua"
    dataplot$color[dataplot$color == "4"] <- "#2196F3" #blue
    dataplot$color[dataplot$color == "5"] <- "#FF9800" #orange
    dataplot$color[dataplot$color == "6"] <- "#FFEB3B" #yellow
    dataplot$color[dataplot$color == "7"] <- "#673AB7" #deep purple
    dataplot$color[dataplot$color == "8"] <- "#f1c40f" #sunflower
    dataplot$color[dataplot$color == "9"] <- "#795548" #brown
    dataplot$color[dataplot$color == "10"] <- "#8BC34A" #light green
    dataplot$color[dataplot$color == "11"] <- "#BA68C8" #purple
    dataplot$color[dataplot$color == "12"] <- "#607D8B" #blue grey
    dataplot$color[dataplot$color == "13"] <- "#009688" #teal
    dataplot$color[dataplot$color == "14"] <- "#E91E63" #pink
    dataplot$color[dataplot$color == "15"] <- "#3F51B5" #indigo

    basedreceptor <- (paste("Receptor : ", dataplot$receptor, "<br/>"))
    longitudepaste <- (paste("Longitude : ", dataplot$lon, "<br/>"))
    latitudepaste <- (paste("Latitude : ", dataplot$lat, "<br/>"))
    heightpaste <- (paste("Height: ", dataplot$height, "<br/>"))
    pressurepaste <- (paste("Pressure : ", dataplot$pressure, "<br/>"))
    datepaste <- (paste("Date : ", dataplot$traj_dt, "<br/>"))
    popup1 <- (paste(basedreceptor,longitudepaste, latitudepaste, heightpaste, pressurepaste, datepaste, sep = ""))

    mymap = leaflet()

    mymap <-
      addProviderTiles(
        mymap,
        "OpenStreetMap",
        group = "OpenStreetMap")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.DarkMatter",
        group = "CartoDB Dark Matter")
    mymap <-
      addProviderTiles(
        mymap,
        "CartoDB.Positron",
        group = "CartoDB Positron")
    mymap <-
      addProviderTiles(
        mymap,
        "Esri.WorldTerrain",
        group = "ESRI World Terrain")
    mymap <-
      addProviderTiles(
        mymap,
        "Stamen.Toner",
        group = "Stamen Toner")
    mymap <-
      addLayersControl(
        mymap,
        position = "topright",
        baseGroups = c("CartoDB Positron",
                       "CartoDB Dark Matter",
                       "Stamen Toner",
                       "ESRI World Terrain"))


    mymap=addCircleMarkers(mymap, lng = dataplot$lon, lat = dataplot$lat, color = dataplot$color, fillOpacity = 1, stroke = FALSE, weight = 7, fill = TRUE, opacity = 1, radius = 1.5, popup = popup1)

    leafIcons <- icons(iconUrl = "http://findicons.com/files/icons/2203/alcohol/32/burn.png",18,18)
    point_indicator<-subset(dataplot,dataplot$hour_along==0)
    mymap = addMarkers(mymap, lng = point_indicator$lon, lat = point_indicator$lat, icon = leafIcons)
    mymap = addScaleBar(mymap, position = c("bottomleft"), options = scaleBarOptions())
    html_legend <- "<img src='http://findicons.com/files/icons/2083/go_green_web/64/compass.png'>"
    mymap <- addControl(mymap,html = html_legend, position = "topright")
    html_legend2 <- "<img src='http://findicons.com/files/icons/2203/alcohol/32/burn.png'>hotspot"
    mymap <- addControl(mymap,html = html_legend2, position = "bottomleft")
  })

  output$showplotleaflet <- renderLeaflet({
     input$go_sim
     if (input$go_sim == 0)
       return("Please click 'Start  Simulation' button to see the result")
     else
       #PROGRESS BAR
       progress <- shiny::Progress$new(session, min = 1, max = 2)
     on.exit(progress$close())
     progress$set(message = 'Plotting in progress')
  
     for (i in 1:2) {
       progress$set(value = i)
       Sys.sleep(1)
     }
     isolate(plot_leaflet_traj())
   })

   output$showplotleafletinput <- renderLeaflet({
     input$go_sim2
     if (input$go_sim2 == 0)
       return("Please click 'Start  Simulation' button to see the result")
     else
       #PROGRESS BAR
       progress <- shiny::Progress$new(session, min = 1, max = 2)
     on.exit(progress$close())
     progress$set(message = 'Plotting in progress')
  
     for (i in 1:2) {
       progress$set(value = i)
       Sys.sleep(1)
     }
     isolate(plot_leaflet_trajinput())
   })
  
   output$showplotleafletcsv <- renderLeaflet({
     input$go_sim3
     if (input$go_sim3 == 0)
       return("Please click 'Start  Simulation' button to see the result")
     else
       #PROGRESS BAR
       progress <- shiny::Progress$new(session, min = 1, max = 2)
     on.exit(progress$close())
     progress$set(message = 'Plotting in progress')
  
     for (i in 1:2) {
       progress$set(value = i)
       Sys.sleep(1)
     }
     isolate(plot_leaflet_trajcsv())
   })

})