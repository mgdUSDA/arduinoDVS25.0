#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 200*1024^2)

suppressMessages(library(readr))
suppressMessages(library(shiny))

suppressMessages(library(dplyr))
suppressMessages(library(GGally))
suppressMessages(library(ggplot2))
suppressMessages(library(ggpubr))
suppressMessages(library(ggsci))
suppressMessages(library(gridExtra))
suppressMessages(library(kableExtra))
suppressMessages(library(scales))
suppressMessages(library(tidyr))

#library(Cairo)   # For nicer ggplot2 output when deployed on Linux



# Define ggsci palettes

d3 <- list()
Npal <- length(pal_d3("category20")(20))
for (i in 1:Npal) {
  d3[[i]] <- pal_d3("category20")(20)[i]
}


uchicago <- list()
Npal <- length(pal_uchicago()(9))
for (i in 1:Npal) {
  uchicago[[i]] <- pal_uchicago()(9)[i]
}

uchicago_l <- list()
Npal <- length(pal_uchicago("light")(9))
for (i in 1:Npal) {
  uchicago_l[[i]] <- pal_uchicago("light")(9)[i]
}

# Based on arduinoDVS2.3 Shiny App

appName <- "arduinoDVS"
appAbbr <- "aDVS"
appVer <- "5.0.1"
appID <- paste(appAbbr, appVer, sep = "")

nearThreshold <- 0.1

colorPath <- "D:/T3620 dusaire/dataprocessing/R/Rrepo/data-parser/shiny/DataPlotInteractive/DataPlotColorPallete.csv"
unitsPath <- "D:/T3620 dusaire/dataprocessing/R/Rrepo/data-parser/shiny/DataPlotInteractive/DataPlotUnits.csv"


#---------------------------------------------------------------------
# Martin duSaire modification of RStudio gallery Plot Interactions
# example.
# Added the side panel controls, data import and subsampling code.
#
#  Expects a long datafile with column headers: time, id, measure, reading, port.
#   Any other columns in the data file will not be used in processing, but will
#   be exported in the saved data file.
#
# Still to do: file and plot save.
#   Assign name to saved data file
#   Include header for row number in exported data file
#   Assign name and save plot .png
#   sub-sampling slider
#   Incorporate basic model fitting using nls() with sliders
#
#
#   How to handle variables and colors not listed in DataPlotColorPalletes.csv and optimize loading of these info files
#   In order to extend to Ohaus MB45 data and others, how to execute R-scripts from Shiny App.
#---------------------------------------------------------------------





ui <- fluidPage(
  sidebarPanel(width = 2,
               radioButtons("dataFormat", "Data Format",
                            c("Arduino DVS data" = "aDVS"),
                            selected = "aDVS"
               ),
               actionButton("loaddata", "Load data"),
               # fileInput('file1', 'Choose CSV File',
               #           accept=c('text/csv', 
               #                    'text/comma-separated-values,text/plain', 
               #                    '.csv')),
               HTML("<hr>"),               
               actionButton("appenddata", "Append data"),
               # fileInput('file1', 'Choose CSV File',
               #           accept=c('text/csv', 
               #                    'text/comma-separated-values,text/plain', 
               #                    '.csv')),
               tags$hr(),
               
               selectInput(inputId = 'sampleid',
                           label = 'Sample ID',
                           choices = NULL,
                           selected = NULL,
                           multiple = TRUE
               ),
               selectInput(inputId = 'measuretype',
                           label = 'Measure type',
                           choices = NULL,
                           selected = NULL,
                           multiple = TRUE
               ),
               selectInput(inputId = 'sensorid',
                           label = 'Sensor ID',
                           choices = NULL,
                           selected = NULL,
                           multiple = TRUE
               ),               
               # selectInput(inputId = 'ycol',
               #             label = 'Y variable',
               #             choices = NULL,
               #             selected = NULL,
               #             multiple = TRUE
               # ),
               # selectInput(inputId = 'globalfilter',
               #             label = 'Global filter',
               #             choices = NULL,
               #             selected = NULL,
               #             multiple = TRUE
               # ),
               # selectInput(inputId = 'factor',
               #             label = 'Factor',
               #             choices = NULL,
               #             selected = NULL
               # ),
               # selectInput(inputId = 'factorFilter',
               #             label = 'Factor filter',
               #             choices = NULL,
               #             selected = NULL,
               #             multiple = TRUE
               # ),
               tags$hr(),
               
               # sliderInput("sampleRate", "Sample Rate", min = 1, max = 20, value = 1, step = 1), 
               # tags$hr(),
               
               checkboxGroupInput("sampleID", "sample data to plot",
                                  c("no data")),
               #    ,
               #                       c("Item A", "Item B", "Item C")),
               
               selectInput(inputId = 'modelType',
                           label = 'Model type',
                           choices = c("none" = 0, "linear model" = 1, "smooth (loess)" = 2),
                           selected = 0
               ),
               tags$hr(),
               radioButtons("outputData", "Data to save",
                            c("Wide data" = "wData", "Plot data" = "pData", "Selected points" = "sData"),
                            selected = "pData",
                            inline = TRUE
               ),               
               tags$div(style="display:inline-block", title="CTRL+click to save plot", downloadButton("savePlot", "Save PNG")),
               tags$div(style="display:inline-block", title="CTRL+click to save data", downloadButton("saveData", "Save data")),
               tags$button(
                 style ="display:inline-block",
                 title="Browser tab will remain open",
                 id = 'close',
                 type = "button",
                 class = "btn action-button",
                 onclick = "setTimeout(function(){window.close();},500);",  # close browser
                 "Stop App"
               )    
               
  ),
  fluidRow(
    column(width = 4,
           plotOutput("plot1", width = "auto", height = 500,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      # click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
                      # ,
                      # resetOnNew = TRUE
           )
    ),
    column(width = 5,
           plotOutput("plot2", width = "auto", height = 900, hover = hoverOpts(id = "plot2_hover", delay = 0)
                      
           )
    )
    
  ),
  tags$hr(),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Info", verbatimTextOutput('viewInfo')),
      tabPanel("Raw data", DT::dataTableOutput('viewRawData')),
      # tabPanel("Wide data", DT::dataTableOutput('viewWideData')),
      tabPanel("Plot data", DT::dataTableOutput('viewPlotData')),
      # tabPanel("Compounds", DT::dataTableOutput('viewCompounds')),
      tabPanel("Selected data", DT::dataTableOutput('viewSelectedPlotData')),
      tabPanel("Selected data stats",  plotOutput("boxPlotSelected", width = "auto", height = 500,
                                                  # Equivalent to: click = clickOpts(id = "plot_click")
                                                  # click = "plot1_click",
                                                  # brush = brushOpts(
                                                  #  id = "plot1_brush"
      ),
      tags$hr(),
      # plotOutput("linePlotISHeight", width = "auto", height = 500,
      # ),      
      DT::dataTableOutput('viewBoxPlotStats')),
      tabPanel("Multiple tables", DT::dataTableOutput('multipleTables'),
               "Table1", DT::dataTableOutput('isasummary'),
               "Table2", DT::dataTableOutput('ishsummary')
      ),
      tabPanel("Signature Plots", plotOutput("sigPlot", width = "auto", height = 500)
      ),
      tabPanel("Class Plots", plotOutput("classPlot", width = "auto", height = 500)           
      )
    )
  ),
  uiOutput("my_tooltip")
)

server <- function(input, output, session) {
  
  # Server variables
  
  
  d3 <- list()
  Npal <- length(pal_d3("category20")(20))
  for (i in 1:Npal) {
    d3[[i]] <- pal_d3("category20")(20)[i]
  }
  
  
  observe({
    if (input$dataFormat == 'aDVS') {
      cat("Arduino DVS data")
      
    } else {
      cat("XYZ")
      cat("...", input$dataFormat)
    }
  })
  
  fileInfo <- reactiveValues(name = NULL, path = NULL)
  ranges <- reactiveValues(x = NULL, y = NULL)
  near <- reactiveValues(x = NULL, y = NULL)
  values <- reactiveValues(data = NULL)
  
  pColors <- reactive({
    tempColors <- read.csv(colorPath, sep = ",", header = TRUE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE)
    pColors <- tempColors$qualitative
    names(pColors) <- tempColors$measure
    pColors
  })
  
  mUnits <- reactive({
    tempUnits <- read.csv(unitsPath, sep = ",", header = TRUE, as.is = TRUE, strip.white = TRUE, blank.lines.skip = TRUE)
    mUnits <- tempUnits$units
    names(mUnits) <- tempUnits$measure
    mUnits
  })
  
  trimWhiteSpace <- function(x) {
    x <- sub("^\\s+", "", x) # trim leading white spaces
    x <- sub("\\s+$", "", x) # trim trailing white spaces
    return(x)  
  }
  
  processData <- function() {
    
    withProgress(message = 'Processing Data', value = 0, {
      #Broken into 5 key steps
      progressSteps <- 6
      
      cat("\n processData()\n")
      
      # Constants
      
      STANDARD <- 36280.00 # mg  tin cup plus printed cover
      
      kfactorThreshold <- 0.20 # Highest acceptable relative change between successive kfactor values.  
      
      # Variables
      
      kfactorCalc <- 13
      kfactorCalcNew <- 13
      kfactorNew <- FALSE
      
      
      # Set expected data folder
      
      nodename <- Sys.info()["nodename"]
      dir1 <- "C:/Users/martin.dusaire/OneDrive - USDA/Documents/55KAS222/arduinoDVS/data/"
      
      if (is.na(file.info(dir1)$isdir) == FALSE) {
        
        #Data folder exists.  Do nothing
        
      } else {
        
        #Data folder does not exist.  Set folder to root.
        
        dir1 <- "C:/"
      }
      
      dirWork <- getwd()
      
      # Choose data file and set up results folders
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/progressSteps, detail = "Choose data file")
      
      cat("\n > choose.files()\n")
      
      infile <- choose.files(paste(dir1, "*.csv", sep = ""), caption = "Choose Arduino Datafile (Not RAW data)")
      infile <- gsub("\\\\", "/", infile)
      dataFileName <- toupper(strsplit(infile, "/")[[1]][length(strsplit(infile, "/")[[1]])])
      
      # infile <- params$infile
      # dataFileName <- params$dataFileName
      filePath <- strsplit(infile, dataFileName)[[1]][[1]]
      
      # Check for FaintingGoats sample info.csv
      
      cat(paste0("\n", Sys.time()," > get sample info file\n"))
      
      sampleInfoFilePath <- paste(filePath, "dvs sample info.csv", sep = "")
      
      if (!file.exists(sampleInfoFilePath)) {
        sampleInfoFilePath <- choose.files(paste(dir1, "*.csv", sep = ""), caption = "Choose arduinoDVS sample info file")
      }
      
      cat(paste0("\n", Sys.time()," > choose files\n"))    
      
      # cat("Imported MSD Library Report data from:\n\n")
      # dataFolder <- choose.dir(default = dir1, caption = "Select folder")
      # seqName <- strsplit(dataFolder, "\\\\")[[1]][length(strsplit(dataFolder, "\\\\")[[1]])]
      
      reportTime <- Sys.time()
      reportTime <- sub(":", "", reportTime)
      reportTime <- sub(":", "", reportTime) # Once for each ":" in the time.
      reportTime <- strsplit(reportTime, "\\.")[[1]][1] # Remove decimal seconds from time.
      
      logfile <- gsub(".csv", ".log", infile, ignore.case = TRUE)
      modelDataSubOutfile <- gsub(".csv", paste("_model_data_", reportTime, ".csv", sep = ""), infile, ignore.case = TRUE)
      outfileFull <- gsub(".csv", paste("full_", reportTime, ".csv", sep = ""), infile, ignore.case = TRUE)
      modelResultsOutfileAll <- paste(filePath, "BCSeries03_model_results.csv", sep = "")
      
      logfileName <- paste(filePath, appID, ".log", sep = "")    
      
      dirOut <- gsub("data", appID, filePath, ignore.case = TRUE)
      #fName <- paste("/", strsplit(infile, "/")[[1]][length(strsplit(infile, "/")[[1]])], sep = "")
      #dataFolder <- strsplit(infile, fName)[[1]][1]
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/progressSteps, detail = "Create output directory")
      
      
      dirOut <- paste(dirOut, "/", reportTime, "/", sep="")
      if (is.na(file.info(dirOut)$isdir) == FALSE) {
        
        #Output folder exists.  Do nothing
        
      } else {
        
        #Output folder does not exist.  Create new folder.
        
        dir.create(dirOut, recursive = TRUE)
      }
      repfName <- paste(dirOut, "VOCReport_", reportTime, ".txt", sep = "")
      
      # Get computer name
      
      nodename <- Sys.info()["nodename"]
      # cat("Computer name:", nodename, "\n", file = repfName, append = TRUE)
      # cat("Parser version:", appVer, "\n", file = repfName, append = TRUE)
      # cat("Sequence name:", filePath, "\n", file = repfName, append = TRUE)
      # cat("Date:", date(), "\n", file = repfName, append = TRUE)
      
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/progressSteps, detail = "Read chamber data")
      
      # Read DVS data and sample info
      
      cat("\n > read chamber data and sample info\n")
      
      # Check for missing header row or incorrect header row.
      # The data files prior to 061124 may have an extra column (11)
      # due to an extra comma added to the header by the Arduino DVS program.
      
      # 11/22/2024 : Removed the extra comma in many, but not all
      # of the data files.
      
      # infile <- "C:/Users/martin.dusaire/OneDrive - USDA/Documents/55KAS222/arduinoDVS/data/BAR0517.CSV"
      
      cat(paste0("\n", Sys.time()," > read chamber data\n"))
      
      firstLine <- readLines(infile, n = 1)
      
      if (length(grep("datetime", firstLine)) > 0) {
        
        # The data file has a header, which I will ignore because some headers
        # Are incorrect. Ignore the header in case it is incorrect, and skip the
        # first line
        
        chamberData <- read.csv(infile, header = FALSE, skip = 1, strip.white = TRUE, as.is = TRUE)
        
      } else {
        
        # The file has no header. Read the data in the first line.
        
        chamberData <- read.csv(infile, header = FALSE, strip.white = TRUE, as.is = TRUE)
      }
      
      # Assign the correct column names to the data file.
      
      colnames(chamberData) <- c("datetime", "measureType", "sampleID", "count", "sensorID", "measure", "sd", "hxAttempts", "kfactor", "units")
      
      cat(paste0("\n", Sys.time()," > read sample info\n")) 
      
      sampleInfo <- read.csv(sampleInfoFilePath, header = TRUE, strip.white = TRUE, as.is = TRUE)
      
      # Configure sample info for display using kableExtra in dataframe sampleInfo
      
      colnames(sampleInfo) <- c("Datafile", "Sample ID", "Sample Type", "Experiment Type",
                                "Preparation Method", "Preparation Time", "stdMass", "tinMass",
                                "End Mass", "ovenDryMass", "Temperature", "hxName",
                                "End Date")
      
      
      #    datafile	sampleID	sampleType	experimentType	preparationMethod	prepartionTime	stdMass	tin	endMass	ovenDry	temperature	hxName	endDate
      
      
      thisSampleInfo <- data.frame(sampleInfo[which(toupper(sampleInfo$Datafile) == dataFileName),])
      
      if (!is.na(thisSampleInfo$ovenDryMass)) {
        
        # Sample oven dry mass is available:  Calculate water content.
        
        waterContentCalculation <- TRUE
        ovenDryMass <- thisSampleInfo$ovenDryMass
      }
      
      cat("\n  > sample info:\n")
      # print(sampleInfo)  
      
      #   sampleInfo <- sampleInfoFG %>%
      # filter(toupper(Datafile) == dataFileName)
      
      #STANDARD <- thisSampleInfo$stdMass
      
      stdTinMass <- STANDARD + thisSampleInfo$tinMass
      
      cat("sampleInfo:\n")
      cat("   > ")
      cat(STANDARD)
      cat("\n")
      cat("   > ")
      cat(stdTinMass)
      cat("\n")
      
      # Process chamber data chamberData, and store in dataframe chDataMass
      
      cat("\n > chamberData:\n")
      cat("   > ")
      print(head(chamberData))  
      cat("\n")
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/progressSteps, detail = "Transform date-time to POSIXct format")
      
      
      cat(paste0("\n", Sys.time()," > transform datetime to POSIXct\n"))
      
      chamberData$datetime <- as.POSIXct(strptime(chamberData$datetime, "%m/%d/%y %H:%M:%S"))
      # hxList <- sampleInfo$hxName  This may be useful if there are multiple strain gauges in the system
      
      # This is most likely legacy code that can be deleted
      
      # if (!is.na(chamberData$kfactor[1])) {
      #   k_factor <- chamberData$kfactor[1]
      # } else {
      #   k_factor <- 88888.88  # Note that in previous versions the k_factor was less than 1, ie, the reciprocal of the current k_factor
      # }
      # 
      # cat("\n > chamberData:\n")
      # cat("   > ")
      # print(k_factor)  
      # cat("\n") 
      
      cat(paste0("\n", Sys.time()," > get unique sample datetimes\n"))
      
      #sampleTimes <- data.frame(datetime = unique(chamberData$datetime))
      sampleTimes <- unique(chamberData$datetime)
      # mass <- NA
      # hx1Diff <- NA
      
      # Increment the progress bar, and update the detail text.
      incProgress(1/progressSteps, detail = "Calculate sample masses time series")
      
      
      cat(paste0("\n", Sys.time()," > calculate masses\n"))
      
      massData <- chamberData[which(chamberData$count == 20), ] %>%
        select(datetime, measureType, measure) %>%
        spread(measureType, measure)  
      
      kfactorCalc <- (massData$STANDARD - massData$TARESTANDARD) / STANDARD
      kfactorError <- abs(diff(kfactorCalc) / kfactorCalc[1:(length(kfactorCalc) - 1)]) > kfactorThreshold
      
      cat("\n > calculate masses:\n")
      cat("   > ")
      print(kfactorCalc)  
      cat("\n") 
      
      
      # Check kfactor for errors in standard mass measurements
      
      while (sum(kfactorError) > 0) {
        
        # Replace bad kfactor with previous kfactor, prsumably the earlier kfactor
        # will be correct. What happens if the first kfactor is bad?
        
        kfactorCalc[kfactorError] <- kfactorCalc[which(kfactorError) - 1]
        kfactorError <- abs(diff(kfactorCalc) / kfactorCalc[1:(length(kfactorCalc) - 1)]) > kfactorThreshold
        
      }
      
      sampleMass <- ((massData$WEIGH - massData$TARE) / kfactorCalc) - stdTinMass
      
      if (waterContentCalculation) {
        
        waterContent <- 100 * (sampleMass - ovenDryMass) / ovenDryMass
      }
      
      mData <- data.frame(datetime = massData$datetime,
                          measureType = "sampleMass",
                          sampleID = chamberData$sampleID[1],
                          count = 20,
                          sensorID = "hx1",
                          measure = sampleMass,
                          sd = NA,
                          hxAttempts = NA,
                          kfactor = kfactorCalc,
                          units = "mg")
      
      wcData <- data.frame(datetime = massData$datetime,
                           measureType = "waterContent",
                           sampleID = chamberData$sampleID[1],
                           count = 20,
                           sensorID = "wc",
                           measure = waterContent,
                           sd = NA,
                           hxAttempts = NA,
                           kfactor = kfactorCalc,
                           units = "pcnt")
      
      
      kfactorData <- data.frame(datetime = massData$datetime,
                                measureType = "sampleMass",
                                sampleID = chamberData$sampleID[1],
                                count = 20,
                                sensorID = "kfactor",
                                measure = kfactorCalc,
                                sd = NA,
                                hxAttempts = NA,
                                kfactor = kfactorCalc,
                                units = NA)
      
      
      allData <- rbind(chamberData, mData, wcData, kfactorData)
      
      sampleTime <- NA
      t0 <- min(allData$datetime)
      timeUnits <- "hours"
      allData$sampleTime <- as.numeric(difftime(allData$datetime, t0, units = timeUnits))
      
      
      list(dataFileName, allData, chamberData)
      
    }) # end of data processing withProgress()
    
    
  } # end of processData()
  
  
  dvsData <- eventReactive(input$loaddata, {
    
    # Process arduino DVS data.  Select data file and load sample info file.
    
    processData()
  })
  
  
  
  dataFileName <- reactive({
    dvsData()[[1]]
  })
  
  allData <- reactive({
    dvsData()[[2]]
  })
  
  rawData <- reactive({
    dvsData()[[3]]
  })
  
  # summaryData <- reactive({
  #   vocData()[[1]]
  # })
  # 
  # allTICData <- reactive({
  #   vocData()[[2]]
  # })
  # 
  # ISRTsummary <- reactive({
  #   vocData()[[3]]
  # })
  # 
  # ISAsummary <- reactive({
  #   vocData()[[4]]
  # })
  # 
  # ISHsummary <- reactive({
  #   vocData()[[5]]
  # })
  # 
  # compoundClasses <- reactive({
  #   vocData()[[6]]
  # }) 
  # 
  # sampleLookup <- reactive({
  #   vocData()[[7]]
  # }) 
  
  output$ui <- renderUI({
    if(is.null(input$dataFormat))
      return()
    
    # Depending on input$dataFormat, generate different UI components
    
    switch(input$dataFormat,
           
           "aDVS" = ,
           "sVOC" = 
    )
    
    
    
  })
  
  
  sampleIDList <- reactive({
    unique(allData()$sampleID)
  })
  
  measureTypeList <- reactive({
    unique(allData()$measureType)
  })
  
  sensorIDList <- reactive({
    unique(allData()$sensorID)
  })
  
  
  factorValues <- reactive({
    if (is.null(input$sampleID) || is.null(allData())) {
      return(NULL)
    } else {
      unique(allData()[, input$sampleID], na.rm = TRUE)
    }
  })
  
  
  
  
  plotData <- reactive({
    
    # Combine the selected variables into a new data frame
    
    validate(
      need(!is.null(allData()), "No data available for plotting.")
    )
    if (is.null(input$measuretype) & is.null(input$sensorid)) {
      validate("Must choose a measurement and sensor.")
    }
    pd <- allData() %>%
      filter(measureType %in% input$measuretype) %>%
      filter(sensorID %in% input$sensorid) %>%
      filter(measure > 0)
    
    # if (is.null(input$sampleid)) {
    #   pd[which(pd[, "measureType"] %in% input$measuretype),]
    # } else {
    #   pd[which(pd[, "sensorID"] %in% input$sensorid),]        
    # }
  })
  
  selectedPlotData <- reactive({
    
    # Combine the selected plotData() into a new data frame
    
    validate(
      need(!is.null(plotData()), "No data available for plotting.")
    )
    if (is.null(input$measuretype) & is.null(input$sensorid)) {
      validate("Must choose a measurement and sensor.")
    }
    
    plotData() %>%
      filter(datetime >= ranges$x[1] & datetime <= ranges$x[2]) %>%
      filter(sensorID %in% input$sensorid) %>%
      filter(measure >= ranges$y[1] & measure <= ranges$y[2])
  })
  
  plotSensorIDList <- reactive({
    unique(input$sensorid)
  })
  
  
  plotUnitsList <- reactive({
    unique(plotData()$units)
  })
  
  # ticPlotData <- reactive({
  #   
  #   # Combine the selected variables into a new data frame
  #   
  #   validate(
  #     need(!is.null(allTICData()), "No data available for plotting.")
  #   )
  #   tpd <- allTICData() 
  #   if (is.null(input$sampleid) & is.null(input$filename )) {
  #     validate("Must choose a sample or file name.")
  #   }
  #   if (is.null(input$sampleid)) {
  #     tpd[which(tpd[, "fileName"] %in% input$filename),]
  #   } else {
  #     tpd[which(tpd[, "Sample"] %in% input$sampleid),]        
  #   }
  # })
  
  
  compoundData <- reactive({
    
    # Combine the selected variables into a new data frame
    
    validate(
      need(!is.null(allData()), "No data available for plotting.")
    )
    cd <- allData() 
    if (is.null(input$sampleid) & is.null(input$filename)) {
      validate("Must choose a sample or file name.")
    }
    if (is.null(input$sampleid)) {
      cd[which(cd[, "fileName"] %in% input$filename),]
    } else {
      cd[which(cd[, "Sample"] %in% input$sampleid),]        
    }
  })
  
  
  outputData <- reactive({
    
    # Combine the selected variables into a new data frame
    
    #od <- wideData()[,c(input$xcol, input$ycol, input$factor, input$globalfilter)]
    od <- od[which(od[,input$factor] %in% input$factorFilter),]
    # %>%
    #   which(wideData(), input$factor %in% input$factorFilter) %>%
    #   wideData()
    
    
  })
  
  
  outData <- reactive({
    switch(input$outputData,
           
           pData = {plotData()},
           sData = {selectedPlotData()}
    )
  })
  
  idLen <- reactive({
    length(input$sampleID)
  })
  
  # Update user selections
  
  observe({
    updateSelectInput(session,
                      inputId = "sampleid",
                      choices = sampleIDList(),
                      selected = sampleIDList()[1]
    )
  })
  
  observe({
    updateSelectInput(session,
                      inputId = "measuretype",
                      choices = measureTypeList(),
                      selected = measureTypeList()[1]
    )
  })  
  
  observe({
    updateSelectInput(session,
                      inputId = "sensorid",
                      choices = sensorIDList(),
                      selected = sensorIDList()[1]
    )
  })  
  
  
  # Update brush
  
  observe({
    # validate(
    #   need(input$xcol, "Select an x-variable"),
    #   need(input$ycol, "Select a y-variable")
    # )
    
    #   brush <- input$plot1_brush
    #   if(!is.null(brush)) {
    #     if (input$xcol == "datetime") {
    #       ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax))
    #     } else {
    #       ranges$x <- c(brush$xmin, brush$xmax)
    #     }
    #     if (length(input$ycol) > 1) {
    #       # Not sure how to handle multiple y-variables
    #     } else {
    #       if (input$ycol == "datetime") {
    #         ranges$y <- as.POSIXct(c(brush$ymin, brush$ymax))
    #       }
    #     }
    #     ranges$y <- c(brush$ymin, brush$ymax)
    #   } else {
    #     ranges$x <- NULL
    #     ranges$y <- NULL
    #   }
    # })
    
    brush <- input$plot1_brush
    if(!is.null(brush)) {
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax))
      
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observe({
    idList <- as.character(unique(rawData()$sampleID))
    # Can use character(0) to remove all choices
    if (is.null(idList))
      idList <- character()
    # Can also set the label and select items
    updateCheckboxGroupInput(session,
                             inputId = "sampleID",
                             label = "Sample data to plot:",
                             choices = idList,
                             selected = idList
    )
    
    
    if (input$close > 0)
      stopApp()                             # stop shiny
  })
  
  GetWD <- reactive({
    base::getWd()
  })
  
  output$viewInfo <- renderPrint({
    
    # Sandbox for ouputting parameter values
    #plotData()[, input$sensorid]
    input$modelType
    # req(pdSelected)
    # summary(pdSelected())
    # str(wideData()[1,input$xcol])
    # str(rawData()$datetime[1])
    # brushedPoints(anotherData(), input$plot1_brush)
    
  })
  
  output$viewRawData <- DT::renderDataTable({
    rawData() %>%
      DT::datatable(options = list(lengthMenu = c(10, 20, 50), pageLength = 50))
  })
  
  # output$viewWideData <- DT::renderDataTable({
  #   allTICData() %>%
  #     DT::datatable(options = list(lengthMenu = c(10, 20, 50), pageLength = 10))
  # })
  
  output$viewPlotData <- DT::renderDataTable({
    plotData() %>%
      DT::datatable(options = list(lengthMenu = c(10, 20, 50), pageLength = 50))
  })
  
  output$viewSelectedPlotData <- DT::renderDataTable({
    selectedPlotData() %>%
      DT::datatable(options = list(lengthMenu = c(10, 20, 50), pageLength = 50))
  })  
  
  output$viewBoxPlotStats <- DT::renderDataTable({
    validate(
      need(!is.null(ranges$x), "Select sensor readings from Plot data.")
    )
    selectedPlotData() %>%
      group_by(sensorID) %>% 
      summarize(count = n(),
                mean = mean(measure),
                std = sd(measure)) %>%
      DT::datatable(options = list(lengthMenu = c(10, 20, 50), pageLength = 50))
  })
  
  
  # output$Standards <- DT::renderDataTable({
  #   ISRTsummary() %>%
  #     DT::datatable(options = list(lengthMenu = c(10, 20, 50), pageLength = 10))
  # }) 
  # 
  # output$isrtsummary <- DT::renderDataTable({
  #   ISRTsummary() %>%
  #     DT::datatable(options = list(lengthMenu = c(10, 20, 50), pageLength = 10))
  # }) 
  # 
  # output$isasummary <- DT::renderDataTable({
  #   ISAsummary() %>%
  #     DT::datatable(options = list(lengthMenu = c(10, 20, 50), pageLength = 10))
  # })   
  # 
  # output$ishsummary <- DT::renderDataTable({
  #   ISHsummary() %>%
  #     DT::datatable(options = list(lengthMenu = c(10, 20, 50), pageLength = 10))
  # }) 
  
  
  
  
  output$plot1 <- renderPlot({
    
    validate(
      need(!is.null(allData()), "No data available for plotting.")
    )
    #   need(input$xcol, "Select an x-variable."),
    #   need(input$ycol, "Select a y-variable"),
    #   need(input$ycol != input$xcol, "x- should not be the same as y-variable"),
    #   need(input$factor, "Select a factor variable.")
    # )
    wd <- as.character(getwd())
    
    # g1 <- ggplot(ticPlotData(),aes(x=RT, y=TIC, color = interaction(fileName, Sample))) +
    #   geom_line() +
    #   ggtitle(paste("Plot of ", input$sampleid, " data for\n", sep = "")) +
    #   theme(plot.title = element_text(hjust=0.5)) +
    #   scale_color_manual(values = rep(pal_d3("category20")(20), times = 4), name = "File and Sample") +      
    #   #      scale_color_d3(name = "File and Sample") +
    #   labs(shape = "Measurements")
    # if (length(grep("date", input$xcol)) > 0) {
    #   g1 <- g1 +
    #     scale_x_datetime(date_labels = "%b-%e %H:%M")
    # }
    # g1 <- g1 + 
    #   xlab(input$xcol) +
    #   ylab(input$ycol)
    
    
    
    
    # summaryData() %>%
    # filter(grepl(input$sensorid, sensorID)) %>%
    # filter(measureType == "WEIGH") %>%
    g1 <- plotData() %>%
      ggplot(aes(x=datetime, y=measure, color = interaction(measureType, sensorID))) +
      geom_line() +
      ggtitle(paste("Measure type and sensor time-series for \n", dataFileName(), sep = "")) +
      theme(plot.title = element_text(hjust=0.5)) +
      scale_color_manual(values = rep(pal_d3("category20")(20), times = 4), name = "Type and Sensor") +       
      #      scale_color_d3(name = "Sensor ID") +
      scale_x_datetime(date_labels = "%b-%e %H:%M") +
      xlab("Date")
    
    g1 <- g1 +
      ylab(paste("Sensor reading [", toString(plotUnitsList()), "]"))
    
    g1
  })
  
  # output$plot2 <- renderPlot({
  #   ggplot(plotData(), aes(get(input$xcol), measure, color = as.factor(key))) +
  #     geom_point()
  #   
  #   #   wd <- as.character(getwd())
  #   #   g <- ggplot(data = plotData(), aes(x = get(input$xcol), y = reading, color = measure, shape = id)) +
  #   #     geom_point(size = 2.5) +
  #   #     scale_shape_manual(values = c(0:18)) +
  #   #     scale_colour_manual(values = pColors()) +
  #   #     coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
  #   #     xlab(paste(input$xcol, " (", mUnits()[input$xcol], ")", sep = "")) +
  #   #     ggtitle(paste("Data for", fileInfo$name))
  #   #   
  #   #   switch(input$modelType,
  #   #          "0" = gfit <- g,
  #   #          "1" = gfit <- g + geom_smooth(method = "lm"),
  #   #          "2" = gfit <- g + geom_smooth(method = "loess")
  #   #   )
  #   #   gfit <- gfit + coord_cartesian(xlim = ranges$x, ylim = ranges$y)
  #   #   gfit
  #   
  # })
  
  
  
  output$plot2 <- renderPlot({
    # validate(
    #   need(!is.null(plotData()), "No data available for plotting."),
    #   need(input$xcol, "Select an x-variable."),
    #   need(input$ycol, "Select a y-variable"),
    #   need(input$ycol != input$xcol, "x- should not be the same as y-variable"),
    #   need(input$factor, "Select a factor variable.")
    # )
    # g2 <- ggplot(ticPlotData(),aes(x=RT, y=TIC, color = interaction(fileName, Sample))) +
    #   geom_line(size = 1.2) +
    #   ggtitle(paste("Plot of ", input$sampleid, " data for\n", sep = "")) +
    #   theme(plot.title = element_text(hjust=0.5)) +
    #   scale_color_manual(values = rep(pal_d3("category20")(20), times = 4), name = "File and Sample") +
    #   #      scale_color_d3(name = "File and Sample") +
    #   labs(shape = "Measurements")
    
    
    g2 <- plotData() %>%
      ggplot(aes(x=datetime, y=measure, color = interaction(measureType, sensorID))) +
      geom_line() +
      ggtitle(paste("Measure type and sensor time-series for \n", dataFileName(), sep = "")) +
      theme(plot.title = element_text(hjust=0.5)) +
      scale_color_manual(values = rep(pal_d3("category20")(20), times = 4), name = "Type and Sensor") +       
      #      scale_color_d3(name = "Sensor ID") +
      scale_x_datetime(date_labels = "%b-%e %H:%M") +
      xlab("Date") +
      ylab("Sensor reading")
    
    g2 <- g2 +
      ylab(paste("Sensor reading [", toString(plotUnitsList()), "]"))        
    
    if (!is.null(ranges$x)) { 
      g2 <- g2 + 
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    }
    
    g2
  })    
  
  
  output$my_tooltip <- renderUI({
    hover <- input$plot2_hover 
    y <- nearPoints(allData(), input$plot2_hover)[ , "sensorID"]
    req(nrow(y) != 0)
    verbatimTextOutput(y)
  })
  
  
  
  output$boxPlotSelected <- renderPlot({
    # validate(
    #   need(!is.null(plotData()), "No data available for plotting."),
    #   need(input$xcol, "Select an x-variable."),
    #   need(input$ycol, "Select a y-variable"),
    #   need(input$ycol != input$xcol, "x- should not be the same as y-variable"),
    #   need(input$factor, "Select a factor variable.")
    # )
    selectedPlotData() %>%
      # dplyr::select(RT, abbrIS) %>%   #select(Height, Area, RT, LibraryID, CAS, Qual, dateTime, dataPath, fileName, abbrIS) %>%
      # dplyr::filter(!is.na(abbrIS)) %>%
      # dplyr::mutate(abbrIS = as.factor(abbrIS)) %>%
      ggplot(aes(x=sensorID, y=measure, color = sensorID)) +
      geom_boxplot() +
      ggtitle(paste("Boxplot of selected sensor data for \n", dataFileName(), sep = "")) +
      theme(plot.title = element_text(hjust=0.5)) +
      scale_color_d3(name = "Sensor ID") +
      xlab("Sensor ID") +
      ylab("Readings")
  })
  
  
  # p <- chDataMass %>%
  #   filter(sensorID %in% c("dryPWM", "wetPWM", "sBME680_h", "mBME680_h")) %>%
  #   filter(measureType == "WEIGH") %>%
  #   select(datetime, measureType, sampleID, sensorID, measure, sampleTime) %>%
  #   #  na.omit() %>%
  #   spread(sensorID, measure) %>%
  #   mutate(dryPWM = as.factor(dryPWM)) %>%
  #   ggplot(aes(x=dryPWM, y=mBME680_h, color = dryPWM)) + 
  #   geom_boxplot() +
  #   ggtitle(paste("Figure ", graphCounter, ".  Mixing volume humidity by dryPWM for \n", dataFileName, sep = "")) +
  #   theme(plot.title = element_text(hjust=0.5)) +
  #   scale_color_d3(name = "Dry pump speed") +
  #   xlab("Dry pump speed") +
  #   ylab("%RH")
  # 
  
  
  # 
  # output$linePlotISRT <- renderPlot({
  #   # validate(
  #   #   need(!is.null(plotData()), "No data available for plotting."),
  #   #   need(input$xcol, "Select an x-variable."),
  #   #   need(input$ycol, "Select a y-variable"),
  #   #   need(input$ycol != input$xcol, "x- should not be the same as y-variable"),
  #   #   need(input$factor, "Select a factor variable.")
  #   # )
  #   summaryData() %>%
  #     dplyr::select(RT, dateTime, abbrIS) %>%   #select(Height, Area, RT, LibraryID, CAS, Qual, dateTime, dataPath, fileName, abbrIS) %>%
  #     dplyr::filter(!is.na(abbrIS)) %>%
  #     dplyr::mutate(abbrIS = as.factor(abbrIS)) %>%
  #     ggplot(aes(x=dateTime, y=RT, color = abbrIS)) +
  #     geom_line() +
  #     ggtitle(paste("Figure ", ".  Internal standard retention times for sequence \n",  sep = "")) +
  #     theme(plot.title = element_text(hjust=0.5)) +
  #     scale_color_d3(name = "Internal standard") +
  #     xlab("date time") +
  #     ylab("RT")
  # }) 
  # 
  # 
  # output$boxPlotISArea <- renderPlot({
  #   
  #   summaryData() %>%
  #     dplyr::select(Area, abbrIS) %>%
  #     dplyr::filter(!is.na(abbrIS)) %>%
  #     dplyr::mutate(abbrIS = as.factor(abbrIS)) %>%
  #     ggplot(aes(x=abbrIS, y=Area, color = abbrIS)) +
  #     geom_boxplot() +
  #     ggtitle(paste("Figure ",  ".  Internal standard area for sequence \n",  sep = "")) +
  #     theme(plot.title = element_text(hjust=0.5)) +
  #     scale_color_d3(name = "Internal standard") +
  #     xlab("Internal standard") +
  #     ylab("Area")
  # })
  # 
  # 
  # output$linePlotISArea <- renderPlot({
  #   
  #   summaryData() %>%
  #     dplyr::select(Area, dateTime, abbrIS) %>%
  #     dplyr::filter(!is.na(abbrIS)) %>%
  #     dplyr::mutate(abbrIS = as.factor(abbrIS)) %>%
  #     ggplot(aes(x=dateTime, y=Area, color = abbrIS)) +
  #     geom_line() +
  #     ggtitle(paste("Figure ",  ".  Internal standard area for sequence \n",  sep = "")) +
  #     theme(plot.title = element_text(hjust=0.5)) +
  #     scale_color_d3(name = "Internal standard") +
  #     xlab("date time") +
  #     ylab("Area")
  # })
  # 
  # output$boxPlotISHeight <- renderPlot({
  #   
  #   summaryData() %>%
  #     dplyr::select(Height, abbrIS) %>%
  #     dplyr::filter(!is.na(abbrIS)) %>%
  #     dplyr::mutate(abbrIS = as.factor(abbrIS)) %>%
  #     ggplot(aes(x=abbrIS, y=Height, color = abbrIS)) +
  #     geom_boxplot() +
  #     ggtitle(paste("Figure ",  ".  Internal standard area for sequence \n",  sep = "")) +
  #     theme(plot.title = element_text(hjust=0.5)) +
  #     scale_color_d3(name = "Internal standard") +
  #     xlab("Internal standard") +
  #     ylab("Height")
  # }) 
  # 
  # output$linePlotISHeight <- renderPlot({
  #   
  #   summaryData() %>%
  #     dplyr::select(Height, dateTime, abbrIS) %>%
  #     dplyr::filter(!is.na(abbrIS)) %>%
  #     dplyr::mutate(abbrIS = as.factor(abbrIS)) %>%
  #     ggplot(aes(x=dateTime, y=Height, color = abbrIS)) +
  #     geom_line() +
  #     ggtitle(paste("Figure ",  ".  Internal standard area for sequence \n",  sep = "")) +
  #     theme(plot.title = element_text(hjust=0.5)) +
  #     scale_color_d3(name = "Internal standard") +
  #     xlab("date time") +
  #     ylab("Height")
  # }) 
  # 
  # 
  # # plot sample signature plots in run order.
  # 
  # 
  # ticSamples <- reactive({
  #   unique(ticPlotData()$fileName)
  # })
  # 
  # targetArea <- 20000000
  # 
  # output$sigPlot <- renderPlot({
  #   
  #   #    ticSamples <- unique(ticPlotData()$fileName)
  #   
  #   sigPlotList <- list()
  #   
  #   for (t in 1:length(ticSamples())) {
  #     #t = 5
  #     
  #     df <- summaryData() %>%
  #       select(RT, LibraryID, CAS, Qual, Sample, fileName, compClass, abbrIS, validSample, AreaNorm) %>%
  #       filter(validSample == TRUE) %>%
  #       #      filter(Qual >= 90) %>%
  #       filter(fileName == ticSamples()[t])
  #     
  #     #filter(fileName == "110223_019.D")
  #     dcb <- which(df$abbrIS == "DCB")
  #     RTdcb <- df$RT[dcb]
  #     pTIC <- ggplot(df, aes(x=RT, y=AreaNorm)) +
  #       geom_segment(aes(x=RT, xend=RT, y=0, yend=AreaNorm), size=1.3)
  # 
  #       if (length(dcb) > 0) {
  #         pTIC <- pTIC +
  #         geom_segment(aes(x=RTdcb, xend=RTdcb, y=0, yend=targetArea),  color = "blue", size=1.3) +
  #           ggtitle(paste("Normalized area for ", df$Sample[1], "\n[", ticSamples()[t], "] t: ", t, " dcb: ", dcb, "\n", sep = "")) +
  #           ylab("Normalized area")
  #       } else {
  #         pTIC <- pTIC +
  #         ggtitle(paste("Area for ", df$Sample[1], "\n[", ticSamples()[t], "] t: ", t, "  no dcb \n", sep = "")) +
  #           ylab("Area")
  #       }
  #     
  #     sigPlotList[[t]] <- pTIC +
  #       theme(plot.title = element_text(hjust=0.5)) +
  #       xlim(0, 35) +
  #       #ylim(0, maxTIC) +
  #       xlab("Retention time (min)")
  #   }
  #   #    list(sampleLookup, sigPlotList)
  #   grid.arrange(grobs = sigPlotList, ncol = 3)
  # })
  # 
  # 
  # output$classPlot <- renderPlot({
  #   QualLow <- 75
  #   QualThreshold <- 90
  #   # summaryData() %>%
  #   #   ggplot(aes(x=compClass, fill = compClass)) +
  #   #   geom_bar() +
  #   #   ggtitle(paste("Figure ",  ".  Internal standard area for sequence \n",  sep = "")) +
  #   #   theme(plot.title = element_text(hjust=0.5)) +
  #   #   scale_color_d3(name = "Compound class") +
  #   #   xlab("Internal standard") +
  #   #   ylab("Height")
  #   
  #   
  #   #    ticSamples <- unique(ticPlotData()$fileName)
  #   classPlotList <- list()
  #   
  #   
  #   # In order to plot all the graphs on the same scale, first calculate the ticMax and classMax of ticSamples()
  #   
  #   maxClassCount <- 0
  #   classMax <- 0
  #   for (t in 1:length(ticSamples())) {
  #     #t = 5
  #     
  #     df <- summaryData() %>%
  #       select(Qual, fileName, compClass, abbrIS) %>%
  #       filter(Qual >= QualLow) %>%
  #       filter(fileName == ticSamples()[t]) %>%
  #       filter(is.na(abbrIS))
  #     
  #     dfTable <- as.data.frame(table(df$compClass, useNA = "always"))      
  #     colnames(dfTable) <- c("class", "count")
  #     classMax <- max(dfTable$count)
  #     if (maxClassCount < classMax) {
  #       maxClassCount <- classMax
  #     }
  #   }
  #   
  #   
  #   for (t in 1:length(ticSamples())) {
  #     #t = 5
  #     
  #     df <- summaryData() %>%
  #       select(Qual, Sample, fileName, compClass, abbrIS) %>%
  #       filter(Qual >= QualLow) %>%
  #       filter(fileName == ticSamples()[t]) %>%
  #       filter(is.na(abbrIS))
  #     
  #     dfTable <- as.data.frame(table(df$compClass, useNA = "always"))
  #     colnames(dfTable) <- c("class", "count")
  #     
  #     if (sum(compoundClasses()[] %in% dfTable$class) < length(compoundClasses())) {
  #       missingClass <- data.frame(class = compoundClasses()[-which(compoundClasses()[] %in% dfTable$class)], count = 0)
  #       dfTable <- rbind(missingClass, dfTable)
  #     }
  #     
  #     dfTable <- dfTable[order(dfTable$class),]
  #     
  #     df90 <- summaryData() %>%
  #       select(Qual, Sample, fileName, compClass, abbrIS) %>%
  #       filter(Qual >= QualThreshold) %>%
  #       filter(fileName == ticSamples()[t]) %>%
  #       filter(is.na(abbrIS))
  #     
  #     dfTable90 <- as.data.frame(table(df90$compClass, useNA = "always"))
  #     colnames(dfTable90) <- c("class", "count")
  #     
  #     if (sum(compoundClasses()[] %in% dfTable90$class) < length(compoundClasses())) {
  #       missingClass <- data.frame(class = compoundClasses()[-which(compoundClasses()[] %in% dfTable90$class)], count = 0)
  #       dfTable90 <- rbind(missingClass, dfTable90)
  #     }
  #     
  #     dfTable90 <- dfTable90[order(dfTable90$class),]
  #     
  #     classPlotList[[t]] <- ggplot(dfTable, aes(x=class, y = count, fill = class)) +
  #       geom_bar(stat = "identity", alpha = 0.3) +
  #       ggtitle(paste("Compound class plot for ", df$Sample[1], "\n[", ticSamples()[t], "]", sep = "")) +
  #       theme(plot.title = element_text(hjust=0.5),
  #             axis.title.x=element_blank(),
  #             axis.text.x = element_text(angle = 90),
  #             #axis.text.x=element_blank(),
  #             axis.ticks.x=element_blank(),
  #             legend.position = "none") +
  #       #scale_fill_d3(palette = "category20") +
  #       geom_bar(data = dfTable90, stat = "identity") +
  #       #scale_fill_d3(palette = "category20") +
  #       #xlab("Compound class") +
  #       ylim(0, maxClassCount) +
  #       ylab("Count")
  #     
  #   }
  #   
  #   grid.arrange(grobs = classPlotList, ncol = 3)
  #   
  # })  
  
  
  
  
  # Downloads are saved in browser designated downloads folder, wherever that may be.
  # The app is set to save a copy of the plot and the datafile that generated the plot.
  
  # Output plot using downloadHandler
  
  plotInput <- function(){
    wd <- as.character(getwd())
    g <- ggplot(data = plotData(), aes(x = get(input$xcol), y = measure,color = as.factor(get(input$factor)), shape = as.factor(key))) +
      geom_point(size = 2.5) +
      scale_shape_manual(values = c(0:18)) +
      scale_colour_manual(values = pColors()) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
      xlab(paste(input$xcol, " (", mUnits()[input$xcol], ")", sep = "")) +
      ggtitle(paste("Data for", fileInfo$name))
    switch(input$modelType,
           "0" = gsave <- g,
           "1" = gsave <- g + geom_smooth(method = "lm"),
           "2" = gsave <- g + geom_smooth(method = "loess")
    )    
    gsave
  }
  
  output$savePlot <- downloadHandler(
    filename = function() {
      switch(input$modelType,
             "0" = model <- "none",
             "1" = model <- "lm",
             "2" = model <- "loess"
      )    
      paste(appName, "_", strsplit(dataFileName(), ".CSV")[[1]][1], "_",
            as.character(input$ycol), "v", as.character(input$xcol), "fit_",
            model, "_",
            format(Sys.time(), "%Y-%m-%d %H:%M"), ".png", sep = "") 
    },
    content = function(file) {
      #      png(file)
      #      print(plotInput())
      ggsave(file, plot = plotInput(), device = "png")
      #      dev.off()
    },
    contentType = "image/png"
  )
  
  # Output .csv using downloadHandler.  This works.  
  
  output$saveData <- downloadHandler(
    filename = function() {
      paste(appAbbr, appVer, "_", strsplit(dataFileName(), ".CSV")[[1]][1], "_",
            gsub(",", "", toString(plotUnitsList())), "_",
            min(ranges$x), " to ", max(ranges$x), "_",
            Sys.Date(), ".csv", sep="")
      # as.character(input$ycol), "v", as.character(input$xcol), "_",
      # format(Sys.time(), "%Y-%m-%d %H:%M"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(outData(), file)
    }
  )
  
  
  # cat("\n > save plot data to file:\n")
  # cat("   > ")
  # #print(plotSensorIDList())  
  # cat("\n") 
  # 
  
} # End of server



shinyApp(ui, server)
