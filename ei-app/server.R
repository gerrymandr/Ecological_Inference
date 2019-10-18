library(shiny)
library(shinydashboard)
library(ggplot2)
library(ei)
library(eiPack)
library(eiCompare)
library(shinycssloaders)
#
library(rgdal)
library(sp)
library(tools)
library(dplyr)

shinyServer(function(input, output, session) {
  
  #
  # Establishes the references on the page
  #
  
  url1 <- a("King's EI page", href='https://gking.harvard.edu/category/research-interests/methods/ecological-inference')
  output$king <- renderUI({
    tagList(tags$p(style='font-size: 11px;', url1))
  })
  
  url2 <- a('Notes from Gingles Expert Witness (.pdf)', href='http://www.socsci.uci.edu/~bgrofman/74%20Grofman%201992.%20Expert%20Witness%20Testimony....pdf')
  output$groffman <- renderUI({
    tagList(tags$p(style='font-size: 11px;', url2))
  })
  
  url3 <- a('Blacksher & Menefee (HeinOnline)', href='http://heinonline.org/HOL/LandingPage?handle=hein.journals/hastlj34&div=9&id=&page=')
  output$blacksher <- renderUI({
    tagList(tags$p(style='font-size: 11px;', url3))
  })
  
  #
  # Uploads the numerical voting data
  #
  # Expectation is that there are at least 3 columns (with a header):
  #   Candidate data (as a percent for calculation)
  #   Racial data (as a percent for calculation)
  #   Total Votes Cast (as a scalar)
  #
  #   The header is used to name the columns for drop-downs
  #
  
  filedata <- reactive({
    req(input$file1) # require that the input is available
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)}
    read.csv(inFile$datapath, stringsAsFactors=F)
  })

  #
  # Uploads the shapefiles (4) and deals with the name & path << The shapefiles are optional
  #   *.dbf
  #   *.shp
  #   *.shx
  #   *.prj
  #
  #   All 4 files are required to produce a map
  #
  
  uploadShapefile <- reactive({
     if (!is.null(input$shapeFile)){
       shapeDF <- input$shapeFile
       prevWD <- getwd()
       uploadDirectory <- dirname(shapeDF$datapath[1])
       setwd(uploadDirectory)
       for (i in 1:nrow(shapeDF)){
         file.rename(shapeDF$datapath[i], shapeDF$name[i])
       }
       shapeName <- shapeDF$name[grep(x=shapeDF$name, pattern="*.shp")]
        shapeNameSimple <- file_path_sans_ext(shapeName)
              shapePath <- paste(uploadDirectory, shapeName, sep="/")
       setwd(prevWD)
       
      shapeFrame <- tryCatch(readOGR(dsn=uploadDirectory, layer=shapeNameSimple), 
                             error=function(errorCondition) {
                               message("There was a problem loading the shapefiles")
                               shapeFrame=c(0)
                              }
                             )
      
      return(shapeFrame)
     } else {
       defaultShapeFrame = c(0)
         return(defaultShapeFrame)
     }
  })
  
  output$shapefileUploaded <- reactive({
    return(length(uploadShapefile()) > 1)
  })
  outputOptions(output, 'shapefileUploaded', suspendWhenHidden=FALSE)
      
#  })

  #
  # Retrieves the columns selected as "Candidate Data", "Racial Demographic Variable" and "Total Votes Cast"
  #
    
  output$dependent <- renderUI({
    analysisData <- filedata()
    if (is.null(analysisData)) return(NULL)
    items=names(analysisData)
    names(items)=items
    selectInput('dependent','Candidate data:',items, selected='')
  })
  
  output$independent <- renderUI({
    analysisData <- filedata()
    if (is.null(analysisData)) return(NULL)
    items=names(analysisData)
    names(items)=items
    selectInput('independent', 'Racial demographic variable:', items, selected='')
  })
  
  output$tot.votes <- renderUI({
    analysisData <- filedata()
    if(is.null(analysisData)) return(NULL)
    items=names(analysisData)
    names(items)=items
    selectInput('tot.votes', 'Total votes cast:',items, selected='')
  })
  
  #
  # Retrieves the setting from the "Homogeneous Precincts Threshold" slider
  #
  
  output$ui.slider <- renderUI({
    if (is.null(input$file1)) return()
    sliderInput('slider', 'Homogeneous precincts threshold', width='100%', min=0, max=25, step=1, ticks=T, post='%', value=5)
  })
  

  #
  # Loads the name of the "precinct" column from the csv file
  #

  output$csvPrecinct <- renderUI({
    analysisData <- filedata()
    if(is.null(analysisData)) return(NULL)
    items=names(analysisData)
    names(items)=items
    selectInput('csvPrecinct', 'Precinct column in the CSV file:',items, selected='')
  })

  #
  # Loads the name of the "precinct" column from the shapefile
  #

  output$shpPrecinct <- renderUI({
    precShapeFrame <- uploadShapefile()
    if(is.null(precShapeFrame)) return(NULL)
    shapeItems=names(precShapeFrame)
    names(shapeItems)=shapeItems
    selectInput('shpPrecinct', 'Precinct column in the Shapefiles:',shapeItems, selected='')
  })

  
  
  
  #
  # Runs the analysis
  #
  
  output$ui.action <- renderUI({
    if (is.null(input$file1)) return()
    actionButton('action', ' Run', icon('refresh', lib='glyphicon'))
    })


  ## run models
  
  model <- eventReactive(input$action, {

    #
    # Loads the elected columns from the file and puts them in the analysisData data frame
    #
        
    analysisData <- filedata()[,c(input$independent, input$dependent, input$tot.votes)]
    names(analysisData) <- c('independent', 'dependent', 'tot.votes')
    
    #
    # This is making a second copy of the data for the plotting secion below.
    #
    
    analysisDataPlot <- analysisData                                 # this is because of the re-ordering below
    
    # homogeneous precincts
    analysisData <- analysisData[order(analysisData$independent),]   # this re-ordering is why we need the second copy above
    homogPrec <- round(input$slider/100*dim(analysisData)[1], digits=0)
    homogPrec.low <- 1:homogPrec
    homogPrec.high <- (dim(analysisData)[1]-homogPrec):dim(analysisData)[1]
    
    analysisData$threshold <- 0
    analysisData$threshold[homogPrec.low] <- 1
    analysisData$threshold[homogPrec.high] <-1
    
    analysisData$homogPrec <- NA
    analysisData$homogPrec[homogPrec.low] <- 1
    analysisData$homogPrec[homogPrec.high] <- 1
    
    analysisData$homogPrec.text <- NA
    analysisData$homogPrec.text[homogPrec.low] <- 'low'
    analysisData$homogPrec.text[homogPrec.high] <- 'high'
    
    homogPrec.low.mean <- mean(analysisData$dependent[analysisData$homogPrec.text=='low'], na.rm=T)
    homogPrec.high.mean <- mean(analysisData$dependent[analysisData$homogPrec.text=='high'], na.rm=T)
    
    
    #
    # Calculate the Goodman linear regressions
    #
    
    # goodman estimates
    goodEstReg <- lm(dependent~independent, data=analysisData)
    
    # 
    # Ecological Inference
    # ====================
    #
    # The basic notation of this problem and the associated solution packages is as follows (from King, pg 31, Table 2.3)
    #
    #                     Voting Decision
    # 
    # Race            |  Vote  |  No Vote  |
    # of        ----------------------------
    # Voting          |        |           |
    # Age       Black | Beta_b |  1-Beta_b |  X
    # Person          |        |           |
    #           ----------------------------
    #                 |        |           |
    #           White | Beta_w |  1-Beta_w |   1-X
    #                 |        |           |
    #           ----------------------------
    # 
    #                     T         1-T
    #
    #         Where 
    #           Beta_b is the fraction of blacks that vote
    #           Beta_W is the fraction of whites that vote
    #           X      is the fraction of voting age people who are black
    #           T      is the fraction of people who vote
    #           N      is the number of voting age people
    #           i      is the subscript that denotes the precinct
    #
    #
    #
    # In this context, 
    #           "dependent" is the candiate data, T
    #           "independent" is the racial demographic, X
    #           "tot.votes" is the total number of votes, N
    #
    #
    
    
    
    #
    # Calculate the Ecological Inference estimates (from eiCompare)
    #
    #    ei_est_gen(cand_vector, race_group, total, rho = 10, data, table_names, sample = 1000, tomog = F, density_plot = F, beta_yes=F,...)
    
    candidateVector <- c("dependent")
    raceGroup <- c("~independent")
    table.names <- c('ei.minority', 'ei.white')

        ei.out <- ei_est_gen(cand_vector = candidateVector, race_group = raceGroup, total = 'tot.votes',
                         data = analysisData[,c(1:3),], table_names = table.names, sample=1000, beta_yes = FALSE) # eiCompare

        #ei.out <- ei(dependent~independent, total=input$tot.votes, data=analysisData) # ei

    # 
    # Build a data frame with the results of the ei.out
    #
    
        edf.t <- data.frame(w=c('White support',
                            homogPrec.low.mean,
                            goodEstReg$coefficients[1],
                            ei.out$ei.white[1]/100,
                            ei.out$ei.white[2]/100),
                        m=c(paste(input$independent, ' support', sep=''), 
                            homogPrec.high.mean, 
                            goodEstReg$coefficients[1]+goodEstReg$coefficients[2], 
                            ei.out$ei.minority[1]/100,
                            ei.out$ei.minority[2]/100))
    row.names(edf.t) <- c(input$dependent, 'Homogeneous precincts', 'Goodman ER', 'Ecol Inf', 'EI.se')
    

    #
    # Set up the plot of the Goodman regression 
    #
    
    # goodman plot
    goodReg.plot <- ggplot(analysisData, aes(x=independent,y=dependent)) + 
      xlab(input$independent) + ylab(input$dependent) +
      geom_smooth(method='lm', se=T, colour='black', fullrange=TRUE) +
      scale_x_continuous(expand=c(0,0), limits=c(0,1)) +
      scale_y_continuous(expand=c(0,0), limits=c(-1.5,1.5)) +
      coord_cartesian(xlim=c(0,1), ylim=c(0,1)) +
      geom_point(size=3, aes(colour=as.factor(analysisData$threshold))) +
      geom_point(pch=1, size=3) +
      geom_point(pch=1, size=5, aes(colour=as.factor(analysisData$homogPrec))) +
      scale_color_manual('Homogeneous precincts', breaks=c(0,1), values=c('Gray', 'Red'), labels=c('No', paste('Most extreme ', input$slider,'%', sep=''))) +
      geom_hline(yintercept=0.5, linetype=2, colour='lightgray') +
      theme_bw() + ggtitle("Goodman's Ecological Regression")
    
    # ei table
    ei.table <- as.data.frame(t(edf.t))
    for(i in 2:5){
      ei.table[,i] <- as.numeric(as.character(ei.table[,i]))
    }
    ei.table.final <- ei.table[,c(1:4)]
    
    # original data with ei estimates
    #analysisData.ei <- analysisData[,c(1:3)]
    #analysisData.ei$EI.est.min <- eiread(ei.out, 'betab')
    #analysisData.ei$EI.est.white <- eiread(ei.out, 'betaw')
    
    # ei dotplot
    
    ei.plot.analysisData <- ei.table[,c(1,4,5)]
    names(ei.plot.analysisData) <- c('race', 'ei.est', 'ei.se')
    
    ei.plot <- ggplot(ei.plot.analysisData, aes(x=ei.est, y=1, col=as.factor(race))) +
      geom_hline(yintercept=1, col='black') +
      geom_point(size=6, shape=3) +
      ylab('') + xlab(paste('Support for candidate ', input$dependent, sep='')) +
      scale_x_continuous(limits=c(-.25,1.25)) +
      scale_y_continuous(limits=c(0,2), breaks=c(0,0.5,1,1.5,2), labels=c('','','','','')) +
      scale_color_manual('Race', values=c('red', 'gray40'), labels=c(input$independent, 'White')) +
      geom_errorbarh(aes(xmin=(ei.est) - 2*(ei.se), xmax=(ei.est) + 2*(ei.se), height=0.1), size=5, alpha=0.5, height=0.1) +
      theme_bw() + ggtitle('Ecological Inference')

    ###
    ### Adding in the precinct shapefile and adding the ei data to it, joining on the "Precinct" column
    ###
    
 
     precShapeFrame <- uploadShapefile() 
     if (length(precShapeFrame) > 1){      

      #
      # Re-calculate the ei Data with ALL of the betas
      #

      ## Sorry, but the original data doesn't include the pure version of the columns 
       
      allFileData <- filedata()

      candidateVector <- c("dependent")
      raceGroup <- c("~independent")
      table.names <- c('ei.minority', 'ei.white')
      
      ei.out.betas <- tryCatch(
                            ei_est_gen(cand_vector = candidateVector,
                                       race_group = raceGroup, 
                                       total = 'tot.votes',
                                       data = analysisData[,c(1:3),], 
                                       table_names = table.names, 
                                       sample=1000, 
                                       beta_yes = TRUE),
                           error=function(errorCondition){
                             message("There was a problem with the EI Estimate for the Plot")
                           }
                          ) # eiCompare
      
      #
      # Join the precinct shape data onto the result of the ei estimate and make a plot
      #

      betaByPrecEst <- ei.out.betas[[2]]
      betaByPrecEst$Precinct <- allFileData$Precinct

      #
      # make sure that the precinct to precinct match takes place
      #

      # Commeted out because of the coercion warning for the "by" 
      #
      # precShapeFrame@data <- tryCatch(left_join(precShapeFrame@data, betaByPrecEst, by = setNames(input$csvPrecinct,input$shpPrecinct)),
      #                                   error=function(errorCondition){
      #                                     message("There was a problem with the names of the Precinct data in the CSV and Shapefiles")
      #                                   },
      #                                   warning=function(warningCondition){
      #                                     message(warningCondition)
      #                                   }
      #                                 )

      precShapeFrame@data <- left_join(precShapeFrame@data, betaByPrecEst, by = setNames(input$csvPrecinct,input$shpPrecinct))
      ei_Beta_Choropleth.plot <- spplot(precShapeFrame, 
                                        z="betab_independent_dependent", 
                                        main=paste("EI Estimate beta b -", input$independent, "&", input$dependent, sep=" "))
                                

      
      
      #
      # Make a plot of the "independent" variable from the original drop-downs
      #

      analysisDataPlot$Precinct <- allFileData$Precinct

      # Commeted out because of the coercion warning for the "by" 
      #
      # precShapeFrame@data <- tryCatch(left_join(precShapeFrame@data, analysisDataPlot, by = setNames(input$csvPrecinct, input$shpPrecinct)),
      #                                   error=function(errorCondition){
      #                                     message("There was a problem with the names of the Precinct data in the CSV and Shapefiles")
      #                                   },
      #                                   warning=function(warningCondition){
      #                                     message(warningCondition)
      #                                   }
      #                                 )
      
      precShapeFrame@data <- left_join(precShapeFrame@data, analysisDataPlot, by = setNames(input$csvPrecinct, input$shpPrecinct))
      racialDemographicVariableChoropleth.plot <- spplot(precShapeFrame, 
                                                         z="independent", 
                                                         main=paste("Racial Demographic Variable:", input$independent, sep=" "))

    }
    else
    {
      #
      # This is the no-op that occurs if no shapefiles are chosen.
      #

      ei_Beta_Choropleth.plot <- plot.new()
      racialDemographicVariableChoropleth.plot <- plot.new()
     }

    # ei_Beta_Choropleth.plot <- plot.new()
    # racialDemographicVariableChoropleth.plot <- plot.new()
    
    list(goodReg.plot = goodReg.plot, 
         ei.table = ei.table.final, 
         ei.plot = ei.plot, 
         ei_Beta_Choropleth.plot = ei_Beta_Choropleth.plot, 
         racialDemographicVariableChoropleth.plot = racialDemographicVariableChoropleth.plot)      
  })    
  
  observeEvent(input$action, {
    output$goodman <- renderPlot({
      
      #withProgress(message='Running EI: Maximizing likelihood...importance sampling.',
      #             detail= 'This process may take several minutes...', value=4, {
      #               for(i in 1:10){
      #                 incProgress(1/10)
      #                 Sys.sleep(20)
      #               }
      #             })
      
      model()$goodReg.plot
    })
  })
  
  observeEvent(input$action, {
    output$est <- renderTable({
      
      model()$ei.table
    }, align='c', digits=3)
  })
  
  observeEvent(input$action, {
    output$ei.bounds <- renderPlot({
      
      model()$ei.plot
      
    }, width=750, height=200)
  })
  
  #
  # Added ObserveEvents for the Map tab.
  #
  
  observeEvent(input$action, {
    output$ei_Beta_Choropleth <- renderPlot({
      model()$ei_Beta_Choropleth.plot
    })
  })
  
  observeEvent(input$action, {
    output$racialDemographicVariableChoropleth <- renderPlot({
      model()$racialDemographicVariableChoropleth.plot
    })
  })
  
  ##
  #
  # Echo of the input CSV data file on the Data tab
  #
  
  output$ei.compare <- renderTable({
    filedata()
  })
})