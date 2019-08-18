#'  Create an interactive graph of depth and temperature data recorded by a Marport
#'  Trawl Explorer or TE/Catch sensor deployed on a trawl. The user can draw a box around a subset
#'  of data and obtain the mean depth, start time, end time, and elapsed time of that
#'  subset.
#'
#'
#' @return
#' An interactive graph and metadata corresponding to the data selected.
#'
#'
#' @details
#'  The purpose of this function is to allow the user to graphically select a subset of
#'  the depth and temperature data recorded by an instrument deployed on the headline of a trawl.
#'
#' @import svDialogs shiny
#' @export
#'

#  This process uses shiny.  It may not need to use shiny.  But it does.
FishDepthTemp <- function() {
  select_trawl_data()
  mydat2<-catch.and.spread
  #select just the time range you want using the datetime variable you typed in
  catch.and.spread <- subset(catch.and.spread, date.time > datetime &date.time < datetime2 & Type.of.Data %in% c(type1, type2))
  catch.and.spread$Serial <- strsplit(serials, split = "ser")
  kpcols <- c('Type.of.Data', 'Value', 'date.time')
  catch.and.spread <- catch.and.spread[c(kpcols)]

  depths <- subset(mydat2, Sensor.Location==11 & Type.of.Data %in% c("Depth", "Temperature"))
  depths$time2 <-as.POSIXct(depths$date.time)
  depths$value<-as.numeric(depths$Value)

  # create the shiny server.  This is the code that makes the plot and lets you select the data.
  # this is what you run using the next section

  server <- function(input,output, session) {
    #library(svDialogs)
    library(ggplot2)


    output$plot <- renderPlot({
      ggplot(depths, aes(time2, value, color=Type.of.Data)) + geom_point()+
        labs(y='Value', size=22)  +
        theme(text = element_text(size=16))
    })

    dat <- reactive({
      user_brush <- input$user_brush
      brushedPoints(depths, user_brush, xvar = "time2", yvar = "value")
    })
    output$table <- renderTable({
      dat()

    })
    output$dep <- renderText({

      means <- aggregate(value ~ Type.of.Data, FUN=mean, data=dat())
      paste0("Mean depth (m) = ", round(means$value[1],1))

    })

    output$wattemp <- renderText({

      means <- aggregate(value ~ Type.of.Data, FUN=mean, data=dat())
      paste0("Mean temperature = ", round(means$value[2],2))

    })

    output$starttime <- renderText({

      start.time <- min(dat()$time2)
      stime<-as.POSIXct(start.time, origin='1970-01-01')
      paste0("Start time = ", stime)

    })


  }

  #############################
  # This is the ui section that lets you interact with and run the code in the server section.
  ui <- fluidPage(

    h3("Click and hold/drag to select the data you want to use for calculation of mean fishing depth and temperature"),
    plotOutput("plot", brush = "user_brush"),
    tableOutput("dat"),
    textOutput("dep"),
    textOutput("wattemp"),
    textOutput("starttime"),
    tags$style("#wattemp {font-size:20px;
             color:black;
             display:block; }"),
    tags$style("#dep {font-size:20px;
             color:black;
             display:block; }"),
    tags$style("#starttime {font-size:20px;
             color:black;
             display:block; }")
  )

  #this runs it all.
  shinyApp(ui = ui, server = server)


  # Part 3.  If you are happy with the data you saw in the plot, save a csv
  ####################################################################################
  #########################################################################################################################
  #############################################

  #save a csv of the data with time added.  Not sure what we should include in this file.
  write.csv(catch.and.spread, paste0('./processed_data_for_rvcat/',
                                     year,"_", lake, "_", vessel, "_", transect, '_',
                                     serials, '.csv'), row.names=FALSE)
}
