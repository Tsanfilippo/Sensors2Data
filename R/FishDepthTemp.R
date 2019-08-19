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
  options(scipen = 999)

  # Prompt to select a data file that has been exported/extracted from Marport SDS.
  the.file <- dlg_open(title = "Select one Marport data file in data folder", filters = dlg_filters[c("R", "All"), ])$res

  mydat <<-read.csv(the.file, dec = ".", numerals =  "no.loss",
                    col.names = c("Time", "Receiver", "Sensor.Location",
                                  "Type.of.Data", "Value", "Type", "SNR", "Noise.floor"),
                    na.strings = "NA",
                    skip = 1, fill=TRUE)


  #Take a look at the data to make sure it was read ok
  head(mydat)

  #subset the data to get just the data for sensors we want
  #  11 is a TE/CATCH on the headrope.  23 is a spread slave.  26 is a spread
  # master.  These numbers correspond to the sensor nodes in Scala.
  # This MAY need to be changed depending on which sensors you have on the net.  It is YOUR job
  # to figure out which ones you are using.  Not anyone elese.
  catch.and.spread <<- subset(mydat, Sensor.Location %in% c("11","12","18","20",  "23", "26"))

  # The time vairable is a string.  Need to convert to a number
  catch.and.spread$Time <- as.numeric(catch.and.spread$Time)

  # the Time variable is milliseconds and below I convert it to seconds
  # because the time has an origin of 01/01/1970 and will need to be added
  # to that value in seconds because that is how POSIX items work.
  catch.and.spread$date.time <- as.POSIXct((catch.and.spread$Time/1000), origin = "1970-01-01", tz="America/New_York")


  # Now you need to enter a date-time that will mark the beginning of the data you want to keep.
  datetime <<- as.POSIXct(dlg_input(GUI =datetime,"Enter the date-time (e.g. 2018-08-12 22:45:00) of the start of data you wish to keep")$res )
  datetime2 <<- as.POSIXct(dlg_input(GUI =datetime,"Enter the date-time (e.g. 2018-08-12 22:45:00) of the end of data you wish to keep")$res )
  #year <- substr(datetime, 1,4)
  #lake <- dlg_input(GUI =vessel,"Enter lake # (e.g. L2 for LM, 3 for LH)")$res
  #vessel <- dlg_input(GUI =vessel,"Enter vessel (e.g. v88 for Sturgeon, v17 For Steelhead)")$res
  #transect <- dlg_input(GUI=transect, "Enter transect (e.g. mw2, no3")$res
  #serials <- dlg_input(GUI = serials, "Enter serials (e.g. ser123, for multiple enter ser123ser456)")$res
  sensor.location <<- as.numeric(dlg_input(GUI = Sensor.Location, "Enter sensor location as a number")$res)
  type1 <<- dlg_input(GUI = Type.of.Data, "Enter 1st data type name (e.g. Depth, Temperature)")$res
  type2 <<- dlg_input(GUI = Type.of.Data, "Enter 2nd data type name (e.g. Depth, Temperature)")$res

  #select just the time range you want using the datetime variable you typed in
  catch.and.spread <- subset(catch.and.spread, date.time > datetime &date.time < datetime2 & Type.of.Data %in% c(type1, type2))
  #catch.and.spread$Serial <- strsplit(serials, split = "ser")
  kpcols <- c('Type.of.Data', 'Value', 'date.time')
  depths<- catch.and.spread[c(kpcols)]
  depths <- subset(depths, Sensor.Location == sensor.location  & Type.of.Data %in% c("Depth", "Temperature"))
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

}
