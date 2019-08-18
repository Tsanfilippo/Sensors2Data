#' @title Estimate time on bottom for trawl
#'
#' @param fdep
#'  A numeric scalar giving the approximate headrope depth for the trawl tow.
#'
#' @return
#' An interactive graph.
#'
#'
#' @details
#'  Create an interactive graph of depth data recorded by an instrument
#'  deployed on the headrope of a trawl. The user can draw a box around a subset
#'  of data and obtain the mean depth, start time, end time, and elapsed time of that
#'  subset.
#'
#'
#' @import svDialogs shiny
#' @export
#'
TimeOnBottom <- function(fdep) {
the.file <- dlg_open(title = "Select csv data file with cable length and fishing depth data.",
                   filters = dlg_filters[c("R", "All"), ])$res

mydat<-read.csv(the.file, header = TRUE)
fdep <- fdep
mydat$time2 <-as.POSIXct(mydat$Time)
mydat$d.diff <- mydat$Depth - as.numeric(fdep)
kpcols <- c("time2", "Temperature", "Depth", "d.diff")
mydat <- mydat[c(kpcols)]
mydat <- subset(mydat, d.diff > -0.75 )

# create the shiny server.  This is the code that makes the plot and lets you select the data.
# this is what you run using the next section

server <- function(input,output, session) {
  #library(svDialogs)
  library(ggplot2)

  output$plot <- renderPlot({
    ggplot(mydat, aes(time2, Depth)) +
      geom_point()+
      labs(y='Depth', size=22, x = "Time")  +
      theme(text = element_text(size=16))
  })

  dat <- reactive({
    user_brush <- input$user_brush
    brushedPoints(mydat, user_brush, xvar = "time2", yvar = "Depth")
  })
  output$table <- renderTable({
    dat()

  })
  output$dep <- renderText({
    dat1 <- dat()
    mean.d <- mean(dat1$Depth)
    paste0("Mean depth (m) = ", round(mean.d, 2))

  })

  output$starttime <- renderText({
    start.time <- min(dat()$time2)
    stime<-as.POSIXct(start.time, origin='1970-01-01')
    paste0("Start time = ", stime)

  })

  output$endtime <- renderText({
    end.time <- max(dat()$time2)
    etime<-as.POSIXct(end.time, origin='1970-01-01')
    paste0("End time = ", etime)

  })

  output$elapstime <- renderText({
    start.time <- min(dat()$time2)
    stime<-as.POSIXct(start.time, origin='1970-01-01')
    end.time <- max(dat()$time2)
    etime<-as.POSIXct(end.time, origin='1970-01-01')
    eltime <- etime - stime
    paste0("Elapsed time (minutes) = ", round(eltime,2))

  })


}

#############################
# This is the ui section that lets you interact with and run the code in the server section.
ui <- fluidPage(

  h3("Click and hold/drag to select the data you want to use for calculation of mean fishing depth and temperature"),
  plotOutput("plot", brush = "user_brush"),
  tableOutput("dat"),
  textOutput("dep"),
  textOutput("starttime"),
  textOutput("endtime"),
  textOutput("elapstime"),
  tags$style("#dep {font-size:20px;
             color:black;
             display:block; }"),
  tags$style("#starttime {font-size:20px;
             color:black;
             display:block; }"),
  tags$style("#endtime {font-size:20px;
             color:black;
             display:block; }"),
  tags$style("#elapstime {font-size:20px;
             color:black;
             display:block; }")
)
#this runs it all.
shinyApp(ui = ui, server = server)
}



