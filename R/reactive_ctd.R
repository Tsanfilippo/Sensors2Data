library(ggplot2)
library(dplyr)
library(oce)
library(rLakeAnalyzer)
library(shiny)

the.file <- read.csv("C:\\Users\\dmwarner\\Documents\\Acoustics\\Michigan\\acoustic\\2019\\CTD\\CSV OUTPUT\\2019v88ser214_no4.csv")

the.file2 <- the.file


ui <- fluidPage(
  fluidRow(
    column(width = 12, class = "well",
           h4("Left plot controls right plot"),
           fluidRow(
             column(width = 6,
                    h5("Entire Dataset (left)"),
                    plotOutput("plot1", height = 350,
                               brush = brushOpts(
                                 id = "plot1_brush",
                                 resetOnNew = TRUE
                               )
                    )
             ),
             column(width = 6,
                    h5("Zoomed Region (right)"),
                    plotOutput("plot2", height = 350)
             )
           ),
           fluidRow(
             column(width = 6,
                    verbatimTextOutput("summary1")),
             column(width = 6,
                    verbatimTextOutput("summary2"))
           )
    )
  )
)
server <- function(input, output) {

  values <- reactiveValues(data=the.file)
  output$plot1 <- renderPlot({
    ggplot() +
      geom_point(data=the.file, aes(x=fluorescence, y=Depth.Bin), color="darkgreen") +
      geom_path(data=the.file, aes(x=fluorescence, y=Depth.Bin), color="darkgreen") +
      geom_point(data=the.file, aes(x=temperature, y=Depth.Bin))+
      geom_path(data=the.file, aes(x=temperature, y=Depth.Bin))+
      scale_y_reverse()
  })

  output$plot2 <- renderPlot({
    ggplot() +
      geom_point(data = values$data, aes(fluorescence, Depth.Bin), color="darkgreen") +
      geom_path(data = values$data, aes(fluorescence, Depth.Bin), color="darkgreen") +
      geom_point(data = values$data, aes(temperature, Depth.Bin))+
      geom_path(data = values$data, aes(temperature, Depth.Bin))+
      scale_y_reverse()

  })

  observe({
    if (!is.null(input$plot1_brush)) {
      values$data <- brushedPoints(the.file, input$plot1_brush)
      my.df <<- as.data.frame(values$data)
    } else {
      values$data <- the.file

    }
    my.df <- reactiveValues(data.frame(values$data))
  })

  output$summary1 <- renderPrint({
    the.file$Depth.Bin[the.file$fluorescence == max(the.file$fluorescence)]
  })

  output$summary2 <- renderPrint({
  max.flr.depth <- my.df$Depth.Bin[my.df$fluorescence == max(my.df$fluorescence)]
    #summary(values$data)
  })

}


shinyApp(ui, server)
