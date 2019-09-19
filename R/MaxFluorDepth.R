#'  Create an interactive graph of CTD data recorded by a Seabird
#'  The user can draw a box around a subset of data and obtain maximum
#'  fluorescence depth. This will eventually be modified to allow other variables
#'  and to save the subsetted data to a csv, allowing one to filter bad data.
#'
#'
#' @return
#' A pair of graphs with the depth of maximum fluorescence displayed.
#'
#'
#' @details
#'  The purpose of this function is to allow the user to graphically select a subset of
#'  CTD data to locate the depth of maximum fluorescence. This could be modified
#'  to also eliminate erroneous data and save the filtered data.
#'
#' @import svDialogs shiny ggplot2
#' @export
#'


MaxFluorDepth <- function(){
the.file <- dlg_open(title = "Select one CTD data file", filters = dlg_filters[c("R", "All"), ])$res

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
  })

  output$summary1 <- renderPrint({
    the.file$Depth.Bin[the.file$fluorescence == max(the.file$fluorescence)]
  })

  output$summary2 <- renderPrint({
  values$data[['Depth.Bin']][values$data[['fluorescence']] == max(values$data[['fluorescence']])]
    #summary(values$data)
  })

}


shinyApp(ui, server)
}
