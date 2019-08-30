library(shiny)
library(ggplot2)
library(data.world)
library(dplyr)
library(tidyverse)
library(DT)

#show data from data.world
gcdata_ds <- "https://data.world/llawsonwork/gcdata"
#gcdatafile <- data.world::query(
#qry_sql("SELECT * FROM gcdataclean"),
#dataset =gcdata_ds
#)

#datafile <- gcdatafile
#so that you will not need data.world
datafile <-mtcars


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #Application Layout
  sidebarLayout(
    
    #Inputs
    sidebarPanel(
      
      #select variable for y-axis
      selectInput(inputId = "ya",
                  label = "Y-axis",
                  choices = colnames(datafile),
                  selected = "life_expec"
      ),
      
      #Select Variable for x axis
      selectInput(inputId = "xa",
                  label = "X-axis",
                  choices = colnames(datafile),
                  selected = "life_expec"
      )
    ),
    
    #output
    mainPanel(
      plotOutput(outputId = "guilfordplot", brush = "plot_brush"),
      htmlOutput(outputId = "summary"), # summary of lin regress all points
      dataTableOutput(outputId = "brushedtracts"), # data table to make sure brushed points are updating correctly
      textOutput(outputId = "brushedreg") # NOT Working summary of lin reg brushed points 
    )
  )
)

#define server function

server <- function(input, output){
  
  #this was useful in creating the regression model as X was always column 1 and Y was always column  in this dataframe
  datasubset <- reactive({
    req(input$xa)
    req(input$ya)
    data.frame(X = datafile[input$xa], Y = datafile[input$ya])
  })
  
  #create datasubset of the brushed points
  brushedsubset <- reactive({
    req(input$xa)
    req(input$ya)
    req(input$plot_brush)
    brushedPoints(datafile, brush = input$plot_brush) %>%
      select(input$xa, input$ya)
  })
  
  #Create plot
  output$guilfordplot <- renderPlot({
    ggplot(data = datafile, aes_string(x = input$xa, y = input$ya)) +
      geom_point() + geom_smooth(method = "lm")
  })
  
  #create summary file
  output$summary <- renderUI({
    model = lm(datasubset()[,2] ~ datasubset()[,1], data = datasubset())
    r2 = format(summary(model)$r.squared, digits = 3)
    txt = paste("The equation of the line is :\nY = ",
                round(coefficients(model)[1],0), " + ",
                round(coefficients(model)[2], 5), "X")
    
    # str_3 <- format(coef(m)[1], digits = 3)
    
    str_1 <- txt
    str_2 <- paste("The R^2 value is equal to ", r2)
    HTML(paste(str_1, str_2, sep = '<br/>'))
  })
  
  
  # create data table
  output$brushedtracts <- DT::renderDataTable({
    select(brushedsubset(), input$xa, input$ya)
  })
  
  # create brushed summary stats
  output$brushedreg <- renderText({
    modelbrush = lm(brushedsubset()[,2] ~ brushedsubset()[,1], data = brushedsubset())
    br2 = format(summary(modelbrush)$r.squared, digits = 3)
    btxt = paste("The equation of the line is :\nY = ",
                 round(coefficients(modelbrush)[1],0), " + ",
                 round(coefficients(modelbrush)[2], 5), "X")
    paste(btxt, ' and the rsquared is: ', br2 )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)