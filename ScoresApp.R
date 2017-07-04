library(shiny)
library(plotly)
# devtools::install_github("ropensci/plotly")
###### UI ###########################################################
ui <- fluidPage(
        sidebarLayout(
          sidebarPanel(
            selectInput("y_axis", label = "Select Y Axis:",
                        choices = c("Scores above 2.5", 
                        "Average Scores")), br()),
# Show a tabset that includes a plot, summary, and table view
# of the generated distribution
          mainPanel(
            tabsetPanel(type = "tabs", 
              tabPanel("Overall", br(), plotlyOutput("plot")),
              tabPanel("Criteria 1", br(), plotlyOutput("plot1")),
              tabPanel("Criteria 2", br(), plotlyOutput("plot2")),
              tabPanel("Summary", div(tableOutput("table"), 
                                      style = "font-size:75%"))
            )
          )     
        )
      ) 
###### Server ####################################################
server <- function(input, output) {
  #### Reactive Data #############################################
  # create an object for ggplot which returns the column the 
  # user specified
  data1 <- reactive({
    y_axis <- switch(input$y_axis,
                     "Scores above 2.5" = results[,c(11,13,15)],
                      "Average Scores" = results[, c(2,4,6)])
  })
  # create gg titles and axis labels
  titles <- reactive({
    titled <- switch(input$y_axis,
                     "Scores above 2.5" = c("Scores Above 2.5", "Percent Above 2.5"),
                     "Average Scores" = c("Average Scores", "Average Score (0-4)"))
  
  })
  # create percent scale for freqAbove 2.5 plots and y axis limits
  scaleByLim <- reactive({
    scaledBy <- switch(input$y_axis,
                       "Scores above 2.5" = list("%", c(0, 1)),
                       "Average Scores" = list(NULL, c(0, 4)))
  
  })
  # Summary of data per table
  tableData <- reactive({
  tabledResults <- switch(input$y_axis,
                          "Scores above 2.5" = results[, c(1, 10:15, 17)],
                          "Average Scores" = results[, 1:7])
  })
  
  #### Ploting Function and rendering ###############################
  givePlotly <- function(y, criteria){
    x <- as.character(results[, 1])
    data <- data.frame(x, y)
    
    plot_ly(data, x = ~x, y = ~y, type = 'bar',
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1))) %>%
      layout(title = paste(titles()[1], criteria, sep = ":"),
             xaxis = list(title = ""),
             yaxis = list(title = titles()[2], tickformat = scaleByLim()[[1]], 
                          range = scaleByLim()[[2]])) %>% 
      config(displayModeBar = F) 
  }
  output$plot <- renderPlotly({
    givePlotly(data1()[, 3], "Overall")
  })
  output$plot1 <- renderPlotly({
    givePlotly(data1()[, 1], "Per Test 1")
  })
  output$plot2 <- renderPlotly({
    givePlotly(data1()[, 2], "Per Test 2")
  })
  output$table <- renderTable({
    tableData()
  })
}       
###### App ##########################################################         
shinyApp(ui, server, options = list(width = "100%", height = 550))

