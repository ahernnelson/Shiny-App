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
                     "Scores above 2.5" = results[,c("perc2.5Test1",
                                                     "perc2.5Test2",
                                                     "highScoresAllPerc")],
                      "Average Scores" = results[, c("c1mean","c2mean","allmean")])
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
  frontFacingColumns <- c("course", "freqAbove2.5", "percAbove2.5",
                          "freq2.5Test1", "perc2.5Test1", "freq2.5Test2",
                          "perc2.5Test2",
                          "highScoresAllFreq","highScoresAllPerc")
  averageColumns <- c("course", "c1mean", "c1median", "c2mean", "c2median", "allmean", "allmedian")
  
  tableData <- reactive({
  tabledResults <- switch(input$y_axis,
                          "Scores above 2.5" = results[, frontFacingColumns],
                          "Average Scores" = results[, averageColumns])
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
  givePlotly <- function(data, criteria){
    p <- ggplot(data, aes(x=parentcode,y=percs)) + 
          geom_bar(stat="identity", aes(fill=course),
               color="black") + labs(y = "Percent (# Occurrences / # Students)", x = "Error Codes") +
               scale_y_continuous(labels = scaleByLim()[[1]], limits = labels = scaleByLim()[[2]] + 
                                  ggtitle(label = paste(titles(), criteria, sep = ": "))
    ggplotly(p, height = 450, width = 580)
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

