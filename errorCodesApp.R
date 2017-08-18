library(reshape2)
library(tidyr)
library(dplyr)


library(shiny)
library(plotly)
# devtools::install_github("ropensci/plotly")
###### UI ###########################################################
ui <- fluidPage(
  # Application title
  titlePanel("Tabsets"),
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
   sidebarPanel(
     fileInput("new_file", "Input CSV File"),
     actionButton("Clean","Me First"),
     actionButton("Analyze","Me Second"),
     radioButtons("course", "Choose course:",
                  c("Calculus 1" = "calc1",
                    "Calculus 2" = "calc2",
                    "Calculus 3" = "calc3",
                    "Numerical Analysis" = "numAnlys",
                    "History of Math" = "histMath",
                    "Real Analyisis" = "realAnlys",
                    "Probability Theory" = "probThry")),
     
     br()),
   # Show a tabset that includes a plot, summary, and table view
   # of the generated distribution
   mainPanel(
     tabsetPanel(type = "tabs", 
                 tabPanel("Overall", plotlyOutput("plot1")),
                 tabPanel("Criteria 1", plotlyOutput("plot2")),
                 tabPanel("Criteria 2", plotlyOutput("plot3")))
  )
 )
)

###### Server #######################################################
server <-  function(input, output) {
  
  saveData <- function(inptable) ({
    new_data <- inptable
    new_data2 <- read.csv(new_data$datapath)
    write.csv(new_data2, "~/new_file.csv")
  })
  
  observeEvent(input$Clean, {
    saveData(input$new_file)} )
  
  observeEvent(input$Analyze, {
    source("setup.r")
  })
  
  #### Reactive Data ################################################
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  
  
  
  data <- reactive({
   course <- switch(input$course,
                    calc1 = courseresults$Calc1,
                    calc2 = courseresults$Calc2,
                    calc3 = courseresults$Calc3,
                    numAnlys = courseresults$NumAnalysis,
                    histMath = courseresults$HistoryMath,
                    realAnlys = courseresults$RealAnalysis,
                    probThry = courseresults$ProbTheory)
  })
  # Reactive varibable for graph aesthetics. Right now only width(Subject to change)
  visuals <- reactive({
   visualized <- switch(input$course,
                        calc1 = list(.9),
                        calc2 = list(.9),
                        calc3 = list(.9),
                        numAnlys = list(.1),
                        histMath = list(.9),
                        realAnlys = list(.9),
                        probThry = list(.9))
  })
  # reactive title
  titles <- reactive({
    titled <- switch(input$course,
                     calc1 ="Calc 1 Errors",
                     calc2 = "Calc 2 Errors",
                     calc3 = "Calc 3 Errors",
                     numAnlys = "Numerical Analysis Errors",
                     histMath = "History of Math Errors",
                     realAnlys = "Real Analysis Errors",
                     probThry = "Prob Theory Errors")
  })
  #### Plots and Rendering ##########################################
  # Generate a plot of the data using givePlotly function. 
  # Also uses the inputs to build the plot label.
  # Note that the dependencies on both the inputs and
  # the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  givePlotly <- function(data, criteria){
    p <- ggplot(data, aes(x=parentcode,y=percs)) + 
          geom_bar(stat="identity",width = visuals()[[1]], position="dodge", aes(fill=codes),
               color="black") + labs(y = "Percent (# Occurrences / # Testsed)", x = "Error Codes") +
               scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + ggtitle(label = paste(titles(), criteria, sep = ": "))
    ggplotly(p, height = 450, width = 580)
  }
  output$plot1 <- renderPlotly({
    givePlotly(data()[[1]], "Overall")
  })
  output$plot2 <- renderPlotly({
    givePlotly(data()[[2]], "Per Criteria 1")

  })
  output$plot3 <- renderPlotly({
    givePlotly(data()[[3]], "Per Criteria 2")
  })
} 
##### App ######
shinyApp(ui, server, options = list(width = "100%", height = 550))


