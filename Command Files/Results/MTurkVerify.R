#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

originalDataDir <- "../../Original Data"

analysisDataDir <- "../../Analysis Data"

generatedDataDir <- file.path(originalDataDir, "Generated data")

graded_num_ans <- read_csv(file.path(analysisDataDir, "GradedNumAnswers.csv"))

graded_nodes <- read_csv(file.path(analysisDataDir, "GradedNodes.csv"))

stats_datasets_tall <- read_csv(file.path(analysisDataDir, "Stats_Datasets_Tall.csv"))

stats_demo <- read_csv(file.path(analysisDataDir, "Stats_Demo.csv"))

responses <- read_csv(file.path(analysisDataDir, "CombinedResponsesWithOrder.csv"))

node_lookup <- read_csv(file.path(generatedDataDir, "node_lookup.csv"), col_types = cols(MaxValue = col_double(),NodeValue = col_double()))

if(interactive()) {

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(12,titlePanel("Double-check MTurk data"))
  ),
  fluidRow(
    column(4,selectInput(
      'filename',
      'Sample Population',
      unique(stats_demo$filename)
    )),
    column(8,selectInput(
      'ids',
      'IDs',
      stats_demo$`Demo-ResponseID`))
  ),
  fluidRow(
    column(12,tabsetPanel(
      tabPanel("Numerical Answers",DT::dataTableOutput("mytable1")),
      tabPanel("Demographics",DT::dataTableOutput("mytable2")),
      tabPanel("Dataset Info",DT::dataTableOutput("mytable3")),
      tabPanel("Click Answers - Table",DT::dataTableOutput("mytable4")),
      tabPanel("Click Answers - Plot",plotOutput("BCgraph",height=200),plotOutput("HDgraph",height=200))
      )
           )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    
    group <- input$filename
    
    if (!is.null(group)) {
      
      newDataset <- stats_demo %>% filter(filename == group) %>% arrange(`Stats-Q_TotalDuration`)
      
      updateSelectInput(session, "ids",
                        label = "Select ID",
                        choices = newDataset %>% select(`Demo-ResponseID`)
      )
    }  
    
  })
      output$mytable1 <- DT::renderDataTable({
        DT::datatable(graded_num_ans %>% filter(`Demo-ResponseID` == input$ids) %>% arrange(DatasetOrder,TaskOrder))
      })
      
      output$mytable2 <- DT::renderDataTable({
        DT::datatable(stats_demo %>% filter(`Demo-ResponseID` == input$ids))
      })

      output$mytable3 <- DT::renderDataTable({
        DT::datatable(stats_datasets_tall %>% filter(`Demo-ResponseID` == input$ids))
      })
      
      output$mytable4 <- DT::renderDataTable({
        DT::datatable(graded_nodes %>% filter(`Demo-ResponseID` == input$ids))
      })
      
      correctCondition <- reactive({stats_datasets_tall %>% filter(`Demo-ResponseID` == input$ids, !is.na(Condition)) %>% select(Condition) %>% unique() %>% unlist()})
      
      output$condition <- renderText({correctCondition})
      
      output$BCgraph <- renderPlot({
        ggplot() + 
          geom_point(data = node_lookup %>% filter(Condition == correctCondition()),
                     aes(x=NodeXAdjusted,y=NodeYAdjusted,color=NodeRank<6)) +
          geom_point(data = graded_nodes %>% filter(Task=="BC", 
                                                    `Demo-ResponseID` == input$ids), 
                     aes(x=Click_X,y=Click_Y), colour="red") + 
          facet_grid(.~Dataset) +
          scale_color_manual(values=c("gray50","blue")) +
          theme_bw()
      })
      
      output$HDgraph <- renderPlot({
        ggplot() + 
          geom_point(data = node_lookup %>% filter(Condition == correctCondition()),
                     aes(x=NodeXAdjusted,y=NodeYAdjusted,color=NodeRank==min(NodeRank))) +
          geom_point(data = graded_nodes %>% filter(Task=="ClickHighDeg", 
                                                    `Demo-ResponseID` == input$ids), 
                     aes(x=Click_X,y=Click_Y), colour="red") + 
          facet_grid(.~Dataset) +
          scale_color_manual(values=c("gray50","blue")) +
          theme_bw()
      })
      
    
  
   
  #newDataset <- reactive({stats_demo %>% filter(filename == input$filename)})
  
  
#   output$distPlot <- renderPlot({
#      # generate bins based on input$bins from ui.R
#      x    <- faithful[, 2] 
#      bins <- seq(min(x), max(x), length.out = input$bins + 1)
#      
#      # draw the histogram with the specified number of bins
#      hist(x, breaks = bins, col = 'darkgray', border = 'white')
#   })
}

}

# Run the application 
shinyApp(ui = ui, server = server)

