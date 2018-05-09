#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
require(ggplot2)
source("data_utils.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "IKARion Analytics and Intervention Dashboard"),
  dashboardSidebar(
    selectInput("courses", "Course", c("Course 1", "Course 2", "Course 3")),  
    dateRangeInput("time_range", 
      label = "Period", 
      start = "2018-01-05",
      end = "2018-01-07",
      min = "2018-01-05",
      max = "2018-01-08",
      format = "dd.mm.yyyy"),
    sidebarMenu(
      menuItem("User Models", tabName = "user_model", icon = icon("dashboard")),
      menuItem("Group Models", tabName = "group_model", icon = icon("dashboard"))  
    )
    
  ),
  
  dashboardBody(
    tabItems(
    tabItem("user_model", 
      textInput("usermodeltext", "User model")
    ),
    tabItem("group_model",
      fluidRow(
        downloadButton("download", "Download Report"),
        box(
          title = "Send group model to XPS",
          selectInput("send_interval", "Interval", c("once", "hourly", "dayly", "weekly")),
          actionButton("send_to_xps", "Send")
        )
      ),
      # Boxes need to be put in a row (or column)
      fluidRow(
        box(
          title = "Group Latency",
          plotOutput("latencyPlot"),
          sliderInput("latency_reference", "Reference point", min = 1, max=75, value=10)
        ),
        box(
          title="Groups above reference",
          DT::dataTableOutput("latencyGroups")
        )
      )
    )
  ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  gLatencies <- getGroupLatencyTest()  
   output$latencyPlot <- renderPlot({
     gLatencies %>%
       ggplot(aes(x=latency)) + stat_ecdf() + 
       geom_vline(xintercept = input$latency_reference) +
       xlab("latency") +
       ylab("% groups") +
       theme_bw()
   })
   
   output$latencyGroups <- DT::renderDataTable(
     gLatencies %>%
       filter(latency > input$latency_reference) %>%
       DT::datatable()
   )
   
   output$report <- downloadHandler(
     filename = "group_model_report.html",
     content = function(file) {
       # Copy the report file to a temporary directory before processing it, in
       # case we don't have write permissions to the current working dir (which
       # can happen when deployed).
       tempReport <- file.path(tempdir(), "group_model_report.Rmd")
       file.copy("group_model_report.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       params <- list(latencyReference = input$latency_reference)
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

