#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("setup.R")
source("data_utils.R")

generateCourseList <- function() {

  print("call")
  courseList <- list()
  cdf <- getCourses()
  courseList[cdf$name] <- cdf$id
  courseList
}


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "IKARion Analytics and Intervention Dashboard"),
  dashboardSidebar(
    selectInput("courses", "Course", generateCourseList()),  
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
      textInput("usermodeltext", "User model"),
      fluidRow(
        downloadButton("download_UM", "Download Report"),
        box(
          title = "Send users models to XPS",
          selectInput("send_interval_UM", "Interval", c("once", "hourly", "dayly", "weekly")),
          actionButton("send_to_xps_UM", "Send")
        )
      ),
      fluidRow(
        valueBoxOutput("activeDaysBox"),
        box(
          title = "Active students (days)",
          plotOutput("activeDaysPlot") %>% withSpinner(color="grey")
        )
      ),
      fluidRow(
        box(
          title = "Active days (cumulative)",
          plotOutput("activeDaysCumPlot") %>% withSpinner(color="grey"),
          sliderInput("active_days_reference", "Reference point", min = 1, max=30, value=2)
        ),
        box(
          title="Users below reference",
          DT::dataTableOutput("activeDaysUsers")
        )
      )
    ),
    
    
    tabItem("group_model",
      fluidRow(
        downloadButton("download_GM", "Download Report"),
        box(
          title = "Send group model to XPS",
          selectInput("send_interval_GM", "Interval", c("once", "hourly", "dayly", "weekly")),
          actionButton("send_to_xps_GM", "Send")
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
 
  #################
  ## Group model ##
  #################
  
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
   
   ################
   ## User model ##
   ################
   
   activeDays <- reactive({
     getActiveDaysAll(input$courses)
   })
   
   output$activeDaysBox <- renderValueBox({
     
     valueBox(
       activeDays()$user %>% unique %>% length,
       subtitle = "Number of active students",
       icon = icon("user"),
       color = "purple"
     )
   })
   output$activeDaysPlot <- renderPlot({
     
     activeDays() %>%
       count(activeDay) %>%
       ggplot(aes(x=activeDay, y=n)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle=90))
   })
   
   output$activeDaysCumPlot <- renderPlot({
     
     activeDays() %>%
       count(user) %>%
       ggplot(aes(x=n)) + stat_ecdf() + 
       geom_vline(xintercept = input$active_days_reference) +
       xlab("active days") +
       ylab("% users") +
       theme_bw()
   })
   
   output$activeDaysUsers <- DT::renderDataTable(
     activeDays() %>%
       count(user, sort=TRUE) %>%
       filter(n < input$active_days_reference) %>%
       rename("Active Days" = n) %>%
       DT::datatable()
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

