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
source("xps_utils.R")

generateCourseList <- function() {
  
  courseList <- list()
  cdf <- getCourses()
  courseList[cdf$name] <- cdf$courseid
  courseList
}


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "IKARion Analytics"),
  dashboardSidebar(
    selectInput("courses", "Course", generateCourseList()),  
    dateRangeInput("time_range", 
                   label = "Period", 
                   start = "2018-05-01",
                   end = "2018-05-30",
                   min = "2018-04-01",
                   max = "2018-12-30",
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
                downloadButton("downloadUM", "Download User Model"),
                box(
                  title = "Send users models to XPS",
                  selectInput("send_interval_UM", "Interval", c("once", "hourly", "dayly", "weekly")),
                  actionButton("UM_to_XPS", "Send")
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
                downloadButton("downloadGM", "Download Group Model"),
                box(
                  title = "Send group model to XPS",
                  selectInput("send_interval_GM", "Interval", c("once", "hourly", "dayly", "weekly")),
                  actionButton("GM_to_XPS", "Send")
                )
              ),
              # Boxes need to be put in a row (or column)
              fluidRow(
                
                # latency forum post
                box(
                  title = "Group Latency",
                  plotOutput("latencyPlot") %>% withSpinner(color="grey"),
                  sliderInput("latency_reference", "Reference point", min = 1, max=250, value=10)
                )
                ,
                box(
                  title="Groups above answer latency (reference)",
                  DT::dataTableOutput("latencyGroups")
                ))
                ,
                #br(),
                
              # fluidRow(
              #   # latency commit message
              #   box(
              #     title = "Group Commit Latency",
              #     plotOutput("CommitLatencyPlot") %>% withSpinner(color="grey"),
              #     sliderInput("commit_latency_reference", "Reference point", min = 1, max=250, value=10)
              #   ),
              #   box(
              #     title="Groups above commit latency (reference)",
              #     DT::dataTableOutput("CommitLatencyGroups")
              #   )
              # ),
              fluidRow(
                box(
                  title = "TestTitle",
                  DT::dataTableOutput("CommitLatencyGroups")
                ),
                box(
                  title = "Group Repos",
                  DT::dataTableOutput("GroupRepos")
                )
              )
            
      )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #################
  ## Group model ##
  #################
  groupSequences <- reactive({
    print("***22")
    df <- getGroupSequencesAll(input$courses)
    browser()
    print("***22_2")
    getGroupSequencesAll(input$courses)
  })
  
  groupGitSequences <- reactive({
    print("***23")
    df <- getGitSequencesAll()
    getGitSequencesAll()
    
    
    
  })
  
  groupLatencies <- reactive({
    trange <- input$time_range 
    start <- trange[1]# %>% as.POSIXlt %>% as.integer
    end <- trange[2] #%>% as.POSIXlt %>% as.integer
    
    #print("***14")
    #print(start)
    #print(end)
    #print("***21")
    #print(groupSequences())
    
    getGroupLatencies2(groupSequences(), start, end)
  })
  
  # commitLatencies <- reactive({
  #   getCommitLatencies(groupGitSequences)
  #   
  # })
  
  
  # get latencies for commits
  groupCommitLatencies <- reactive({
    print("***20")
    #trange <- input$time_range 
    #start <- trange[1]# %>% as.POSIXlt %>% as.integer
    #end <- trange[2] #%>% as.POSIXlt %>% as.integer
    start <- "2018-05-01"
    end <- "2018-05-30"
    #print(start)
    #print(end)
    print("***26")
    all_repos <- getGroupRepositories()
    #print(getGroupRepositories())
    print(all_repos)
    print("***27")
    #browser()
    print(all_repos[1,]$repo)
    print("***28")
    
    print(getGroupSequenceGit(all_repos[1,]$repo))
    #print("***25")
    
    print("***29")
    print(all_repos[1,]$repo)
    print(start)
    print(end)
    
    getCommitLatencies(all_repos[1,]$repo, start, end)
  })
  
  createGroupModel <- reactive({
    list(
      model_metadata=list(
        course_id=input$courses, 
        period_from=input$time_range[1],
        period_to=input$time_range[2]
      ),
      latencies=groupLatencies(),
      # add commit latencies to list
      commitLatencies <- groupCommitLatencies(),
      sequences=(groupSequences() %>% group_by(group_id) %>% do(sequence=select(., -group_id)))
    )
  })
  
  output$latencyPlot <- renderPlot({
    groupLatencies() %>%
      ggplot(aes(x=latency)) + stat_ecdf() + 
      geom_vline(xintercept = input$latency_reference) +
      xlab("latency") +
      ylab("% groups") +
      theme_bw()
  })
  
  output$CommitLatencyPlot <- renderPlot({
    groupCommitLatencies() %>%
      ggplot(aes(x=latency)) + stat_ecdf() + 
      geom_vline(xintercept = input$commit_latency_reference) +
      xlab("latency") +
      ylab("% groups") +
      theme_bw()
  })
  
  
  
  # # plot for group latencies
  # # change latency_reference for own slider
  # output$CommitlatencyPlot <- renderPlot({
  #   groupCommitLatencies() %>%
  #     ggplot(aes(x=latency)) + stat_ecdf() + 
  #     geom_vline(xintercept = input$commit_latency_reference) +
  #     xlab("latency") +
  #     ylab("% groups") +
  #     theme_bw()
  # })
  
  output$latencyGroups <- DT::renderDataTable(
    groupLatencies() %>%
      filter(latency > input$latency_reference) %>%
      DT::datatable()
  )
  
  output$CommitLatencyGroups <- DT::renderDataTable(
    groupCommitLatencies() %>%
      #filter(latency > input$commit_latency_reference) %>%
      DT::datatable()
  )
  
  # test data
  ##
  n = c(2, 3, 5) 
  s = c("aa", "bb", "cc") 
  b = c(TRUE, FALSE, TRUE) 
  df = data.frame(n, s, b) 
  ##
  
  testTable <- df
  
  output$test <- DT::renderDataTable(
    df %>% 
      DT::datatable()
  )
  
  output$GroupRepos <- DT::renderDataTable(
    
    getGroupRepositories()
  )
  
  #output$CommitLatencies <- DT::renderDataTable(
    # com latency dfs
 # )
  
  output$downloadGM <- downloadHandler(
    filename = "group_model.json",
    content = function(file) {
      createGroupModel() %>% toJSON() %>% writeLines(file)
    }
  )
  
  observeEvent(input$GM_to_XPS, {
    createGroupModel() %>% sendModelToXPS
    showNotification("Model successfully send to XPS.")
  })
  
  ################
  ## User model ##
  ################
  
  activeDaysModel <- reactive({
    getActiveDaysAll(input$courses) %>%
      mutate(activeDay=ymd(activeDay))
  })
  
  activeDays <- reactive({
    trange <- input$time_range 
    
    activeDaysModel() %>% filter((activeDay >= trange[1]) & (activeDay <= trange[2]))
  })
  
  createUserModel <- reactive({
    list(
      model_metadata=list(
        course_id=input$courses, 
        period_from=input$time_range[1],
        period_to=input$time_range[2]
      ),
      activeDays=(activeDays() %>% group_by(user) %>% do(activeDays=select(., activeDay)))
    )
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
  
  output$downloadUM <- downloadHandler(
    filename = "user_model.json",
    content = function(file) {
      createModel() %>% toJSON() %>% writeLines(file)
    }
  )
  
  observeEvent(input$UM_to_XPS, {
    print("send user model")
    createUserModel() %>% sendModelToXPS
    showNotification("Model successfully send to XPS.")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

