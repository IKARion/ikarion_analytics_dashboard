#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("config.R")
source("setup.R")
source("data_utils.R")
source("xps_utils.R")

# Load modules
source("modules/user_active_days_module.R")
source("modules/group_latency_module.R")
source("modules/xps_module.R")

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
    selectInput("group_tasks", "Group Task", list(none="none")),
    
    dateRangeInput("time_range", 
                   label = "Period", 
                   start = "2018-09-01",
                   end = "2019-05-01",
                   min = "2018-04-01",
                   max = "2019-12-30",
                   format = "dd.mm.yyyy"),
    sidebarMenu(id="tabs", 
      menuItem("User Models", tabName = "user_model", icon = icon("dashboard")),
      menuItem("Group Models", tabName = "group_model", icon = icon("dashboard")),
      checkboxInput("content", "include \"content\"", value = FALSE),
      #checkboxInput("req_answer", "include \"requires_answer\"", value = FALSE),
      menuItem("XPS", tabName = "xps", icon = icon("calendar"))
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
                  selectInput("send_interval_UM", "Interval", c("once", "minute", "10 minutes", "hour")),
                  actionButton("UM_to_XPS", "Send")
                )
              ),
              userActiveDaysUI("user_active_days")
      ),
      
      
      tabItem("group_model",
              fluidRow(
                downloadButton("downloadGM", "Download Group Model"),
                box(
                  title = "Send group model to XPS",
                  selectInput("send_interval_GM", "Interval", c("once", "minute", "10 minutes", "hour")),
                  actionButton("GM_to_XPS", "Send"),
                  actionButton("Add_all_relevant_jobs",  "Add jobs for relevant tasks")
                )
              ),
              groupLatencyUI("group_latencies")#,
              #commitLatencyUI("commit_latencies")
            
      ),
      tabItem("xps", 
              modelsToXpsUI("xps")
      )
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  getTaskList <- reactive({
    getTaskListForCourse(input$courses)
  })
  
  getGroupsAndUsers <- reactive ({
    getGroupsAndUsersForCourse(input$courses, input$group_tasks)
  })
  
  getAllGroupLatencies <- reactive({
    
    data <- getAllLatenciesFun(groupLatencies(), getGroupsAndUsers())
    data
    
  })
  
  
  # Update UI
  observe({
    taskList <- list(none="none")
    tasks <- getTaskList()
    if (length(tasks) > 0) {
      taskList[tasks$task_name] <- tasks$task_id  
    }
    
    updateSelectInput(session, "group_tasks", choices = taskList)
  })
  
  selectedTask <- reactive({getTaskList() %>% filter(task_id == input$group_tasks)})
  

  
  #################
  ## Group model ##
  #################

  groupData <- callModule(groupLatency, "group_latencies", reactive(input$courses), reactive(input$group_tasks), reactive(input$time_range))

  groupSequences <- groupData$sequences
  groupTaskSequences <- groupData$taskSequences
  groupLatencies <- groupData$latencies
  
  # TODO: use task context.
  createGroupModel <- reactive({
    
    model <- NULL
    
    # empty models no longer supported
    # always send complete model with all values 0 if no rel. activity took place
    
    
    # if (dim(groupTaskSequences())[1] == 0) {
    #   # send empty group model with task and groups
    #   model <- buildEmptyGroupModel(input$courses, 
    #                                 selectedTask(),
    #                                 getGroupsAndUsers()
    #   )
    # } else { }
    
    # send normal group model with all additional info
    model <- buildGroupModel(input$courses,
                             #input$time_range[1],
                             #input$time_range[2],
                             selectedTask(),
                             getGroupsAndUsers(), 
                             
                             calculateWorkImbalance(),
                             # groupSelfAssessments(),
                             # groupWeightedForumWordcount(),
                             # groupWeightedWikiWordcount(),
                             calculateGroupSelfAssessments(),
                             
                             
                             #calculateWeightedForumWordcount(),
                             (calculateForumWordcount(groupTaskSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/replied"))),
                             
                             
                             calculateWeightedWikiWordcount(),
                             #(calculateWikiWordcount(groupTaskSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/updated"))),
                             

                             #getAllGroupLatencies(),
                    
                             
                             #(calculateWikiWordcount(groupTaskSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/updated"))),
                             #(groupTaskSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated") %>%  group_by(group_id) %>% do(sequence=select(., -c(group_id, content))))
                             if(input$content == TRUE) {
                               generateGroupTaskSequencesWithContent(groupTaskSequences(), getGroupsAndUsers(), selectedTask())
                             } else {
                               generateGroupTaskSequences(groupTaskSequences(), getGroupsAndUsers(), selectedTask())
                             }
                             # generateGroupTaskSequencesWithContent(groupTaskSequences(), getGroupsAndUsers())
    )
    
    
    
    model
  }) 
  
  # necessary because getGroupSelfAssessmentsAll is also called in group_template.R in the backend (outside of the dashboard) 
  calculateGroupSelfAssessments <- function() {
    data <- getGroupSelfAssessmentsAll(input$courses, input$group_tasks, as.numeric(Sys.time()), getGroupsAndUsers())

  }
  
  # necessary because getGroupWeightedForumWordcountAll is also called in group_template.R in the backend (outside of the dashboard) 
  calculateWeightedForumWordcount <- function() {
    data <- getGroupWeightedForumWordcountAll(input$courses, input$group_tasks, as.numeric(Sys.time()), getGroupsAndUsers())
  }
  
  # necessary because getGroupWeightedWikiWordcountAll is also called in group_template.R in the backend (outside of the dashboard) 
  calculateWeightedWikiWordcount <- function() {
    data <- getGroupWeightedWikiWordcountAll(input$courses, input$group_tasks, as.numeric(Sys.time()), getGroupsAndUsers())
  }
  
  
  
  
  # calculate work imbalance
  calculateWorkImbalance <- function() {
    data <- calculateWorkImbalanceFun(groupTaskSequences(), getGroupsAndUsers())
  }

  
  # calculate forum wordcount and format properly for group model
  calculateForumWordcount <- function(df) {
    data <- calculateForumWordcountFun(df, getGroupsAndUsers())
    data
  }
  
  # calculate wiki wordcount and format properly for group model
  calculateWikiWordcount <- function(df) {
    data <- calculateWikiWordcountFun(df, getGroupsAndUsers())
    data
  }
  
  
  output$downloadGM <- downloadHandler(
    filename = "group_model.json",
    content = function(file) {
      createGroupModel() %>% toJSON(simplifyVector = T) %>% writeLines(file)
    }
  )
  
  observeEvent(input$GM_to_XPS, {
    
    createGroupModel() %>% addScheduledTask(input$send_interval_GM, "script_templates/group_template.R", "group_model")
    showNotification("Model successfully send to XPS.")
    callModule(modelsToXps, "xps")
  })
  
  observeEvent(input$Add_all_relevant_jobs, {
    
    
    # get all tasks
    all_tasks <- getTaskListForCourse(input$courses)
    
    # get all jobs
    scheduled_tasks <- getScheduledTasks()
    
    # generate list of relevant task without job
    
    
    # add job for each relevant task
    
    
    createGroupModel() %>% addScheduledTask(input$send_interval_GM, "script_templates/group_template.R", "group_model")
    showNotification("Model successfully send to XPS.")
    callModule(modelsToXps, "xps")
  })
  
  
  
  
  ################
  ## User model ##
  ################
  activeDays <- callModule(userActiveDays, "user_active_days", reactive(input$courses), reactive(input$time_range))
  
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
  
  output$downloadUM <- downloadHandler(
    filename = "user_model.json",
    content = function(file) {
      createUserModel() %>% toJSON() %>% writeLines(file)
    }
  )
  
  observeEvent(input$UM_to_XPS, {
    #print("send user model")
    createUserModel() %>%  addScheduledTask(input$send_interval_UM, "script_templates/active_days_template.R", "user_model")
    showNotification("Model successfully send to XPS.")
    callModule(modelsToXps, "xps")
  })
  
  callModule(modelsToXps, "xps")
}

# Run the application
options(shiny.port=5341)
captureStackTraces(
shinyApp(ui = ui, server = server))

