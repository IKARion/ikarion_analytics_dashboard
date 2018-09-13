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
                   start = "2018-05-01",
                   end = "2018-09-30",
                   min = "2018-04-01",
                   max = "2018-12-30",
                   format = "dd.mm.yyyy"),
    sidebarMenu(id="tabs",
      menuItem("User Models", tabName = "user_model", icon = icon("dashboard")),
      menuItem("Group Models", tabName = "group_model", icon = icon("dashboard")),
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
                  selectInput("send_interval_UM", "Interval", c("once", "minute", "hour")),
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
                  selectInput("send_interval_GM", "Interval", c("once", "minute", "hour")),
                  actionButton("GM_to_XPS", "Send")
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
  
  # Update UI
  observe({
    taskList <- list(none="none")
    tasks <- getTaskListForCourse(input$courses)

    if (length(tasks) > 0) {
      taskList[tasks$task_name] <- tasks$task_id  
    }
    
    updateSelectInput(session, "group_tasks", choices = taskList)
  })
  
  
  #################
  ## Group model ##
  #################
  groupData <- callModule(groupLatency, "group_latencies", reactive(input$courses), reactive(input$time_range)) # reactive(input$group_tasks)
  groupSequences <- groupData$sequences
  groupLatencies <- groupData$latencies
  
  # TODO: use task context.
  createGroupModel <- reactive({
    list(
      model_metadata=list(
        course_id=input$courses, 
        period_from=input$time_range[1],
        period_to=input$time_range[2],
        group_task="None"
      ),
      latencies=groupLatencies(),
      # add commit latencies to list
      #commitLatencies <- groupCommitLatencies(),
      #sequences=(groupSequences() %>% group_by(group_id) %>% do(sequence=select(., -group_id)))
      sequences=(groupSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated") %>%  group_by(group_id) %>% do(sequence=select(., -group_id)))
    )
  })
  
  # output$GroupRepos <- DT::renderDataTable(
  #   
  #   getGroupRepositories()
  # )
  
  output$downloadGM <- downloadHandler(
    filename = "group_model.json",
    content = function(file) {
      createGroupModel() %>% toJSON() %>% writeLines(file)
    }
  )
  
  observeEvent(input$GM_to_XPS, {
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
    print("send user model")
    createUserModel() %>%  addScheduledTask(input$send_interval_UM, "script_templates/active_days_template.R", "user_model")
    showNotification("Model successfully send to XPS.")
    callModule(modelsToXps, "xps")
  })
  
  callModule(modelsToXps, "xps")
}

# Run the application 
shinyApp(ui = ui, server = server)


# ###
# 
# # clean html tags from forum posts
# htmlTagClean <- function(htmlString) {
#   return(gsub("<.*?>", "", htmlString))
# }
# 
# # get group sequences for course "http://localhost/ikarion_moodle/course/view.php?id=6" for groups 17, 18
# seq17 <- getGroupSequence("http://localhost/ikarion_moodle/course/view.php?id=6",17)
# seq18 <- getGroupSequence("http://localhost/ikarion_moodle/course/view.php?id=6",18)
# 
# 
# # filter to only countain forum and wiki activities
# s7 <- seq17 %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated")
# s8 <- seq18 %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated")
# 
# 
# ## FORUM ACTIVITIES
# 
# # filter to only contain forum activities
# f_s7 <- s7 %>% filter(verb_id == "http://id.tincanapi.com/verb/replied")
# f_s8 <- s8 %>% filter(verb_id == "http://id.tincanapi.com/verb/replied")
# 
# # add new column with cleaned forum content
# cleaned_df7 <- mutate(f_s7, cleaned_content = htmlTagClean(forum_content))
# cleaned_df8 <- mutate(f_s8, cleaned_content = htmlTagClean(forum_content))
# 
# # try wordcount instead of character count
# #cleaned_df7 <- mutate(cleaned_df7, word_count = wordcount(cleaned_content)) // wordcount function calculates sum of words for all fields in column; should only calculate it for one field
# 
# # add new column with character count for each activity
# cleaned_df7 <- mutate(cleaned_df7, char_count = nchar(cleaned_content))
# cleaned_df8 <- mutate(cleaned_df8, char_count = nchar(cleaned_content))
# 
# # calculate sum of characters for all forum posts for each user
# user_contribution7 <- cleaned_df7 %>%
#   group_by(user_id) %>%
#   summarise(char_count_sum = sum(char_count))
# 
# user_contribution8 <- cleaned_df8 %>%
#   group_by(user_id) %>%
#   summarise(char_count_sum = sum(char_count))
# 
# library(reldist)
# 
# gini(user_contribution8$char_count_sum)
# gini(user_contribution7$char_count_sum)
# 
# # calculate normalized gini coefficient for one group
# gini(user_contribution7$char_count_sum)*length(user_contribution7$char_count_sum)/(length(user_contribution7$char_count_sum)-1)
# gini(user_contribution8$char_count_sum)*length(user_contribution8$char_count_sum)/(length(user_contribution8$char_count_sum)-1)
# 
# 
# ## WIKI CONTRIBUTIONS
# 
# # filter to only contain wiki activities
# w_s7 <- s7 %>% filter(verb_id == "http://id.tincanapi.com/verb/updated")
# w_s8 <- s8 %>% filter(verb_id == "http://id.tincanapi.com/verb/updated")
