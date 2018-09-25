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
  
  getTaskList <- reactive({
    getTaskListForCourse(input$courses)
  })
  
  getGroupsAndUsers <- reactive ({
    getGroupsAndUsersForCourse(input$courses, input$group_tasks)
  })
  
  getAllGroupLatencies <- reactive({
    # add latency = 0 for groups without forum activity
    latencies <- groupLatencies()
    allGroups <- getGroupsAndUsers() %>% 
      select(c("group_id")) %>% 
      mutate(latency = 0)
    
    missing <- anti_join(allGroups, latencies, by = c("group_id"))
    complete_data <- full_join(missing, latencies)
    
    complete_data
    
    #browser()
    
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
    list(
      model_metadata=list(
        course_id=input$courses, 
        period_from=input$time_range[1],
        period_to=input$time_range[2],
        task_context=list(selectedTask()),
        groups = getGroupsAndUsers()
      ),
      
      # latency list that only contains groups that were active in the forum
      #average_latencies=groupLatencies(),
      
      # latency list with all groups, forum inative groups have a latency of 0
      average_latencies=getAllGroupLatencies(),
      
      
      work_imbalance = calculateWorkImbalance(),

      text_contributions_forum = (calculateForumWordcount(groupTaskSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/replied"))),
      text_contributions_wiki = (calculateWikiWordcount(groupTaskSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/updated"))),
      
      
      
      # add commit latencies to list
      #commitLatencies <- groupCommitLatencies(),
      #sequences=(groupSequences() %>% group_by(group_id) %>% do(sequence=select(., -group_id)))
      
      # complete group sequence with content
      #group_sequences=(groupSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated") %>%  group_by(group_id) %>% do(sequence=select(., -group_id)))
      
      # complete group sequence without content
      #group_sequences=( groupSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated") %>%  group_by(group_id) %>% do(sequence=select(., -c(group_id, content))))
    
      # group TASK sequences containing relavant activities
      # USE THIS for current group model specification
      group_sequences=( groupTaskSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated") %>%  group_by(group_id) %>% do(sequence=select(., -c(group_id, content))))
      
    )
  })
  
  # TODO insert non active users with wordcount = 0, currently only active users are accounted for
  calculateWorkImbalance <- function() {
    forum_data <- calculateForumWordcountGini(groupTaskSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/replied"))
    wiki_data <- calculateWikiWordcountGini(groupTaskSequences() %>% filter(verb_id == "http://id.tincanapi.com/verb/updated"))
    

    # get list of all users
    groupsAndUsers <- getGroupsAndUsers()

    # see which users do not appear in the data and add forum and wiki wordcount of 0 so that all users that are assigned to a group 
    # are accounted for in the gini calculation
    
    # generate table for all groups and users with:
      # user_forum_wordcount = 0 and user_wiki_wordcount = 0
    all_users <- unnest(groupsAndUsers)
    colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"

    all_users <- all_users %>%
      select(c(group_id, user_id)) %>% 
      mutate(user_forum_wordcount = 0) %>%
      mutate(user_wiki_wordcount = 0)

    # dataframe with all users (active + inactive) 
    merge <- NULL

    # only activity in forum
    if (is.null(wiki_data) & !is.null(forum_data)) {
      merge <- forum_data %>%
        mutate(user_wiki_wordcount = 0)

    # only activity in wiki
    } else if (!is.null(wiki_data) & is.null(forum_data)) {
      merge <- wiki_data %>%
        mutate(user_forum_wordcount = 0)

    # activity in forum and wiki
    } else if (!is.null(wiki_data) & !is.null(forum_data)) {
      merge <- full_join(forum_data, wiki_data) %>%
        replace_na(list(user_forum_wordcount = 0, user_wiki_wordcount = 0))
    } else  {
      
      # no activity at all
      # only possible if groups are logged on group creation without user activity
      print("no wiki or forum activity logged")
    }
    
    
    # add all inactive users to the data
    
    # missing users
    missing <- anti_join(all_users, merge, by = c("group_id", "user_id"))

    complete_data <- full_join(missing, merge) %>% 
      group_by(user_id) %>%
      # forum contribution weight:  3
      # wiki contibution  weight:   1
      mutate(overall_wordcount = sum(3*user_forum_wordcount, user_wiki_wordcount))
    
    
    gini_data <- complete_data %>% 
      group_by(group_id) %>% 
      summarise(gini_index = gini(overall_wordcount)*length(overall_wordcount)/(length(overall_wordcount)-1))
    
    gini_data
  }
  

  # calculate Forum wordcunt for every user in every group (for gini calculation)
  calculateForumWordcountGini <- function(df) {
    if(nrow(df) > 0) {
      data <- df %>% 
        mutate(charcount = nchar(htmlTagClean(content))) %>% 
        mutate(wordcount = wordcount(htmlTagClean(content))) %>% 
        group_by(group_id) %>% 
        group_by(user_id, add = T) %>% 
        summarise(user_forum_wordcount = sum(wordcount))
    } else {
      #print("no forum activity")
    }
    
    
  }

  
  # calculate forum wordcount and format properly for group model
  calculateForumWordcount <- function(df) {
    
    final_data <- NULL
    
    # get list of all users
    groupsAndUsers <- getGroupsAndUsers()
    
    # see which users do not appear in the data and add forum and wiki wordcount of 0 so that all users that are assigned to a group 
    # are accounted for in the gini calculation
    
    # generate table for all groups and users with:
    # user_forum_wordcount = 0 and user_wiki_wordcount = 0
    all_users <- unnest(groupsAndUsers)
    colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
    
    all_users <- all_users %>%
      select(c(group_id, user_id)) %>% 
      mutate(user_wordcount = 0)
    
    if(nrow(df) > 0) {
      data <- df %>% mutate(charcount = nchar(htmlTagClean(content))) %>% 
        mutate(wordcount = wordcount(htmlTagClean(content))) %>% 
        group_by(group_id) %>% 
        group_by(user_id, add = T) %>% 
        summarise(user_wordcount = sum(wordcount))
      
      missing <- anti_join(all_users, data, by = c("group_id", "user_id"))
      
      complete_data <- full_join(missing, data)
   
      final_data <- complete_data %>% 
        group_by(group_id) %>% 
        do(group_members=select(., -group_id))  
      
    } else {
      
      # data with all inactive users
      final_data <- all_users %>% 
        group_by(group_id) %>% 
        do(group_members=select(., -group_id)) 
    }
    
    final_data
  }
  
  # calculate Wiki wordcount for every user in every group (for gini calculation)
  calculateWikiWordcountGini <- function(df) {

    if(nrow(df) > 0) {
      data <- df %>%
        #mutate(charcount = nchar(htmlTagClean(content))) %>%
        mutate(wordcount = wordcount(htmlTagClean(content))) %>%
        group_by(group_id) %>%
        #group_by(user_id, add = T) %>%
        arrange(timestamp, .by_group = T) %>%
        mutate(textchange = wordcount - lag(wordcount)) # calculate textchange as difference between current and last revision
      
      # correct value for first revisions (since textchange for first posts are "NA")
      firstRevisions <- is.na(data$textchange)
      data$textchange[firstRevisions] <- data$wordcount[firstRevisions]
      
      # correct value for revisions that are shorter than the last on (change negative numbers to 0)
      negativeWordcounts <- data$textchange < 0
      data$textchange[negativeWordcounts] <- 0
      
      # calculate overall wordcount for each user
      sum_wordcounts <- data %>%
        group_by(group_id) %>%
        group_by(user_id, add = T) %>%
        summarise(user_wiki_wordcount = sum(textchange))
      
      sum_wordcounts
      
    } else {
      #print("no wiki activity")
    }
    
    
  }
  
  # calculate wiki wordcount and format properly for group model
  calculateWikiWordcount <- function(df) {
    
    final_data <- NULL
    
    # get list of all users
    groupsAndUsers <- getGroupsAndUsers()
    
    # see which users do not appear in the data and add wiki wordcount of 0 so that all users that are assigned to a group 
    # are accounted for in the calculation
    
    # generate table for all groups and users with:
    # user_forum_wordcount = 0 and user_wiki_wordcount = 0
    all_users <- unnest(groupsAndUsers)
    colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
    
    all_users <- all_users %>%
      select(c(group_id, user_id)) %>% 
      mutate(user_wordcount = 0)
    
    if (nrow(df) > 0) {
      data <- df %>% 
        #mutate(charcount = nchar(htmlTagClean(content))) %>% 
        mutate(wordcount = wordcount(htmlTagClean(content))) %>% 
        # only group by "group_id" for "wordcount" calculation since revisions between different users in the group need to be considered for "textchange" calculation 
        group_by(group_id) %>% 
        arrange(timestamp, .by_group = T) %>%
        mutate(textchange = wordcount - lag(wordcount)) # calculate textchange as difference between current and last revision
      
      # correct value for first revisions (since textchange for first posts are "NA")
      firstRevisions <- is.na(data$textchange)
      data$textchange[firstRevisions] <- data$wordcount[firstRevisions]
      
      # correct value for revisions that are shorter than the last on (change negative numbers to 0)
      negativeWordcounts <- data$textchange < 0
      data$textchange[negativeWordcounts] <- 0
      
      # calculate overall wordcount for each user
      sum_wordcounts <- data %>% 
        group_by(group_id) %>% 
        group_by(user_id, add = T) %>% 
        summarise(user_wordcount = sum(textchange))
      
      missing <- anti_join(all_users, sum_wordcounts, by = c("group_id", "user_id"))
      
      complete_data <- full_join(missing, sum_wordcounts)
      
      # format according to model specification
      final_data <- complete_data %>% 
        group_by(group_id) %>% 
        do(group_members=select(., -group_id))
      
    } else {
      
      # data with all inactive users
      final_data <- all_users %>% 
        group_by(group_id) %>% 
        do(group_members=select(., -group_id))
    }
    
    final_data
    
  }
  
  # clean html tags from forum posts
  htmlTagClean <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
  }
  
  # output$GroupRepos <- DT::renderDataTable(
  #   
  #   getGroupRepositories()
  # )
  
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
captureStackTraces(
shinyApp(ui = ui, server = server))

