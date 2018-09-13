require(jsonlite)
require(dplyr)
require(magrittr)

#endpoint <- "http://descartes.inf.uni-due.de:5000"
endpoint <- "http://localhost:5000" 

replaceUrlChars <- function(string) {
  string <- gsub("/", "$slash$", string)
  string <- gsub("?", "$qmark$", string, fixed = T)
}

getData <- function(...) {
  
  paste(endpoint, ..., sep="/") %>%
    URLencode %>%
    fromJSON %>% 
    extract2("data")
}

###
# ENCODE GROUP NOT COMPLETE URL
###

courseName <- function(nameDf) {
  print(nameDf)
  nameDf %>% 
  transmute(name=ifelse(is.na(en), de, en))
}

getCourses <- function() {
  
  getData("user_model/courses") %>% 
    distinct(courseid, .keep_all = TRUE)
}

getUserList <- function(courseId) {

  courseId <- replaceUrlChars(courseId)
  data_frame(
    user=getData("user_model", courseId)
  )
}

getTaskListForCourse <- function(courseId) {
  
  courseId <- replaceUrlChars(courseId)
  getData("groups/group_tasks", courseId)
}

getGroupListForCourse <- function(courseId) {

  courseId <- replaceUrlChars(courseId)
  data_frame(
    group=getData("groups/groups_for_course", courseId) 
  )
}

getGroupListForTask <- function(taskId) {
  
  taskId <- replaceUrlChars(taskId)
  data_frame(
    group=getData("groups/groups_for_task", taskId) 
  )
}

getActiveDaysUser <- function(userId, courseId) {

  courseId <- replaceUrlChars(courseId)
  
  data_frame(
    user=substring(digest::sha1(userId),1,8), activeDay=getData("user_model/active_days", userId, courseId)
  )
}

getActiveDaysAll <- function(courseId) {
  print("get active days all")
  courseId <- replaceUrlChars(courseId)
  getUserList(courseId) %>%
    rowwise %>%
    do(getActiveDaysUser(.$user, courseId)) %>% ungroup
}

getGroupSequence <- function(courseId, groupId) {
  
  courseId <- replaceUrlChars(courseId)
  getData("groups/group_activities", courseId, groupId) %>% as_data_frame #%>% 
}

getGroupTaskSequence <- function(courseId, groupId, taskId) {
  courseId <- replaceUrlChars(courseId)
  
  getData("groups/grouptask_activities", courseId, groupId, taskId)
}

getGroupRepositories <- function() {
  data_frame(repo=getData("groups/repositories"))
}

getGroupSequenceGit <- function(repo) {

  repo <- replaceUrlChars(repo)
  data <- getData("groups/repo_activities",repo) %>% as_data_frame
}

getGitSequencesAll <- function() {
  
  repos <- getGroupRepositories()
  #courseId <- replaceUrlChars(courseId)
  getGroupRepositories() %>% 
    rowwise %>% 
    do(getGroupSequenceGit(.$repo))
  
}

getGroupSequencesAll <- function(courseId) {
  
  courseId <- replaceUrlChars(courseId)
  getGroupListForCourse(courseId) %>%
    rowwise %>%
    do(getGroupSequence(courseId, .$group))
}

getGroupTaskSequencesAll <- function(courseId, taskId) {
  
  courseId <- replaceUrlChars(courseId)
  getGroupListForTask(taskId) %>%
    rowwise %>%
    do(getGroupTaskSequence(courseId, .$group, taskId))
}

getGitCommitLatencies <- function(start, end) {
  
  start %<>% as.POSIXct %>% as.integer
  end %<>% as.POSIXct %>% as.integer
  
  groupSequences %>%
    group_by(group_id) %>%
    filter((verb_id == "http://id.tincanapi.com/verb/replied") & 
             (timestamp >= start) & (timestamp <= end)) %>%
    arrange(timestamp) %>%
    do({
      ts <- append(start, .$timestamp, end)
      mLatency <- sapply(2:length(ts), function(i) {
        
        seconds_to_period(ts[i] - ts[i - 1]) %>% time_length("hour")
      }) %>% mean %>% round(2)
      
      data_frame(latency=mLatency)
    }) %>% 
    mutate(group=paste("Group", group_id))
  
}


getGroupLatencies2 <- function(groupSequences, start, end) {
  
  start %<>% as.POSIXct %>% as.integer
  end %<>% as.POSIXct %>% as.integer

  groupSequences %>%
    group_by(group_id) %>%
    filter((verb_id == "http://id.tincanapi.com/verb/replied") & 
             (timestamp >= start) & (timestamp <= end)) %>%
    arrange(timestamp) %>%
    do({
      ts <- append(start, .$timestamp, end)
      mLatency <- sapply(2:length(ts), function(i) {
        
        seconds_to_period(ts[i] - ts[i - 1]) %>% time_length("hour")
      }) %>% mean %>% round(2)
      
      data_frame(latency=mLatency)
    }) %>% 
    mutate(group=paste("Group", group_id))
}

# calculate the latencies between activities in the bitbucket 
getCommitLatencies <- function(groupGitSequences, start, end) {
  
  start %<>% as.POSIXct %>% as.integer
  end %<>% as.POSIXct %>% as.integer
  
  groupGitSequences %>%
    group_by(group_id) %>%
    #filter((verb_id == "http://id.tincanapi.com/verb/replied") & 
    filter((verb_id == "http://activitystrea.ms/schema/1.0/update") & 
             (timestamp >= start) & (timestamp <= end)) %>%
    arrange(timestamp) %>%
    do({
      ts <- append(start, .$timestamp, end)
      mLatency <- sapply(2:length(ts), function(i) {
        
        seconds_to_period(ts[i] - ts[i - 1]) %>% time_length("hour")
      }) %>% mean %>% round(2)
      
      data_frame(latency=mLatency)
    }) %>% 
    mutate(group=paste("Group", group_id)) #%>% 
    #print()
  
}

getGroupLatencies <- function(courseId, start, end) {
  
  courseId <- replaceUrlChars(courseId)
  getGroupSequencesAll(courseId) %>%
    group_by(group_id) %>%
    filter((verb_id == "http://id.tincanapi.com/verb/replied") & 
             (timestamp >= start) & (timestamp <= end)) %>%
    arrange(timestamp) #%>%
  # do({
  #   ts <- append(start, .$timestamp, end)
  #   mLatency <- sapply(2:length(ts), function(i) {
  #     
  #     seconds_to_period(ts[i] - ts[i - 1]) %>% time_length("hour")
  #   }) %>% mean
  #   
  #   data_frame(group=paste("Group", .$group_id), latency=mLatency)
  # })
}

getGroupLatencyTest <- function() {
  
  data_frame(
    group=1:20,
    latency=rnorm(20,10,sd = 10) %>% abs
  )
}
