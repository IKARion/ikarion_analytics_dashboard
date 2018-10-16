require(jsonlite)
require(dplyr)
require(magrittr)
require(lubridate)

endpoint <- appConfig$dataEndpoint

replaceUrlChars <- function(string) {
  string <- gsub("/", "$slash$", string)
  string <- gsub("?", "$qmark$", string, fixed = T)
}

getData <- function(...) {
  
  paste(endpoint, ..., sep="/") %>%
    URLencode %>%
    #print() %>% 
    fromJSON %>%
    extract2("data")
}

###
# ENCODE GROUP NOT COMPLETE URL
###

courseName <- function(nameDf) {
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

getGroupsAndUsersForCourse <- function(courseId, task) {
  courseId <- replaceUrlChars(courseId)
  data <- getData("groups/groups_for_task", courseId, task) %>% 
    do(data.frame(select(., c(id, group_members))))
  
  colnames(data)[which(names(data) == "id")] <- "group_id"
  data
  
}

getGroupListForCourse <- function(courseId) {

  courseId <- replaceUrlChars(courseId)
  data_frame(
    group=getData("groups/groups_for_course", courseId) 
  )
  
}

getGroupListForTask <- function(courseId, taskId) {

  courseId <- replaceUrlChars(courseId)
  
  groups <- getData("groups/groups_for_task", courseId, taskId)
  
  groups <- select(groups, id)
  groups <- rename(groups, group = id)
  
  groups
}

getActiveDaysUser <- function(userId, courseId) {

  courseId <- replaceUrlChars(courseId)
  
  data_frame(
    #user=substring(digest::sha1(userId),1,8), activeDay=getData("user_model/active_days", userId, courseId)
    user=userId, activeDay=getData("user_model/active_days", userId, courseId)
  )
}

getActiveDaysAll <- function(courseId) {
  courseId <- replaceUrlChars(courseId)
  getUserList(courseId) %>%
    rowwise %>%
    do(getActiveDaysUser(replaceUrlChars(.$user), courseId)) %>% ungroup
}

getGroupSequence <- function(courseId, groupId) {
  
  courseId <- replaceUrlChars(courseId)
  getData("groups/group_activities", courseId, groupId) %>% as_data_frame #%>% 
}

getGroupTaskSequence <- function(courseId, groupId, taskId) {
  
  courseId <- replaceUrlChars(courseId)
  data <- getData("groups/grouptask_activities", courseId, groupId, taskId) %>% as_data_frame
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

# old function: get all sequences for course
# use getGroupTaskSequence instead
getGroupSequencesAll <- function(courseId) {

  courseId <- replaceUrlChars(courseId)
  getGroupListForCourse(courseId) %>%
    rowwise %>%
    do({print(.$group)
      getGroupSequence(courseId, .$group)}
      )
}

getGroupTaskSequencesAll <- function(courseId, taskId) {
  courseId <- replaceUrlChars(courseId)

  getGroupListForTask(courseId, taskId) %>%
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

  data <- groupSequences %>%
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
    }) #%>% 
    #mutate(group=paste("Group", group_id))
  
  
  data
  
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
    mutate(group=paste("Group", group_id))
  
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
