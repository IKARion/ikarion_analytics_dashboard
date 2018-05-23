require(jsonlite)
require(dplyr)
require(magrittr)

endpoint <- "http://localhost:5000"

getData <- function(...) {
  
  paste(endpoint, ..., sep="/") %>%
    URLencode %>% # %T>%
    #print %>%
    fromJSON %>% 
    extract2("data")
}

courseName <- function(nameDf) {
  
  nameDf %>% 
    transmute(name=ifelse(is.na(en), de, en))
}

getCourses <- function() {
  
  res <- getData("user_model/courses") %>% 
    distinct(courseid, .keep_all = TRUE)
  cNames <- courseName(res$name)
  
  bind_cols(id=res$courseid, cNames)
}

getUserList <- function(courseId) {
  
  data_frame(
    user=getData("user_model", courseId)
  )
}

getGroupList <- function(courseId) {
  
  data_frame(
    group=getData("user_model/groups_for_course", courseId)
  )
}

getActiveDaysUser <- function(userId, courseId) {
  
  data_frame(
    user=userId, activeDay=getData("user_model/active_days", userId, courseId)
  )
}

getActiveDaysAll <- function(courseId) {
  
  getUserList(courseId) %>%
    rowwise %>%
    do(getActiveDaysUser(.$user, courseId)) %>% ungroup
}

getGroupSequence <- function(courseId, groupId) {
  
  getData("user_model/group_activities", courseId, groupId) %>% as_data_frame
}

getGroupSequencesAll <- function(courseId) {
  
  getGroupList(courseId) %>%
    rowwise %>%
    do(getGroupSequence(courseId, .$group))
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

getGroupLatencies <- function(courseId, start, end) {
  
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