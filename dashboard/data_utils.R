require(jsonlite)
require(dplyr)
require(magrittr)

#endpoint <- "http://descartes.inf.uni-due.de:5000"
endpoint <- "http://0.0.0.0:5000" 

replaceUrlChars <- function(string) {
  string <- gsub("/", "$slash$", string)
  string <- gsub("?", "$qmark$", string, fixed = T)
  #print(string)
}

getData <- function(...) {
  
  #s2 <- gsub("/", "#slash#", ...)
  #print("***after")
  #print(s2)
  #s2 <- gsub("?", "#qmark#", s2, fixed = T)
  paste(endpoint, ..., sep="/") %>%
    URLencode %>% #%T>%
    #print("1") %>% 
    #print %>%
    #gsub("?", "##") %>% 
    print %>% 
    #print("2") %>% 
    fromJSON %>% 
    extract2("data")
}

###
# ENCODE GROUP NOT COMPLETE URL
###

courseName <- function(nameDf) {
  #print(nameDf)
  #nameDf %>% 
  #transmute(name=ifelse(is.na(en), de, en))
}

getCourses <- function() {
  
  res <- getData("user_model/courses") 
  #%>% 
  #  distinct(courseid, .keep_all = TRUE)
  #cNames <- courseName(res$name)
  
  #print(res)
  #print(res$data)
  
  #bind_cols(id=res$courseid, cNames)
  res
}

getUserList <- function(courseId) {
  #print("***12")
  #print(courseId)
  #print("***new")
  #print(replaceUrlChars(courseId))
  courseId <- replaceUrlChars(courseId)
  data_frame(
    user=getData("user_model", courseId)
  )
}

getGroupList <- function(courseId) {
  #print(getData("user_model/groups_for_course", courseId))
  courseId <- replaceUrlChars(courseId)
  data_frame(
    group=getData("user_model/groups_for_course", courseId) 
  )
  
}

getActiveDaysUser <- function(userId, courseId) {
  #print("***10")
  #print(userId)
  #print(courseId)
  courseId <- replaceUrlChars(courseId)
  
  data_frame(
    user=substring(digest::sha1(userId),1,8), activeDay=getData("user_model/active_days", userId, courseId)
    
    #user=userId, activeDay=getData("user_model/active_days", userId, courseId)
  )
}

getActiveDaysAll <- function(courseId) {
  #print("***9")
  #print(courseId)
  #print(getUserList(courseId))
  #print(courseId)
  courseId <- replaceUrlChars(courseId)
  getUserList(courseId) %>%
    rowwise %>%
    do(getActiveDaysUser(.$user, courseId)) %>% ungroup
}

groupData <- NULL

getGroupSequence <- function(courseId, groupId) {
  print("***user_model/group_activities")
  courseId <- replaceUrlChars(courseId)
  groupData <<-getData("user_model/group_activities", courseId, groupId) %>% as_data_frame #%>% 
    #print()
  groupData
}


getGroupRepositories <- function() {
  df <- data_frame(repo=getData("user_model/repositories"))
  #res <- getData("user_model/repositories") %>% as_data_frame
  #print(df)
}

getGroupSequenceGit <- function(repo) {
  #groups <- getData("user_model/repositories")
  repo <- replaceUrlChars(repo)
  
  data <- getData("user_model/repo_activities",repo) %>% as_data_frame
  
  # get activities for groups
  #print("***groups")
  #print(groups)
}

getGitSequencesAll <- function() {
  
  repos <- getGroupRepositories()
  #courseId <- replaceUrlChars(courseId)
  getGroupRepositories() %>% 
    rowwise %>% 
    do(getGroupSequenceGit(.$repo)) #%>% 
    #print()
  
}

getGroupSequencesAll <- function(courseId) {
  
  courseId <- replaceUrlChars(courseId)
  getGroupList(courseId) %>%
    rowwise %>%
    do(getGroupSequence(courseId, .$group)) #%>% 
    #print()
}

getGitCommitLatencies <- function(start, end) {
  print("***getGitCommitLatencies")
  
  start %<>% as.POSIXct %>% as.integer
  end %<>% as.POSIXct %>% as.integer
  
  #print("***15")
  #getGroupSequenceGit()
  #print(start)
  #print(end)
  
  
  
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
  
  print("***latencies")
  #print(groupSequences)
  
  start %<>% as.POSIXct %>% as.integer
  end %<>% as.POSIXct %>% as.integer
  
  print("***15")
  #getGroupSequenceGit()
  #print(start)
  #print(end)
  
  
  
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
  
  print("**gs")
  print(groupGitSequences)
  print("***commit latencies")
  print("start")
  print(start)
  print("end")
  print(end)
  #browser()
  #print(groupSequences)
  
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
