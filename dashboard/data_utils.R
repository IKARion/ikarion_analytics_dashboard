require(jsonlite)
require(dplyr)
require(magrittr)

endpoint <- "http://descartes.inf.uni-due.de:5000"

getData <- function(...) {
  
  paste(endpoint, ..., sep="/") %>%
    URLencode %>%
    fromJSON
}

courseName <- function(nameDf) {
  
  nameDf %>% 
    transmute(name=ifelse(is.na(en), de, en))
}

getCourses <- function() {
  
  res <- fromJSON(paste(endpoint, "user_model", "courses", sep="/"))
  cNames <- courseName(res$courses$name)
  
  bind_cols(id=res$courses$courseid, cNames)
}

getUserList <- function(courseId) {
  
  getData("user_model", courseId) %>%
    as_data_frame
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



getGroupLatencyTest <- function() {
  
  data_frame(
    group=1:20,
    latency=rnorm(20,10,sd = 10) %>% abs
  )
}
