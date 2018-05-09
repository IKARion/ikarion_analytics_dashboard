require(jsonlite)
require(dplyr)

getData <- function(dataUrl) {
  
  fromJSON(dataUrl)
}

getCourseList <- function() {
  
  fromJSON("http://descartes.inf.uni-due.de:5000/user_model/courses") %>%
    data_frame(course=.$courses)
}

getUserList <- function(courseId) {
  
  fromJSON(paste("http://descartes.inf.uni-due.de:5000/user_model", courseId, sep="/")) %>%
    data_frame(user=.$users)
}

getActiveDaysUser <- function(userId, courseId) {
  
  fromJSON(paste("http://descartes.inf.uni-due.de:5000/user_model/active_days", userId, courseId, sep="/")) %>%
    data_frame(user=userId, active_day=.)
}

getActiveDaysAll <- function(courseId) {
  
  getUserList(courseId) %>%
    rowwise %>%
    do(getActiveDaysUser(.$user, courseId)) %>%
    count(activeDay)
}

getGroupLatencyTest <- function() {
  
  data_frame(
    group=1:20,
    latency=rnorm(20,10,sd = 10) %>% abs
  )
}