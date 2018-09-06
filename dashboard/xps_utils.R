require(httr)
require(digest)
require(jsonlite)
xpsEndpoint <- "http://[host]:[port]/"
managementEndpoint <- "http://descartes.inf.uni-due.de:5000/management"

buildCustomScript <- function(model, scriptTemplate) {
  
  c(paste("course <-", model$model_metadata$course_id),
    paste("pFrom <-", model$model_metadata$period_from),
    paste("pTo <-", model$model_metadata$period_to)) %>%
    append(readLines(scriptTemplate)) 
}

addScheduledTask <- function(model, interval, scriptTemplate, label) {
  
  sendModelToXPS(model)
  
  if (interval %in% c("minute", "hour")) {
    
    if (interval == "hour") {
      interval <- interval * 60
    }
    filename <- digest(model$model_metadata) # hash of metadata becomes filename to avoid duplicates.
    buildCustomScript(model, scriptTemplate) %>%
      writeLines(filename)
    
    description <- c(label, as.character(model$model_metadata)) %>%
      paste(collapse="\n")
    uri <- tools::file_path_as_absolute(filename)
    
    paste(managementEndpoint, "add_r_script", description, interval, uri) %>% GET
  }
}

getScheduledTasks <- function() {
  
  paste(managementEndpoint, "jobs", sep="/") %>%
    URLencode %>%
    fromJSON
}

sendModelToXPS <- function(model) {
  
  POST(xpsEndpoint, add_headers(Authorization = "Token 285e19f20beded7d215102b49d5c09a0"), body = toJSON(model))
}
