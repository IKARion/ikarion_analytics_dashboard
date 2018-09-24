require(httr)
require(digest)
require(jsonlite)
xpsEndpoint <- appConfig$xpsEndpoint
managementEndpoint <- appConfig$managementEndpoint

buildCustomScript <- function(model, scriptTemplate) {
  
  c(paste("setwd('", getwd(), "')", sep=""),
    paste("course <- '", model$model_metadata$course_id, "'", sep=""),
    paste("pFrom <- '", model$model_metadata$period_from, "'", sep=""),
    paste("pTo <- '", model$model_metadata$period_to, "'", sep="")) %>%
    append(readLines(scriptTemplate)) 
}

addScheduledTask <- function(model, interval, scriptTemplate, label) {

  sendModelToXPS(model)
  
  if (interval %in% c("minute", "hour")) {
    frequencyMinutes <- 1
    if (interval == "hour") {
      frequencyMinutes <- 60
    }
    filename <- digest(model$model_metadata) # hash of metadata becomes filename to avoid duplicates.
    buildCustomScript(model, scriptTemplate) %>%
      writeLines(filename)
    
    description <- c(label, as.character(model$model_metadata)) %>%
      paste(collapse="\n")
    uri <- tools::file_path_as_absolute(filename)
    
    paste(managementEndpoint, "add_r_script", replaceUrlChars(description), frequencyMinutes, 
          replaceUrlChars(uri), sep="/") %>% URLencode %>% GET
  }
}

getScheduledTasks <- function() {
  
  paste(managementEndpoint, "jobs", sep="/") %>%
    URLencode %>%
    fromJSON
}

sendModelToXPS <- function(model) {
  
  fail <- TRUE
  
  while (fail) {
    
    tryCatch({
      
      res <- POST(xpsEndpoint, add_headers(Authorization = "Token b5b41fac0361d157d9673ecb926af5ae"), 
           body = toJSON(model), content_type_json())
      fail <- FALSE
    },
    error = function(err) {
      
      print(err)
    })
  }
  res
}
