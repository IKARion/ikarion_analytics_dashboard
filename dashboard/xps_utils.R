require(httr)
require(digest)
require(jsonlite)
require(ngram)
require(tidyr)
require(reldist)

xpsEndpoint <- appConfig$xpsEndpoint
managementEndpoint <- appConfig$managementEndpoint

buildCustomScript <- function(model, scriptTemplate) {
  
  c(paste("setwd('", getwd(), "')", sep=""),
    paste("course <- '", model$model_metadata$course_id, "'", sep=""),
    paste("pFrom <- '", model$model_metadata$period_from, "'", sep=""),
    paste("pTo <- '", model$model_metadata$period_to, "'", sep=""),
    paste("taskId <- '", model$model_metadata$task_context[[1]]$task_id, "'", sep=""))  %>%
    append(readLines(scriptTemplate)) 
}

calculateWorkImbalanceFun <- function(groupTaskSequences, groupsAndUsers) {
  
  forum_data <- calculateForumWordcountGini(groupTaskSequences %>% filter(verb_id == "http://id.tincanapi.com/verb/replied"))
  wiki_data <- calculateWikiWordcountGini(groupTaskSequences %>% filter(verb_id == "http://id.tincanapi.com/verb/updated"))
  
  # see which users do not appear in the data and add forum and wiki wordcount of 0 so that all users that are assigned to a group 
  # are accounted for in the gini calculation
  
  # generate table for all groups and users with:
  # user_forum_wordcount = 0 and user_wiki_wordcount = 0
  # all_users <- unnest(groupsAndUsers)
  # colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
  
  all_users <- unnest(groupsAndUsers)
  if ("name" %in% names(all_users)) {
    colnames(all_users)[which(names(all_users) == "name")] <- "user_id"
  } else {
    colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
  }
  
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
    mutate(overall_wordcount = sum(user_forum_wordcount, user_wiki_wordcount))
  
  
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

calculateForumWordcountFun <- function(df, groupsAndUsers) {
  #browser()
  final_data <- NULL
  
  # get list of all users
  #groupsAndUsers <- groupsAndUsers
  
  # see which users do not appear in the data and add forum and wiki wordcount of 0 so that all users that are assigned to a group 
  # are accounted for in the gini calculation
  
  # generate table for all groups and users with:
  # user_forum_wordcount = 0 and user_wiki_wordcount = 0
  # all_users <- unnest(groupsAndUsers)
  # colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
  
  all_users <- unnest(groupsAndUsers)
  if ("name" %in% names(all_users)) {
    colnames(all_users)[which(names(all_users) == "name")] <- "user_id"
  } else {
    colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
  }
  
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

calculateWikiWordcountFun <- function(df, groupsAndUsers) {
  
  final_data <- NULL
  
  # get list of all users
  #groupsAndUsers <- groupsAndUsers
  
  # see which users do not appear in the data and add wiki wordcount of 0 so that all users that are assigned to a group 
  # are accounted for in the calculation
  
  # generate table for all groups and users with:
  # user_forum_wordcount = 0 and user_wiki_wordcount = 0
  
  # all_users <- unnest(groupsAndUsers)
  # if ("name" %in% names(all_users)) {
  #   
  #   all_users %<>% rename(name="user_id")
  # } else {
  #   
  #   all_users %<>% rename(fullname="user_id")
  # }
  
  all_users <- unnest(groupsAndUsers)
  if ("name" %in% names(all_users)) {
    colnames(all_users)[which(names(all_users) == "name")] <- "user_id"
  } else {
    colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
  }
  
  #browser()
  #colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
  
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

getAllLatenciesFun <- function(latencies, GroupsAndUsers) {
  
  all_users <- unnest(GroupsAndUsers)
  if ("name" %in% names(all_users)) {
    colnames(all_users)[which(names(all_users) == "name")] <- "user_id"
  } else {
    colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
  }
  
  # all_users <- unnest(GroupsAndUsers)
  # #all_users2 <- unnest(GroupsAndUsers)
  # 
  # if ("name" %in% names(all_users1)) {
  #   
  #   #all_users1 %<>% rename("name"="user_id")
  #   colnames(all_users1)[which(names(all_users1) == "name")] <- "user_id"
  #   
  # } else {
  #   
  #   #all_users1 %<>% rename("fullname"="user_id")
  #   colnames(all_users1)[which(names(all_users1) == "fullname")] <- "user_id"
  # }
  
  #colnames(all_users2)[which(names(all_users2) == "fullname")] <- "user_id"
  
  #browser()
  latencies <- latencies
  allGroups <- GroupsAndUsers %>% 
    select(c("group_id")) %>% 
    mutate(latency = 0)
  
  missing <- anti_join(allGroups, latencies, by = c("group_id"))
  complete_data <- full_join(missing, latencies)
  
  complete_data
}


buildGroupModel <- function(course, from, to, task, groups, average_latencies, work_imbalance, text_contribution_forum, text_contribution_wiki, group_sequences) {
  
  model <- list(
    model_metadata=list(
      course_id=course, 
      period_from=from,
      period_to=to,
      task_context=list(task),
      groups = groups
    ),
    
    # latency list with all groups, forum inative groups have a latency of 0
    average_latencies=average_latencies,
    work_imbalance = work_imbalance,
    text_contributions_forum = text_contribution_forum,
    text_contributions_wiki = text_contribution_wiki,
    # group TASK sequences containing relavant activities
    # USE THIS for current group model specification
    group_sequences=group_sequences
  )
  
  model
  
}

# clean html tags from forum posts
htmlTagClean <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
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
      print() %>% 
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
  
  #print(toJSON(model))
  
  while (fail) {
    
    tryCatch({
      
      res <- POST(xpsEndpoint, add_headers(Authorization = "Token b5b41fac0361d157d9673ecb926af5ae"), 
           body = toJSON(model, simplifyVector = T), content_type_json())
      fail <- FALSE
      print("Model Sent")
    },
    error = function(err) {
      
      print(err)
    })
  }
  res
}
