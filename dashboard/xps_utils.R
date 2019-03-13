require(httr)
require(digest)
require(jsonlite)
require(ngram)
require(tidyr)
require(reldist)
require(schoolmath)
require(anytime)

source("dummy_data.R")

xpsEndpoint <- appConfig$xpsEndpoint
managementEndpoint <- appConfig$managementEndpoint

# clean content (html tags, punctation, ...)
htmlTagClean <- function(htmlString) {
  htmlString <- gsub("<.*?>", " ", htmlString)      # remove html tags
  htmlString <- gsub("&nbsp;", " ", htmlString)     # remove no-break spaces
  htmlString <- gsub("['\"]", " ", htmlString)      # remove single and double quotes
  htmlString <- gsub("[\\\\/]", " ", htmlString)    # remove slash und backslash
  htmlString <- gsub("[[:punct:]]"," ", htmlString) # remove punctation
  htmlString <- gsub("\\s+", " ", htmlString)       # remove redundant white spaces
  return(htmlString)
}

buildCustomScript <- function(model, scriptTemplate) {
  
  c(paste("setwd('", getwd(), "')", sep=""),
    paste("course <- '", model$model_metadata$course_id, "'", sep=""),
    paste("pFrom <- '", model$model_metadata$period_from, "'", sep=""),
    paste("pTo <- '", model$model_metadata$period_to, "'", sep=""),
    paste("taskId <- '", model$model_metadata$task_context[[1]]$task_id, "'", sep=""))  %>%
    append(readLines(scriptTemplate)) 
}

getGroupSelfAssessmentsAll <- function(courseId, taskId, timestamp, groupsAndUsers) {

  final_data <- NULL
  
  # get data
  courseId <- replaceUrlChars(courseId)
  
  # uncomment when endpoint is implemented  
  # getGroupListForTask(courseId, taskId) %>%
  #   rowwise %>%
  #   do(getGroupSelfAssessments(courseId, .$group, taskId, timestamp))
  
  
  # insert dummy data
  #data <- dummy_SA_T1
  data <- data.frame()
  #data <- dummy_EM3_T1
  
  
  # construct complete dataset for all users with 0 values
  
  all_users <- unnest(groupsAndUsers)
  if ("name" %in% names(all_users)) {
    colnames(all_users)[which(names(all_users) == "name")] <- "user_id"
  } else {
    colnames(all_users)[which(names(all_users) == "fullname")] <- "user_id"
  }
  
  all_users <- all_users %>%
    select(c(group_id, user_id)) %>% 
    mutate(item1 = 0) %>% 
    mutate(item2 = 0) %>% 
    mutate(item3 = 0)
  
  if(nrow(data) > 0) {
    
    # deselect timestamps
    data <- data %>%
      subset(select = -timestamp)

    missing <- anti_join(all_users, data, by = c("group_id", "user_id"))

    ## calculate new value for forum wordcount
    # defined as the share of words a users has written in comparison to all words written by all group members in the forum

    complete_data <- full_join(missing, data) 

    #browser()

    final_data <- complete_data %>%
      group_by(group_id) %>%
      do(group_members=select(., -c(group_id)))

    #browser()
    
  } else {
    
    # data with all inactive users
    final_data <- all_users %>% 
      group_by(group_id) %>% 
      do(group_members=select(., -group_id)) 
  }
  
  # join dataframe with real data and missing data
  
  
  
  # bring data in correct format: ("group_members", ...)
  # data <- dummy_SA_T1 %>% 
  #   group_by(group_id) %>% 
  #   do(group_members = select(., -c(group_id, timestamp)))
  
  
  
  final_data
}

getGroupWeightedForumWordcountAll <- function(courseId, taskId, timestamp, groupsAndUsers) {
  courseId <- replaceUrlChars(courseId)
  
  # uncomment when endpoint is implemented  
  # getGroupListForTask(courseId, taskId) %>%
  #   rowwise %>%
  #   do(getGroupWeightedForumWordcount(courseId, .$group, taskId, timestamp))
  
  # insert dummy data
  
  data <- dummy_WFW_T1 %>% 
    group_by(group_id) %>%
    do(group_members = select(., -c(group_id, timestamp)))
  
  # TODO add users with forum wordcount = 0 if backend does not give complete list with inactive users
  
  
  data
  
}

getGroupWeightedWikiWordcountAll <- function(courseId, taskId, timestamp, groupsAndUsers) {
  courseId <- replaceUrlChars(courseId)
  
  # uncomment when endpoint is implemented  
  # getGroupListForTask(courseId, taskId) %>%
  #   rowwise %>%
  #   do(getGroupWeightedWikiWordcount(courseId, .$group, taskId, timestamp))
  
  # insert dummy data
  data <- dummy_WWW_T1 %>% 
    group_by(group_id) %>%
    do(group_members = select(., -c(group_id, timestamp)))
  
  # TODO add users with wiki wordcount = 0 if backend does not give complete list with inactive users
  
  
  data
  
}

generateGroupTaskSequences <- function(sequences, groupsAndUsers, task) {
  
  data <- data.frame()
  #browser()
  if(dim(sequences)[1] > 0) {
    # classify activities
    
    ### TODO add classification again when fixed ###
    data <- classify_activities(sequences, task)
    # data <- sequences
    
    #browser()
    data <- data %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated") %>%  group_by(group_id) %>% do(sequence=select(., -c(group_id, content)))
    #browser()
  } else {
    # empty sequence
  }

  data
}

generateGroupTaskSequencesWithContent <- function(sequences, groupsAndUsers, task) {
  
  # classify activities
  ### TODO add classification again when fixed ###
  #data <- classify_activities(sequences, task)
  data <- sequences
  
  data <- data %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated")  %>%  group_by(group_id) %>% do(sequence=select(., -c(group_id)))
  data
}

# classify the activities based on the model that ws generated using the mercur mooc data
# possible classes: coordination, monitoring, major contribution, minor contribution, other
classify_activities <- function(data, task) {
  
  ### adding start activity to each group sequence ###
  
  start <- data[1,]
  start$content <- "start"
  start$object_id <- "start"
  start$object_name <- "start"
  start$object_type <- "start"
  start$timestamp <- task$task_start
  start$user_id <- "start"
  start$verb_id <- "start"
  
  sequences_with_start <- data.frame()
  
  # adding a start activity to one group sequence
  addStartActivity <- function(df, start) {
    start_activity <- start
    start_activity$group_id <- df[1,]$group_id
    
    group_sequence <- rbind(df, start_activity)
    
    sequences_with_start <<- rbind(sequences_with_start, group_sequence)
  }
  
  ### calculating features ###
  
  data %>% 
    group_by(group_id) %>% 
    do(addStartActivity(., start))
  
  # clean content
  sequences_with_start$content <- htmlTagClean(sequences_with_start$content)
  
  # order first -> last
  sequences_with_start <- sequences_with_start %>%
    group_by(group_id) %>%
    arrange(timestamp, .by_group = T)
  
  # calculate features
  # 1) wordcount   needs to be calculated for wiki edits
  # 2) type        done
  # 3) tool_change done
  # 4) idle_since  done
  # 5) position    done
  # 6) user_change done
  # 7) period      done
  
  # 8) fixed wordcount done
  
  # 1) add wordcount
  sequences_with_start <- sequences_with_start %>% 
    rowwise() %>% 
    mutate(wordcount = wordcount(content))
  
  # 2) add type
  sequences_with_start$type <- NA
  sequences_with_start$type[sequences_with_start$object_type == "http://collide.info/moodle_wiki_page"] <- "wiki"
  sequences_with_start$type[sequences_with_start$object_type == "http://id.tincanapi.com/activitytype/forum-topic"] <- "forum"
  sequences_with_start$type[sequences_with_start$object_type == "start"] <- "start"
  
  sequences_with_start <- sequences_with_start %>% 
    group_by(group_id) %>% 
    # 3) add tool change 
    mutate(tool_change = ifelse(type == lag(type), F, T)) %>%
    
    # 4) add idle since
    mutate(idle_since = as.numeric(timestamp) - lag(as.numeric(timestamp))) %>% 
    
    # 5) add position in sequence
    mutate(position = row_number()-1) %>% 
    
    # 6) add user change
    mutate(user_change = ifelse(user_id == lag(user_id), F, T)) %>% 
    
    # add day (for period calculation)
    mutate(day = anydate(as.numeric(timestamp))) # %>%
  
  
  # 7 ) add period: timeperiod that the activity appeared on (1-7) (one period covers 2 days)
  
  # task begin is the day of the start activity (which is added based on the task context)
  task_begin <- sequences_with_start[1,]$day
  
  sequences_with_start <- sequences_with_start %>% 
    group_by(group_id) %>% 
    # add period
    mutate(period = as.numeric(day - task_begin) + 1) %>%
    # fix period
    mutate(period = ifelse(is.odd(period), period+1, period)) %>% 
    mutate(period = period/2) # %>% 
  
  
  # 8) correct text change for wiki entries
  
  forum_and_start <- sequences_with_start %>% 
    filter(type %in% c("forum", "start"))
  
  wiki <- sequences_with_start %>% 
    filter(type == "wiki")
  
  # calculate wordcount as diff between wordcount of current and last contribution
  wiki <- wiki %>% 
    group_by(group_id) %>% 
    mutate(wordcount_fixed = wordcount - lag(wordcount))
  
  # fix negative wordcounts
  wiki$wordcount_fixed[wiki$wordcount_fixed < 0] <- 0
  # fix wordcounts for first contributions
  wiki$wordcount_fixed[is.na(wiki$wordcount_fixed)] <- wiki$wordcount[is.na(wiki$wordcount_fixed)]
  
  wiki$wordcount <- wiki$wordcount_fixed
  wiki <- wiki %>% 
    subset(select = -wordcount_fixed)
  
  forum_and_start$wordcount <- as.numeric(forum_and_start$wordcount)
  wiki$wordcount <- as.numeric(wiki$wordcount)
  
  #all_data <- rbind(forum, etherpad)
  all_data <- bind_rows(forum_and_start, wiki) %>% 
    group_by(group_id) %>% 
    dplyr::arrange(timestamp, .by_group = T)  
  
  all_data_without_start <- all_data %>%
    filter(!type == "start")
  
  ### classifying activities ###
  
  # load model
  classification_model <- readRDS("./model/rf_model.rds")
  
  all_data_without_start$type <- as.factor(all_data_without_start$type)
  
  predicted_classes <- predict(classification_model, all_data_without_start)
  all_data_without_start$class <- predicted_classes

  # delete features from dataset
  all_data_without_start <- all_data_without_start %>% 
    subset(select = -c(wordcount, type, tool_change, idle_since, position, user_change, day, period))
  
  all_data_without_start
}

calculateSelfAssessmentFun <- function(course, task, timestamp) {
  
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
    merge <- all_users
    # no activity at all
    # only possible if groups are logged on group creation without user activity
  }

  
  # add all inactive users to the data
  
  # missing users
  missing <- anti_join(all_users, merge, by = c("group_id", "user_id"))
  
  complete_data <- full_join(missing, merge) %>% 
    group_by(user_id) %>%
    # forum contribution weight:  1
    # wiki contibution  weight:   1
    mutate(overall_wordcount = sum(user_forum_wordcount, user_wiki_wordcount))
  
  
  
  gini_data <- complete_data %>% 
    group_by(group_id) %>% 
    summarise(gini_index = gini(overall_wordcount)*length(overall_wordcount)/(length(overall_wordcount)-1))
  
  # fix NaN values
  gini_data[is.nan(gini_data)] <- 0
  
  gini_data
  
}

## change function 
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
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
    
    
    ## calculate new value for forum wordcount
    # defined as the share of words a users has written in comparison to all words written by all group members in the forum
    
    complete_data <- full_join(missing, data) %>% 
      group_by(group_id) %>% 
      mutate(group_wordcount = sum(user_wordcount)) %>% 
      mutate(weighted_forum_wordcount = user_wordcount / group_wordcount)
    
    complete_data$weighted_forum_wordcount[is.nan(complete_data$weighted_forum_wordcount)] <- 0
    
    final_data <- complete_data %>% 
      group_by(group_id) %>% 
      do(group_members=select(., -c(group_id, user_wordcount, group_wordcount)))  
    
  } else {
    
    # data with all inactive users
    
    all_users <- all_users %>% 
      mutate(weighted_forum_wordcount = 0) %>% 
      subset(select = -user_wordcount)
    
    
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
    
    complete_data <- full_join(missing, sum_wordcounts) %>% 
      group_by(group_id) %>% 
      mutate(group_wordcount = sum(user_wordcount)) %>% 
      mutate(weighted_wiki_wordcount = user_wordcount / group_wordcount)
    
    complete_data$weighted_wiki_wordcount[is.nan(complete_data$weighted_wiki_wordcount)] <- 0
    
    # format according to model specification
    final_data <- complete_data %>% 
      group_by(group_id) %>% 
      do(group_members=select(., -c(group_id, user_wordcount, group_wordcount)))
    
  } else {
    
    # data with all inactive users
    
    all_users <- all_users %>% 
      mutate(weighted_wiki_wordcount = 0) %>% 
      subset(select = -user_wordcount)
    
    
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

  latencies <- latencies
  allGroups <- GroupsAndUsers %>%
    select(c("group_id")) %>%
    mutate(latency = 0)

  missing <- anti_join(allGroups, latencies, by = c("group_id"))
  complete_data <- full_join(missing, latencies)
  
  complete_data
}


#buildGroupModel <- function(course, from, to, task, groups, average_latencies, work_imbalance, text_contribution_forum, text_contribution_wiki, group_sequences) {
buildGroupModel <- function(course, 
                            #from, 
                            #to, 
                            task, 
                            groups, 
                            work_imbalance,
                            self_assessment, 
                            weighted_forum_wordcount, 
                            weighted_wiki_wordcount, 
                            #work_imbalance, 
                            #text_contribution_forum, 
                            #text_contribution_wiki, 
                            group_sequences) {  
  
  model <- list(
    model_metadata=list(
      course_id=course, 
      #period_from=from,
      #period_to=to,
      task_context=task,
      groups = groups
    ),
    
    work_imbalance = work_imbalance,
    
    # latency list with all groups, forum inative groups have a latency of 0
    #average_latencies=average_latencies,
    
    weighted_forum_wordcount = weighted_forum_wordcount, 
    weighted_wiki_wordcount = weighted_wiki_wordcount,
    self_assessment = self_assessment,
    
    #text_contributions_forum = text_contribution_forum,
    #text_contributions_wiki = text_contribution_wiki,
    # group TASK sequences containing relavant activities
    # USE THIS for current group model specification
    group_sequences=group_sequences
  )
  
  model
}

# send empty group model that only contains the task and groups before a relevant activity occured
# no longer used, since according to the specification "empty" models should also contain all fields (with values = 0)
buildEmptyGroupModel <- function(course, task, groups) {  
  model <- list(
    model_metadata=list(
      course_id=course, 
      task_context=list(task),
      groups = groups
    )
  )
  
  model
}

addScheduledTask <- function(model, interval, scriptTemplate, label) {

  sendModelToXPS(model)
  
  if (interval %in% c("minute", "10 minutes", "hour")) {
    frequencyMinutes <- 1
    if (interval == "10 minutes") {
      frequencyMinutes <- 10
    }
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
