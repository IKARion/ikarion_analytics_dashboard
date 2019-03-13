source("config.R")
source("xps_utils.R")
source("data_utils.R")

# inputs:
# paste("course <- '", model$model_metadata$course_id, "'", sep=""),
# paste("pFrom <- '", model$model_metadata$period_from, "'", sep=""), # !!deleted
# paste("pTo <- '", model$model_metadata$period_to, "'", sep=""),     # !!deleted
# paste("taskId <- '", model$model_metadata$task_context$task_id, "'", sep=""))


# task sequences for course and task
groupTaskSequences <- getGroupTaskSequencesAll(course, taskId)

# latencies from course, do not include forum inactive groups
# groupLatencies <- getGroupLatencies2(groupTaskSequences, pFrom, pTo)

# selected task
task <- getTaskListForCourse(course) %>% filter(task_id == taskId)

# all groups and users for task
groupsAndUsers <- getGroupsAndUsersForCourse(course, taskId)

# include forum inactive groups in the latencies with latency = 0
# latencies <-getAllLatenciesFun(groupLatencies, groupsAndUsers)

work_imbalance <- calculateWorkImbalanceFun(groupTaskSequences, groupsAndUsers)

forum_sequences <- groupTaskSequences %>% filter(verb_id == "http://id.tincanapi.com/verb/replied")
wiki_sequences <- groupTaskSequences %>% filter(verb_id == "http://id.tincanapi.com/verb/updated")

#group_sequences <- groupTaskSequences %>% filter(verb_id == "http://id.tincanapi.com/verb/replied" | verb_id == "http://id.tincanapi.com/verb/updated") %>%  group_by(group_id) %>% do(sequence=select(., -c(group_id, content)))
group_sequences <- generateGroupTaskSequences( groupTaskSequences, groupsAndUsers, task)

forum_wordcount <- calculateForumWordcountFun(forum_sequences, groupsAndUsers)
wiki_wordcount <- calculateWikiWordcountFun(wiki_sequences, groupsAndUsers)

### !!! ###
# ADD SELFASSESSMENT HERE AS WELL

#weighted_forum_wordcount <- getGroupWeightedForumWordcountAll(course, taskId, as.numeric(Sys.time()))
#weighted_wiki_wordcount <- getGroupWeightedWikiWordcountAll(course, taskId, as.numeric(Sys.time()))


self_assessment <- getGroupSelfAssessmentsAll(course, taskId, as.numeric(Sys.time()), groupsAndUsers)

# add check if a relevant activitiy occured (dim(gTS))
# only then, calculate the values, otherwise call buildEmptyGroupModel with only task and group info

model <- NULL

# if (dim(groupTaskSequences[1] == 0)) {
#   model <- buildEmptyGroupModel(course,
#                                 task,
#                                 groupsAndUsers)
# } else { }
model <- buildGroupModel(course = course,
                         #from = pFrom,
                         #to = pTo,
                         task = task,
                         groups = groupsAndUsers,
                         #average_latencies = groupLatencies,
                         work_imbalance = work_imbalance,
                         
                         self_assessment = self_assessment,
                        
                         weighted_forum_wordcount = forum_wordcount,
                         weighted_wiki_wordcount = wiki_wordcount,
                         
                         #weighted_forum_wordcount = weighted_forum_wordcount,
                         #weighted_wiki_wordcount = weighted_wiki_wordcount,
                         
                         
                         group_sequences = group_sequences)


# model <- buildGroupModel(course = course,
#                 from = pFrom,
#                 to = pTo,
#                 task = task,
#                 groups = groupsAndUsers,
#                 #average_latencies = groupLatencies,
#                 work_imbalance = work_imbalance,
#                 text_contribution_forum = forum_wordcount,
#                 text_contribution_wiki = wiki_wordcount,
#                 group_sequences = group_sequences)

print(model)
  
model %>% sendModelToXPS
