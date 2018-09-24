source("config.R")
source("xps_utils.R")
source("data_utils.R")

groupSequences <- getGroupSequencesAll(course)

groupLatencies <- getGroupLatencies2(groupSequences, pFrom, pTo)

sequences=(groupSequences %>% group_by(group_id) %>% do(sequence=select(., -group_id)))

list(
  model_metadata=list(
    course_id=course, 
    period_from=pFrom,
    period_to=pTo
  ),
  latencies=groupLatencies,
  # add commit latencies to list
  sequences=(groupSequences %>% group_by(group_id) %>% do(sequence=select(., -group_id)))
) %>% sendModelToXPS