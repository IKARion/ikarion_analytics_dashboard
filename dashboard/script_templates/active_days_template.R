source("xps_utils.R")
source("data_utils.R")

activeDays <- getActiveDaysAll(course) %>%
  mutate(activeDay=ymd(activeDay)) %>% 
  filter((activeDay >= pFrom) & (activeDay <= pEnd))

list(
  model_metadata=list(
    course_id=course, 
    period_from=pFrom,
    period_to=pTo
  ),
  activeDays=(activeDays %>% group_by(user) %>% do(activeDays=select(., activeDay)))
) %>% sendModelToXPS