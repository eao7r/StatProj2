library(tidyverse)
library(sjlabelled)

survey <- survey %>%
  mutate(across(!contains("THERM"), ~ replace(.x, .x==99, NA))) %>%
  mutate(across(contains("THERM"), ~ replace(.x, .x==999, NA))) %>%
  mutate(across(!contains("THERM"), to_factor))

survey <- survey[complete.cases(survey[, !grepl("SOCTRUST", names(survey))]),]