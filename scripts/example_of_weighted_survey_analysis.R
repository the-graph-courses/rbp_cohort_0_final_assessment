# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, srvyr, haven)

ng_raw <- haven::read_dta(here("data/NGIR7BFL.DTA"))

ng_design <- 
  ng_raw %>% 
  as_survey_design(id = v001, strata = v022, weights = v005, nest = TRUE) 


## weighted mean
ng_design %>%
  mutate(v613 = as.numeric(as.character(v613))) %>% 
  filter(v613 < 51) %>% 
  group_by(v013) %>% 
  summarise(mean_desired_num_kids = survey_mean(v613))

# checks out: https://imgur.com/a/K7IilAG
ng_raw %>%
  mutate(v613 = as.numeric(as.character(v613))) %>% 
  filter(v613 < 51) %>% 
  group_by(v013) %>% 
  summarise(mean_desired_num_kids = mean(v613))

