library(aurum)
library(tidyverse)

cprdenvname <- "CPRD_depression_data"
yaml        <- ".aurum.yaml"

cprd     <- CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
analysis <- cprd$analysis("dh_augment")
cohort <- cohort %>% analysis$cached("cohort_interim_6")

#Get relevant codes
cohort <- cohort %>% select(patid, first_antidep_date)

consult_index <- cprd$tables$consultation %>%
  inner_join(cohort %>% select(patid, first_antidep_date), by = "patid") %>%
  mutate(difftime = datediff(consdate, first_antidep_date)) %>%
  filter(difftime >= -365) %>%
  distinct(patid, consdate) %>%
  analysis$cached("distinct_consults")

hes_index <- cprd$tables$hesDiagnosisEpi %>%
  inner_join(cohort %>% select(patid, first_antidep_date), by = "patid") %>%
  mutate(difftime = datediff(epistart, first_antidep_date)) %>%
  filter(difftime >= -365) %>%
  distinct(patid, epistart) %>%
  analysis$cached("distinct_hes_apc")


#Now create the variables
analysis <- cprd$analysis("dh_augment")
cons <- cons %>% analysis$cached("distinct_consults")
hes <- hes %>% analysis$cached("distinct_hes_apc")

#Create new vars:
# Number of appointments and APC in year prior to index
# Healthcare use will be a time-varying covariate in models
cons_sum <- cons %>% 
  inner_join(cohort %>% select(patid, first_antidep_date), by = "patid") %>%
  mutate(difftime = datediff(consdate, first_antidep_date)) %>%
  filter(difftime >= -365, difftime < 0) %>%
  group_by(patid) %>% 
  summarise(health_use_before_index = n()) %>% 
  ungroup() %>% analysis$cached("prior_consultations")

hes_sum <- hes %>% 
  inner_join(cohort %>% select(patid, first_antidep_date), by = "patid") %>%
  mutate(difftime = datediff(epistart, first_antidep_date)) %>%
  filter(difftime >= -365, difftime < 0) %>%
  group_by(patid) %>% 
  summarise(apc_count_before_index = n()) %>% 
  analysis$cached("prior_HES")
