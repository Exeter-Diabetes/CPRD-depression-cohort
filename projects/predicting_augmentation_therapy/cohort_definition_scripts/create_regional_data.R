library(ggplot2)
library(dplyr)
library(patchwork)
library(aurum)


#Find current practice
cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"

cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
analysis <- cprd$analysis("dh_augment")
cohort <- cohort %>% analysis$cached("cohort_interim_9")

pracids <- cprd$tables$patient %>% 
  inner_join(cohort %>% select(patid), by = "patid") %>%
  select(patid, pracid) %>% analysis$cached("pracids")

#Join this file with practice file:
practice <- pracids %>% inner_join(cprd$tables$practice, by = "pracid") %>%
  inner_join(cprd$tables$region, by = c("region" = "regionid")) %>%
  analysis$cached("regional_data")
