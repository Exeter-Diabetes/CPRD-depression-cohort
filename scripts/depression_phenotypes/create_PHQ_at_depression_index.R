library(ggplot2)
library(dplyr)
library(patchwork)
library(lubridate)
library(dbplyr)

#Connect
cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"

cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)


#Depression data
analysis <- cprd$analysis("all_patid")
phq <- phq %>% analysis$cached("clean_PHQ9_medcodes")


analysis <- cprd$analysis("dh_augment")
cohort <- cohort %>% analysis$cached("cohort_interim_8")


#Create merged table
phq_table <- phq %>% inner_join(cohort %>% select(patid, first_antidep_date), by = "patid") %>% 
  select(patid, medcodeid, obsdate, first_antidep_date, testvalue, PHQ9_cat)

#Hard-coded conversion table
category_lookup <- tibble::tribble(
  ~medcodeid,              ~category_score,
  "1839571000006114",      17,
  "1839561000006119",      12,
  "1839581000006112",      23.5,
  "1839551000006116",      7,
  "1839541000006118",      2
)




# Step 1: Find valid partial dates (exactly 9 distinct codes within date window)
valid_partial_dates <- phq_table %>%
  filter(PHQ9_cat == "partial") %>%
  filter(
    datediff(obsdate, first_antidep_date) >= -30,
    datediff(obsdate, first_antidep_date) <= 7
  ) %>%
  group_by(patid, obsdate) %>%
  summarise(n_codes = n_distinct(medcodeid), .groups = "drop") %>%
  filter(n_codes == 9) %>%
  select(patid, obsdate)

# Step 2: Sum valid partial codes into a single score per patient-date
partial_as_scores <- phq_table %>%
  filter(PHQ9_cat == "partial") %>%
  filter(
    datediff(obsdate, first_antidep_date) >= -30,
    datediff(obsdate, first_antidep_date) <= 7
  ) %>%
  semi_join(valid_partial_dates, by = c("patid", "obsdate")) %>%
  group_by(patid, obsdate, first_antidep_date) %>%
  summarise(testvalue = sum(testvalue, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    PHQ9_cat  = "score",
    medcodeid = NA
  )

# Step 3: Clean main table — remove NAs, apply date window, drop all partials,
# replace category testvalues with hardcoded scores, add back partial scores
phq_table_clean <- phq_table %>%
  filter(
    !(PHQ9_cat %in% c("score", "partial") & is.na(testvalue))
  ) %>%
  filter(
    datediff(obsdate, first_antidep_date) >= -30,
    datediff(obsdate, first_antidep_date) <= 7
  ) %>%
  filter(PHQ9_cat != "partial") %>%
  left_join(category_lookup, by = "medcodeid", copy = T) %>%
  mutate(
    testvalue = case_when(
      PHQ9_cat == "category" ~ category_score,
      TRUE                   ~ testvalue
    )
  ) %>%
  select(-category_score) %>%
  union_all(partial_as_scores)

# Step 4: Select score closest to index date, preferring score > partial > category
phq_final <- phq_table_clean %>%
  mutate(
    abs_days      = abs(datediff(obsdate, first_antidep_date)),
    type_priority = case_when(
      PHQ9_cat == "score"    ~ 1L,
      PHQ9_cat == "partial"  ~ 2L,
      PHQ9_cat == "category" ~ 3L
    )
  ) %>%
  group_by(patid) %>%
  window_order(abs_days, type_priority) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  filter(row_num == 1) %>%
  filter(testvalue >= 0, testvalue <= 27) %>%
  select(patid, phq_score = testvalue) %>% 
  analysis$cached("phq_scores_at_index")
