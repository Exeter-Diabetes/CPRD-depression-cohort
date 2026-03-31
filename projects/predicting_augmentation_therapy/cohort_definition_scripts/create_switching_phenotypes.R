#Create a clean antidepressants table
library(ggplot2)
library(dplyr)
library(patchwork)
library(aurum)

cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"

cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)

#Antidepressant table
analysis <- cprd$analysis("all_patid")
ads <- ads %>% analysis$cached("clean_antidepressant_prodcodes")

#Cohort
analysis <- cprd$analysis("dh_augment")
cohort <- cohort %>% analysis$cached("cohort_interim_9")

#Takes a while to generate!
ads <- ads %>% 
  inner_join(cohort %>% select(patid, first_antidep_date), by = "patid") %>%
  filter(date >= first_antidep_date) %>%
  analysis$cached("clean_antidepressants")

analysis <- cprd$analysis("dh_augment")
cohort <- cohort %>% analysis$cached("cohort_interim_9") %>%
  filter(is.na(phq_score) | phq_score >= 10) %>%
  collect()

#Antidepressant table
ads_local <- ads %>% analysis$cached("clean_antidepressants") %>% 
  collect()


#Arrange and re-label
ads_local <- ads_local %>% arrange(patid, date)
ads_table <- ads_local %>% select(patid, date,  chem_name, drug_class) %>% 
  distinct(patid, date, chem_name, drug_class)


index_ad <- ads_table %>%
  group_by(patid) %>%
  slice_min(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(patid,
         first_rx_date = date,
         index_chem    = chem_name)

subsequent <- ads_table %>%
  inner_join(index_ad,  by = "patid") %>%
  inner_join(cohort %>% select(patid, first_antidep_date), by = "patid") %>%
  filter(date > first_antidep_date)

first_ssri_switch <- subsequent %>%
  filter(drug_class == "SSRI" & chem_name != index_chem) %>%
  group_by(patid) %>%
  slice_min(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(patid,
         first_ssri_switch_date = date,
         first_ssri_switch_name = chem_name)

first_class_switch <- subsequent %>%
  filter(drug_class != "SSRI") %>%
  group_by(patid) %>%
  slice_min(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(patid,
         first_class_switch_date  = date,
         first_class_switch_name  = chem_name,
         first_class_switch_class = drug_class)


ad_switches <- cohort %>% select(patid) %>% 
  left_join(index_ad,          by = "patid") %>%
  left_join(first_ssri_switch,  by = "patid") %>%
  left_join(first_class_switch, by = "patid")

ad_switches %>%
  summarise(
    n_total             = n(),
    n_any_rx            = sum(!is.na(first_rx_date)),
    n_ssri_switch       = sum(!is.na(first_ssri_switch_date)),
    n_class_switch      = sum(!is.na(first_class_switch_date)),
    pct_ssri_switch     = mean(!is.na(first_ssri_switch_date)) * 100,
    pct_class_switch    = mean(!is.na(first_class_switch_date)) * 100
  )

dbExecute(con2, "
CREATE TABLE dh_augment_switching (
  patid                      BIGINT,
  first_rx_date              DATE,
  index_chem                 VARCHAR(100),
  first_ssri_switch_date     DATE,
  first_ssri_switch_name     VARCHAR(100),
  first_class_switch_date    DATE,
  first_class_switch_name    VARCHAR(100),
  first_class_switch_class   VARCHAR(100)
);
")

# ── Chunk insert ──────────────────────────────────────────────────────────────
chunksize  <- 50000
total_rows <- nrow(ad_switches)
nchunks    <- ceiling(total_rows / chunksize)

for (i in seq_len(nchunks)) {
  idx   <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
  chunk <- ad_switches[idx, ]
  
  processed <- lapply(chunk, function(col) {
    if (is.character(col) || is.factor(col)) {
      paste0("'", gsub("'", "''", as.character(col)), "'")
    } else if (inherits(col, "Date")) {
      ifelse(is.na(col), "NULL", paste0("'", format(col, "%Y-%m-%d"), "'"))
    } else if (is.numeric(col)) {
      ifelse(is.na(col) | is.nan(col) | is.infinite(col),
             "NULL",
             format(col, scientific = FALSE))
    } else {
      rep("NULL", length(col))
    }
  })
  
  row_strings <- do.call(paste, c(processed, sep = ","))
  row_strings <- paste0("(", row_strings, ")")
  
  query <- paste0(
    "INSERT INTO dh_augment_switching (",
    paste(
      c(
        "patid",
        "first_rx_date",
        "index_chem",
        "first_ssri_switch_date",
        "first_ssri_switch_name",
        "first_class_switch_date",
        "first_class_switch_name",
        "first_class_switch_class"
      ),
      collapse = ", "
    ),
    ") VALUES ",
    paste(row_strings, collapse = ",")
  )
  
  dbExecute(con2, query)
  print(paste("Inserted chunk", i, "of", nchunks))
}
