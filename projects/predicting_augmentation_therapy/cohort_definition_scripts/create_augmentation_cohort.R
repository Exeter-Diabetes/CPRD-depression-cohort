  library(aurum)
  library(tidyverse)
  
  cprdenvname <- "CPRD_depression_data"
  yaml <- ".aurum.yaml"
  
  con2 <- dbConnect(MariaDB())
  
  
  
  cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
  analysis <- cprd$analysis("dh")
  
  #Load in the cohort table
  cohort <- cohort %>% 
    analysis$cached("depression_cohort_interim_10")
  
  first_ad <- first_ad %>% analysis$cached("clean_first_antidepressant")
    
    
    #Describe the cohort
    cohort %>% count()
  
  cohort %>% filter(valid_practice == 1) %>% count() #3,539,388
  
  cohort %>% filter(valid_practice == 1) %>% filter(valid_gender == 1) %>% 
    count() 
  
  #Any QOF depression - Listed in GitHub index date tree
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% count() 
  
  #with clear date
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% count() 
  
  
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% count() #1,804,428
  
  cohort %>%  filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
    filter(depression_diag_under18yo == 0) %>% count() 
  
  
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
    filter(depression_diag_under18yo == 0)  %>% filter(diagnosed_after_QOF == 1) %>% count() #1,374,994
  
  
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
    filter(depression_diag_under18yo == 0)  %>% filter(diagnosed_after_QOF == 1) %>%
    filter(with_hes == 1) %>% count() 
  
  #Create our cohort
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
    filter(depression_diag_under18yo == 0)  %>% filter(diagnosed_after_QOF == 1) %>%
    filter(with_hes == 1) %>% filter(!is.na(ethnicity_5cat))
  
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
    filter(depression_diag_under18yo == 0)  %>% filter(diagnosed_after_QOF == 1) %>%
    filter(with_hes == 1) %>% filter(!is.na(ethnicity_5cat)) %>% 
    filter(reg_183days_ad_code_index == 1) %>% count() #912,424
  
  
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
    filter(depression_diag_under18yo == 0)  %>% filter(diagnosed_after_QOF == 1) %>%
    filter(with_hes == 1) %>% filter(!is.na(ethnicity_5cat)) %>% 
    filter(reg_183days_ad_code_index == 1) %>% filter(ad_code_index_date >= as.Date("2006-04-01")) %>%
    count() 
  
  
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
    filter(depression_diag_under18yo == 0)  %>% filter(diagnosed_after_QOF == 1) %>%
    filter(with_hes == 1) %>% filter(!is.na(ethnicity_5cat)) %>% 
    filter(reg_183days_ad_code_index == 1) %>% filter(ad_code_index_date >= as.Date("2006-04-01")) %>%
    filter(is.na(first_antidep_not_ssri) | first_antidep_not_ssri == 0) %>% 
    count() 
  
  cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
    filter(depression_diag_under18yo == 0)  %>% filter(diagnosed_after_QOF == 1) %>%
    filter(with_hes == 1) %>% filter(!is.na(ethnicity_5cat)) %>% 
    filter(reg_183days_ad_code_index == 1) %>% filter(ad_code_index_date >= as.Date("2006-04-01")) %>%
    filter(is.na(first_antidep_not_ssri) | first_antidep_not_ssri == 0) %>% 
    filter(is.na(multiple_ssri_same_date) | multiple_ssri_same_date == 0) %>%
    count() 
   
    
  cohort %>% filter(valid_practice == 1) %>% 
      filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
      filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
      filter(depression_diag_under18yo == 0)  %>% filter(diagnosed_after_QOF == 1) %>%
      filter(with_hes == 1) %>% filter(!is.na(ethnicity_5cat)) %>% 
      filter(reg_183days_ad_code_index == 1) %>% filter(ad_code_index_date >= as.Date("2006-04-01")) %>%
      filter(is.na(first_antidep_not_ssri) | first_antidep_not_ssri == 0) %>% 
      filter(is.na(multiple_ssri_same_date) | multiple_ssri_same_date == 0) %>%
      filter(age_at_ad_code_index >= 18) %>% 
      count() 
  
  # Next, we add first antidepressant to the table
  analysis <- cprd$analysis("dh_augment")
  cohort_interim_1 <- cohort %>% filter(valid_practice == 1) %>% 
    filter(valid_gender == 1) %>% filter(qof_depression == 1) %>% 
    filter(preexisting == 0) %>% filter(registered_current_gp_90 == 1) %>% 
    filter(depression_diag_under18yo == 0)  %>% filter(diagnosed_after_QOF == 1) %>%
    filter(with_hes == 1) %>% filter(!is.na(ethnicity_5cat)) %>% 
    filter(reg_183days_ad_code_index == 1) %>% 
    filter(ad_code_index_date >= as.Date("2006-04-01")) %>%
    filter(is.na(first_antidep_not_ssri) | first_antidep_not_ssri == 0) %>% 
    filter(is.na(multiple_ssri_same_date) | multiple_ssri_same_date == 0) %>%
    filter(age_at_ad_code_index >= 18) %>% analysis$cached("cohort_interim_1")
    
  
  #Join first antidepressant
  analysis <- cprd$analysis("all_patid")
  first_ad <- first_ad %>% analysis$cached("clean_first_antidepressant")
  first_ad <- first_ad %>% select(patid, first_antidepressant = chem_name) %>% 
    distinct(patid, first_antidepressant)
  
  analysis <- cprd$analysis("dh_augment")
  cohort_interim_2 <- cohort_interim_1 %>% left_join(first_ad, by = "patid") %>%
    analysis$cached("cohort_interim_2")
  
  #Filter out people who don't ever try antidepressants
  cohort_interim_3 <- cohort_interim_2 %>% 
    filter(!is.na(first_antidepressant)) %>% 
    analysis$cached("cohort_interim_3")
  
  #Next, we add in the SMI diagnoses
  analysis <- cprd$analysis("dh")
  smi <- smi %>% analysis$cached("cohort_smi_classified")
  smi_short <- smi %>% 
    select(patid, schizophrenia_status_first_antidep_date, 
           bipolar_status_first_antidep_date, 
           other_psychosis_status_first_antidep_date)
  
  cohort_interim_3 %>% left_join(smi_short, by = "patid") %>% 
    filter(!schizophrenia_status_first_antidep_date == "exclude") %>%
    filter(!bipolar_status_first_antidep_date == "exclude") %>%
    filter(!other_psychosis_status_first_antidep_date == "exclude") %>% count()
  
  
  cohort_interim_3 %>% left_join(smi_short, by = "patid") %>% 
    filter(!schizophrenia_status_first_antidep_date == "exclude") %>%
    filter(!bipolar_status_first_antidep_date == "exclude") %>%
    filter(!other_psychosis_status_first_antidep_date == "exclude") %>%
    filter(!schizophrenia_status_first_antidep_date == "pre_existing") %>%
    filter(!bipolar_status_first_antidep_date == "pre_existing") %>%
    filter(!other_psychosis_status_first_antidep_date == "pre_existing") %>%
    count() 
  
  analysis <- cprd$analysis("dh_augment")
  cohort_interim_4 <- cohort_interim_3 %>%
    left_join(smi_short, by = "patid") %>% 
    filter(!schizophrenia_status_first_antidep_date == "exclude") %>%
    filter(!bipolar_status_first_antidep_date == "exclude") %>%
    filter(!other_psychosis_status_first_antidep_date == "exclude") %>%
    filter(!schizophrenia_status_first_antidep_date == "pre_existing") %>%
    filter(!bipolar_status_first_antidep_date == "pre_existing") %>%
    filter(!other_psychosis_status_first_antidep_date == "pre_existing") %>%
    analysis$cached("cohort_interim_4")
  
  analysis <- cprd$analysis("all_patid")
  epilepsy <- epilepst %>% analysis$cached("first_epilepsy_date")
  
  epi_short <- epilepsy %>%
    select(patid, date) %>%
    rename(first_epilepsy_date = date) %>%
    distinct()
  
  #Remove epilepsy cases
  cohort_interim_5 <- cohort_interim_4 %>%
    left_join(epi_short, by = "patid") %>%
    mutate(
      epilepsy_status = case_when(
        is.na(first_epilepsy_date)                              ~ "control",
        first_epilepsy_date < first_antidep_date                ~ "pre_existing",
        first_epilepsy_date >= first_antidep_date               ~ "incident",
        TRUE                                                    ~ NA_character_
      )
    ) %>%
    filter(!epilepsy_status == "pre_existing") %>% 
    analysis$cached("cohort_interim_5")
  
  
  analysis <- cprd$analysis("all_patid")
  ap <- ap %>% analysis$cached("clean_antipsychotics_prodcodes")
  
  
  #Add in augmentation
      exclude_drugs <- c("prochlorperazine", "promazine", "levomepromazine", 
                         "flupentixol", "amitriptyline")
      
      augment_drugs <- c("quetiapine", "risperidone", "olanzapine", "aripiprazole")
  
  
  ap_post <- ap %>%
    inner_join(cohort_interim_5 %>% 
                 select(patid, first_antidep_date), by = "patid") %>%
        collect()
  
  ap_post <- ap_post %>%
    mutate(
      chem_name = antipsychotics_cat %>%
        str_trim() %>%
        str_to_lower() %>%
        str_remove("\\s.*$")
    ) %>%
    select(-antipsychotics_cat) %>%
    filter(!chem_name %in% exclude_drugs) %>%
    arrange(patid, issuedate) %>%
    
    # ---- Flag any antipsychotic before or on index date
    group_by(patid) %>%
    mutate(any_ap_before_index = any(issuedate <= first_antidep_date)) %>%
    
    # ---- First post-index antipsychotic only
    filter(issuedate > first_antidep_date) %>%
    slice_min(order_by = issuedate, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    
    # ---- Classify
    mutate(
      augmentation_status = case_when(
        any_ap_before_index          ~ "exclude",
        chem_name %in% augment_drugs ~ "case",
        TRUE                         ~ "censored"
      ),
      augmentation_date = issuedate
    ) %>%
    select(patid, any_ap_before_index, augmentation_status, augmentation_date)
  
  
  # Upload
  dbExecute(con2, "
  CREATE TABLE dh_augment_first_antipsychotic (
    patid               BIGINT,
    any_ap_before_index TINYINT(1),
    augmentation_status VARCHAR(20),
    augmentation_date   DATE
  );
  ")
  
  chunksize  <- 50000
  total_rows <- nrow(ap_post)
  nchunks    <- ceiling(total_rows / chunksize)
  
  for (i in seq_len(nchunks)) {
    
    idx   <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
    chunk <- ap_post[idx, ]
    
    processed <- lapply(chunk, function(col) {
      if (is.character(col) || is.factor(col)) {
        paste0("'", gsub("'", "''", as.character(col)), "'")
      } else if (inherits(col, "Date")) {
        ifelse(is.na(col), "NULL", paste0("'", format(col, "%Y-%m-%d"), "'"))
      } else if (is.logical(col)) {
        ifelse(is.na(col), "NULL", as.integer(col))
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
      "INSERT INTO dh_augment_first_antipsychotic (",
      paste(colnames(ap_post), collapse = ", "),
      ") VALUES ",
      paste(row_strings, collapse = ",")
    )
    
    dbExecute(con2, query)
    print(paste("Inserted chunk", i, "of", nchunks))
  }
  
#Now table is here, let's merge tables together:
analysis <- cprd$analysis("dh_augment")
cohort_interim_5 <- cohort_interim_5 %>% analysis$cached("cohort_interim_5")
ap <- ap %>% analysis$cached("first_antipsychotic")
  
cohort_interim_5 %>% left_join(ap, by = "patid") %>%
    mutate(
      any_ap_before_index = coalesce(any_ap_before_index, FALSE),
      augmentation_status = coalesce(augmentation_status, "control")
    ) %>% filter(!augmentation_status == "exclude") %>% count()
  
  
#Final cohort
cohort_interim_6 <- cohort_interim_5 %>% left_join(ap, by = "patid") %>%
    mutate(
      any_ap_before_index = coalesce(any_ap_before_index, FALSE),
      augmentation_status = coalesce(augmentation_status, "control")
    ) %>% filter(!augmentation_status == "exclude") %>% analysis$cached("cohort_interim_6")
  
  #Add in healthcare use variables at baseline, and also BMI.
  analysis <- cprd$analysis("dh_augment")
  
cohort_interim_6 <- cohort_interim_6 %>% analysis$cached("cohort_interim_6")
consult <- consult %>% analysis$cached("prior_consultations")
hes <- hes %>% analysis$cached("prior_HES")
  
cohort_interim_7 <- cohort_interim_6 %>%
    left_join(consult, by = "patid") %>%
    left_join(hes, by = "patid") %>%
    mutate(
      health_use_before_index = coalesce(health_use_before_index, 0),
      apc_count_before_index = coalesce(apc_count_before_index, 0)
    ) %>% analysis$cached("cohort_interim_7")
  
  #Next, load in BMI
analysis <- cprd$analysis("all_patid")
bmi <- bmi %>% analysis$cached("clean_bmi_medcodes")
  
analysis <- cprd$analysis("dh_augment")
  
bmi_short <- bmi %>%
    inner_join(cohort %>% select(patid, first_antidep_date), by = "patid") %>%
    mutate(difftime = datediff(date, first_antidep_date)) %>%
    filter(difftime <= 0, difftime > -730) %>%
    mutate(abs_diff = abs(difftime)) %>%
    group_by(patid) %>%
    slice_min(abs_diff, n = 1, with_ties = FALSE) %>%
    select(patid, bmi = testvalue) %>% 
    mutate(obesity = ifelse(bmi >= 30.0, 1, 0))
  
cohort_interim_8 <- cohort_interim_7 %>%
    left_join(bmi_short, by = "patid") %>%
    analysis$cached("cohort_interim_8")

#Load in the PHQ-9
phq <- phq %>% analysis$cached("phq_scores_at_index")
phq <- phq %>% mutate(
  phq_cat = 
    case_when(
    phq_score < 10 ~ "not valid", 
    phq_score >= 10 & phq_score <= 14 ~ "Moderate",
    phq_score >= 15 & phq_score <= 19 ~ "Moderate-severe",
    phq_score >= 20 & phq_score <= 27 ~ "Severe"
  )
)

cohort_interim_9 <- cohort_interim_8 %>% 
  left_join(phq %>% select(patid, phq_score, phq_cat), by = "patid") %>%
  analysis$cached("cohort_interim_9")

