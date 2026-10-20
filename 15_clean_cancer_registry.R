############################################################
# FILE: 15_clean_cancer_registry.R
# PURPOSE: Clean and standardize longitudinal cancer
#          registry data for oncology RWE analyses
############################################################

############################################################
# LOAD SETUP
############################################################
source("scripts/00_setup.R")
getwd()
############################################################
# LIBRARIES
############################################################
library(dplyr)
library(lubridate)
library(stringr)

############################################################
# LOAD RAW DATA
############################################################
registry_raw <- read.csv(
  "data_raw/cancer_registry.csv",
  stringsAsFactors = FALSE
)

patients_clean <- read.csv(
  "data_clean/patients_clean.csv",
  stringsAsFactors = FALSE
)

############################################################
# STANDARDIZE COLUMN NAMES
############################################################
names(registry_raw) <- tolower(names(registry_raw))
names(patients_clean) <- tolower(names(patients_clean))

############################################################
# INITIAL DATA INSPECTION
############################################################
cat("====================================\n")
cat("RAW CANCER REGISTRY DATA OVERVIEW\n")
cat("====================================\n")

cat("Rows:", nrow(registry_raw), "\n")
cat("Columns:", ncol(registry_raw), "\n")
cat("Unique patient IDs:",
    length(unique(registry_raw$patient_id)),
    "\n")
#How many patients appear more than once in the registry?
cat("Patients with multiple records:",
    sum(table(registry_raw$patient_id) > 1),
    "\n")
str(registry_raw)

############################################################
# CREATE WORKING COPY
############################################################
registry_clean <- registry_raw

############################################################
# CONVERT DATE VARIABLES
############################################################
registry_clean$diagnosis_date <-
  as.Date(registry_clean$diagnosis_date)

registry_clean$death_date <-
  as.Date(registry_clean$death_date)

str(registry_clean)
############################################################
# REMOVE RECORDS WITH MISSING PATIENT IDs
############################################################
#Count how many patient IDs are missing and store the number
#cat means to show, display or print, How many rows are ABOUT TO be removed show that?

missing_id_n <- sum(is.na(registry_clean$patient_id))

cat("\nMissing patient IDs removed:",
    missing_id_n,
    "\n")

registry_clean <- registry_clean %>%
  filter(!is.na(patient_id))

sum(is.na(registry_clean$patient_id))

nrow(registry_raw)
nrow(registry_clean)


############################################################
# KEEP ONLY VALID LINKABLE PATIENTS
############################################################
#“From registry_clean, extract rows whose patient IDs are NOT 
#found in the clean patient dataset, and save those bad rows into a 
#separate audit object called registry_invalid_linkage_records.
registry_invalid_linkage_records <- registry_clean %>%
  filter(!patient_id %in% patients_clean$patient_id)

# PRE-CHECK COUNT
invalid_linkage_n <- nrow(registry_invalid_linkage_records)
print(invalid_linkage_n)
cat("Invalid registry linkage records identified:",
    invalid_linkage_n,
    "\n")
getwd()
# SAVE AUDIT FILE
write.csv(
  registry_invalid_linkage_records,
  "data_audit/registry_invalid_linkage_records.csv",
  row.names = FALSE
)

#Take the registry_clean dataset and keep only rows where its patient_id exists in patients_clean
registry_clean <- registry_clean %>%
  filter(patient_id %in% patients_clean$patient_id)

sum(!registry_clean$patient_id %in% patients_clean$patient_id)

cat("Rows after linkage validation:",
    nrow(registry_clean),
    "\n")

############################################################
# REMOVE EXACT DUPLICATE ROWS
############################################################
# Detect exact duplicate records
duplicate_records <- registry_clean %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

duplicate_records <- registry_clean %>%
  arrange(patient_id, diagnosis_date) %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

View(duplicate_records)
# Quantify duplicates
nrow(duplicate_records)

#Count how many unique patient IDs exist among the duplicate records
length(unique(duplicate_records$patient_id))

# Review duplicates
View(duplicate_records)

# Save audit dataset
getwd()
write.csv(
  duplicate_records,
  "data_audit/registry_exact_duplicates.csv",
  row.names = FALSE
)

before_dup <- nrow(registry_clean)

#Remove duplicates
registry_clean <- registry_clean %>%
  distinct()

after_dup <- nrow(registry_clean)

cat("Exact duplicate rows removed:",
    before_dup - after_dup,
    "\n")

############################################################
# STANDARDIZE CHARACTER VARIABLES
############################################################
registry_clean <- registry_clean %>%
  mutate(
    cancer_type = str_trim(cancer_type),
    cancer_type = str_to_title(cancer_type),
    
    stage = str_trim(stage),
    stage = str_to_upper(stage),
    
    province = str_trim(province),
    province = str_to_upper(province),
    
    registry_source = str_trim(registry_source)
  )

unique(registry_clean$cancer_type)
unique(registry_clean$stage)
unique(registry_clean$province)
unique(registry_clean$registry_source)

table(registry_clean$cancer_type)
############################################################
# HANDLE BLANK VALUES
############################################################
registry_clean$cancer_type[
  registry_clean$cancer_type == ""
] <- NA

registry_clean$stage[
  registry_clean$stage == ""
] <- NA

table(registry_clean$cancer_type, useNA = "ifany")
############################################################
# HARMONIZE CANCER TYPE SPELLING
############################################################
registry_clean$cancer_type <-
  case_when(
    
    registry_clean$cancer_type %in%
      c("Lungg") ~ "Lung",
    
    registry_clean$cancer_type %in%
      c("Breest") ~ "Breast",
    
    registry_clean$cancer_type %in%
      c("Colon") ~ "Colorectal",
    
    registry_clean$cancer_type %in%
      c("Prostrate") ~ "Prostate",
    
    registry_clean$cancer_type %in%
      c("Melanomia") ~ "Melanoma",
    
    TRUE ~ registry_clean$cancer_type
  )

############################################################
# VALIDATE STAGE VALUES
############################################################
#Only evaluate rows where stage is not missing 
#AND from valid stages specified, check stages are not valid

valid_stages <- c("I", "II", "III", "IV")

invalid_stage_n <- sum(
  !is.na(registry_clean$stage) &
    !registry_clean$stage %in% valid_stages
)

cat("Invalid stage values identified:",
    invalid_stage_n,
    "\n")

#Find stage values that are NOT in the list of valid stages,
#and replace them with missing values (NA).
registry_clean$stage[
  !registry_clean$stage %in% valid_stages
] <- NA

############################################################
# STANDARDIZE CONFIRMATION VARIABLE
############################################################
table(registry_clean$confirmed, useNA = "ifany")
unique(registry_clean$confirmed)
registry_clean <- registry_clean %>%
  mutate(
    confirmed = case_when(
      confirmed %in% c("Yes", "Y") ~ "Yes",
      confirmed %in% c("No", "N") ~ "No",
      TRUE ~ NA_character_
    )
  )
View(registry_clean)
############################################################
# REMOVE TEMPORALLY INVALID RECORDS
############################################################

# IDENTIFY TEMPORAL INCONSISTENCIES
# Count temporal inconsistencies
invalid_temporal_n <- sum(
  !is.na(registry_clean$death_date) &
    registry_clean$diagnosis_date >
    registry_clean$death_date
)


# PRE-CLEAN VALIDATION
# Show number of problematic records
cat("Temporal inconsistencies found:",
    invalid_temporal_n,
    "\n")

# Review problematic records
invalid_temporal_records <- registry_clean %>%
  filter(
    !is.na(death_date) &
      diagnosis_date > death_date
  ) %>%
  select(
    patient_id,
    diagnosis_date,
    death_date,
    cancer_type,
    stage
  )

# View problematic records
View(invalid_temporal_records)


# SAVE AUDIT FILE
getwd()
write.csv(
  invalid_temporal_records,
  "data_audit/registry_temporal_inconsistencies.csv",
  row.names = FALSE
)


# REMOVE TEMPORALLY INVALID RECORDS
before_temporal_clean <- nrow(registry_clean)

registry_clean <- registry_clean %>%
  filter(
    is.na(death_date) |
      diagnosis_date <= death_date
  )

after_temporal_clean <- nrow(registry_clean)

# POST-CLEAN VALIDATION
cat("Rows before temporal cleaning:",
    before_temporal_clean,
    "\n")

cat("Rows after temporal cleaning:",
    after_temporal_clean,
    "\n")

cat("Temporal inconsistencies removed:",
    before_temporal_clean -
      after_temporal_clean,
    "\n")

############################################################
# REMOVE DUPLICATE TUMOUR-STAGE RECORDS
############################################################

# Identify duplicate tumour-stage records
tumour_duplicate_records <- registry_clean %>%
  group_by(
    patient_id,
    diagnosis_date,
    cancer_type,
    stage
  ) %>%
  filter(n() > 1) %>%
  ungroup()

############################################################
# DUPLICATE SUMMARY
############################################################

# Total duplicate rows
nrow(tumour_duplicate_records)

# Number of duplicate tumour-stage groups
tumour_duplicate_records %>%
  group_by(
    patient_id,
    diagnosis_date,
    cancer_type,
    stage
  ) %>%
  summarise(
    records = n(),
    .groups = "drop"
  ) %>%
  nrow()

############################################################
# REVIEW DUPLICATE RECORDS
############################################################

tumour_duplicate_records %>%
  arrange(
    patient_id,
    diagnosis_date,
    cancer_type,
    stage
  ) %>%
  select(
    patient_id,
    diagnosis_date,
    cancer_type,
    stage,
    confirmed,
    death_date,
    registry_source,
    province
  ) %>%
  View()

############################################################
# PRIORITIZE MOST COMPLETE RECORDS
############################################################

registry_clean <- registry_clean %>%
  arrange(
    patient_id,
    diagnosis_date,
    cancer_type,
    stage,
    desc(confirmed == "Yes"),
    desc(!is.na(death_date))
  )

############################################################
# REVIEW PRIORITIZED RECORDS BEFORE REMOVAL
############################################################

registry_clean %>%
  group_by(
    patient_id,
    diagnosis_date,
    cancer_type,
    stage
  ) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  select(
    patient_id,
    diagnosis_date,
    cancer_type,
    stage,
    confirmed,
    death_date,
    registry_source,
    province
  ) %>%
  View()

############################################################
# SAVE AUDIT FILE
############################################################

write.csv(
  tumour_duplicate_records,
  "data_audit/registry_duplicate_tumour_stage_records.csv",
  row.names = FALSE
)

############################################################
# REMOVE DUPLICATE TUMOUR-STAGE RECORDS
############################################################

before_tumour_dedup <- nrow(registry_clean)

registry_clean <- registry_clean %>%
  distinct(
    patient_id,
    diagnosis_date,
    cancer_type,
    stage,
    .keep_all = TRUE
  )

after_tumour_dedup <- nrow(registry_clean)

############################################################
# POST-CLEAN VALIDATION
############################################################

cat("Rows before tumour-stage deduplication:",
    before_tumour_dedup,
    "\n")

cat("Rows after tumour-stage deduplication:",
    after_tumour_dedup,
    "\n")

cat("Duplicate tumour-stage rows removed:",
    before_tumour_dedup -
      after_tumour_dedup,
    "\n")

removed_tumour_duplicates <- tumour_duplicate_records %>%
  anti_join(
    registry_clean,
    by = c(
      "patient_id",
      "diagnosis_date",
      "cancer_type",
      "stage",
      "confirmed",
      "death_date",
      "registry_source",
      "province"
    )
  )

View(removed_tumour_duplicates)
nrow(removed_tumour_duplicates)


registry_clean %>%
  arrange(
    patient_id,
    cancer_type,
    diagnosis_date,
    stage
  ) %>%
  View()

registry_clean %>%
  group_by(
    patient_id,
    cancer_type
  ) %>%
  filter(n_distinct(stage) > 1) %>%
  arrange(
    patient_id,
    cancer_type,
    diagnosis_date,
    stage
  ) %>%
  select(
    patient_id,
    diagnosis_date,
    cancer_type,
    stage,
    confirmed,
    death_date,
    registry_source,
    province
  ) %>%
  View()
############################################################
# FIX: CREATE CONFIRMED FLAG (AFTER CLEANING)
############################################################

registry_clean <- registry_clean %>%
  mutate(
    confirmed_flag = case_when(
      confirmed == "Yes" ~ 1,
      confirmed == "No" ~ 0,
      TRUE ~ NA_real_
    )
  )

############################################################
# FINAL VALIDATION
############################################################

cat("\n====================================\n")
cat("CLEAN REGISTRY DATA SUMMARY\n")
cat("====================================\n")

cat("Final rows:",
    nrow(registry_clean),
    "\n")

cat("Unique patients:",
    length(unique(registry_clean$patient_id)),
    "\n")

cat("Colorectal cancer records:",
    sum(registry_clean$colorectal_flag == 1, na.rm = TRUE),
    "\n")

cat("Missing stage values:",
    sum(is.na(registry_clean$stage)),
    "\n")

cat("Confirmed cancers:",
    sum(registry_clean$confirmed_flag == 1, na.rm = TRUE),
    "\n")
############################################################
# SORT FINAL DATASET
############################################################
registry_clean <- registry_clean %>%
  arrange(patient_id, diagnosis_date)

############################################################
# FINAL VALIDATION
############################################################
cat("\n====================================\n")
cat("CLEAN REGISTRY DATA SUMMARY\n")
cat("====================================\n")

cat("Final rows:",
    nrow(registry_clean),
    "\n")

cat("Unique patients:",
    length(unique(registry_clean$patient_id)),
    "\n")

cat("Colorectal cancer records:",
    sum(registry_clean$colorectal_flag == 1,
        na.rm = TRUE),
    "\n")

cat("Missing stage values:",
    sum(is.na(registry_clean$stage)),
    "\n")

cat("Confirmed cancers:",
    sum(registry_clean$confirmed_flag == 1,
        na.rm = TRUE),
    "\n")

############################################################
# SAVE CLEAN DATASET
############################################################

getwd()

write.csv(
  registry_clean,
  "data_clean/cancer_registry_clean.csv",
  row.names = FALSE
)

cat("\nClean cancer registry dataset saved:\n")
cat("data_clean/cancer_registry_clean.csv\n")
saveRDS(registry_clean, "data_clean/cancer_registry_clean.rds")
############################################################
# END OF SCRIPT
############################################################
