############################################################
# FILE: 16_clean_hospital_admissions.R
# PURPOSE: Clean and standardize longitudinal hospital
#          admissions data for oncology RWE analyses
############################################################

############################################################
# LOAD SETUP
############################################################
source("scripts/00_setup.R")

############################################################
# LIBRARIES
############################################################
library(dplyr)
library(lubridate)
library(stringr)

############################################################
# LOAD RAW DATA
############################################################
adm_raw <- read.csv(
  "data_raw/hospital_admissions.csv",
  stringsAsFactors = FALSE
)

patients_clean <- read.csv(
  "data_clean/patients_clean.csv",
  stringsAsFactors = FALSE
)

############################################################
# STANDARDIZE COLUMN NAMES
############################################################
View(adm_raw)
View(patients_clean)
names(adm_raw) <- tolower(names(adm_raw))
names(patients_clean) <- tolower(names(patients_clean))

############################################################
# INITIAL DATA INSPECTION
############################################################
cat("====================================\n")
cat("RAW HOSPITAL ADMISSIONS OVERVIEW\n")
cat("====================================\n")

cat("Rows:", nrow(adm_raw), "\n")

cat("Columns:", ncol(adm_raw), "\n")

cat("Unique patient IDs:",
    length(unique(adm_raw$patient_id)),
    "\n")

cat("Patients with multiple admissions:",
    sum(table(adm_raw$patient_id) > 1,
        na.rm = TRUE),
    "\n")

str(adm_raw)

############################################################
# CREATE WORKING COPY
############################################################
adm_clean <- adm_raw

############################################################
# CONVERT DATE VARIABLES
############################################################
adm_clean$admit_date <-
  as.Date(adm_clean$admit_date)

adm_clean$discharge_date <-
  as.Date(adm_clean$discharge_date)

adm_clean$enrollment_start <-
  as.Date(adm_clean$enrollment_start)

adm_clean$enrollment_end <-
  as.Date(adm_clean$enrollment_end)

adm_clean$death_date <-
  as.Date(adm_clean$death_date)

str(adm_clean)
############################################################
# REMOVE RECORDS WITH MISSING PATIENT IDs
############################################################

missing_id_records <- adm_clean %>%
  filter(is.na(patient_id))

missing_id_n <- nrow(missing_id_records)

cat("\nMissing patient ID records identified:",
    missing_id_n,
    "\n")
getwd()
write.csv(
  missing_id_records,
  "data_audit/admissions_missing_patient_ids.csv",
  row.names = FALSE
)

before_missing_id <- nrow(adm_clean)
cat("Current rows before cleaning:",
    before_missing_id,
    "\n")

adm_clean <- adm_clean %>%
  filter(!is.na(patient_id))

after_missing_id <- nrow(adm_clean)

cat("Current rows after cleaning:",
    after_missing_id,
    "\n")


cat("Rows after removing missing IDs:",
    after_missing_id,
    "\n")

cat("Rows removed:",
    before_missing_id - after_missing_id,
    "\n")

############################################################
# KEEP ONLY VALID LINKABLE PATIENTS
############################################################

#Take the dataset adm_clean,then keep only rows whose patient_id does NOT exist inside
#patients_clean$patient_id, and save those bad rows into a new object called invalid_linkage_records
invalid_linkage_records <- adm_clean %>%
  filter(!patient_id %in%
           patients_clean$patient_id)

invalid_linkage_n <- nrow(
  invalid_linkage_records
)

cat("\nInvalid linkage records identified:",
    invalid_linkage_n,
    "\n")

length(unique(
  invalid_linkage_records$patient_id
))
write.csv(
  invalid_linkage_records,
  "data_audit/admissions_invalid_linkage_records.csv",
  row.names = FALSE
)

before_linkage <- nrow(adm_clean)

#Take the current admissions dataset (adm_clean), then keep only rows where the admission patient ID
#exists inside the clean patient registry IDs,then save the filtered result back into adm_clean
adm_clean <- adm_clean %>%
  filter(patient_id %in%
           patients_clean$patient_id)

after_linkage <- nrow(adm_clean)

cat("Rows before linkage validation:",
    before_linkage,
    "\n")

cat("Rows after linkage validation:",
    after_linkage,
    "\n")

cat("Rows removed:",
    before_linkage - after_linkage,
    "\n")

############################################################
# REMOVE EXACT DUPLICATE ROWS
############################################################

duplicate_records <- adm_clean %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

duplicate_n <- nrow(duplicate_records)

cat("\nExact duplicate rows identified:",
    duplicate_n,
    "\n")

write.csv(
  duplicate_records,
  "data_audit/admissions_exact_duplicates.csv",
  row.names = FALSE
)

before_dup <- nrow(adm_clean)

adm_clean <- adm_clean %>%
  distinct()

after_dup <- nrow(adm_clean)

cat("Rows before duplicate removal:",
    before_dup,
    "\n")

cat("Rows after duplicate removal:",
    after_dup,
    "\n")

cat("Duplicate rows removed:",
    before_dup - after_dup,
    "\n")

############################################################
# STANDARDIZE CHARACTER VARIABLES
############################################################
View(adm_clean)
unique(adm_clean$hospital_id)
unique(adm_clean$most_responsible_dx)
unique(adm_clean$procedure_code)

adm_clean <- adm_clean %>%
  mutate(
    
    hospital_id =
      str_trim(hospital_id),
    
    hospital_id =
      str_to_upper(hospital_id),
    
    most_responsible_dx =
      str_trim(most_responsible_dx),
    
    most_responsible_dx =
      str_to_upper(most_responsible_dx),
    
    procedure_code =
      str_trim(procedure_code),
    
    procedure_code =
      str_to_upper(procedure_code)
    
  )

############################################################
# HANDLE BLANK VALUES
############################################################

adm_clean$most_responsible_dx[
  adm_clean$most_responsible_dx == ""
] <- NA

adm_clean$procedure_code[
  adm_clean$procedure_code == ""
] <- NA

############################################################
# FIX COMMON DIAGNOSIS CODE ISSUES
############################################################

adm_clean$most_responsible_dx <-
  case_when(
    
    adm_clean$most_responsible_dx == "I2L"
    ~ "I21",
    
    TRUE ~
      adm_clean$most_responsible_dx
  )
unique(adm_clean$most_responsible_dx)
############################################################
# VALIDATE DIAGNOSIS CODE FORMAT
############################################################


invalid_dx_records <- adm_clean %>%
  filter(
    !is.na(most_responsible_dx) &
      !str_detect(
        most_responsible_dx,
        "^[A-Z][0-9]{2}$"
      )
  )

invalid_dx_n <- nrow(invalid_dx_records)

cat("\nInvalid diagnosis codes identified:",
    invalid_dx_n,
    "\n")

write.csv(
  invalid_dx_records,
  "data_audit/admissions_invalid_dx_codes.csv",
  row.names = FALSE
)

############################################################
# VALIDATE PROCEDURE CODE FORMAT
############################################################
unique(adm_clean$procedure_code)
table(adm_clean$procedure_code,
      useNA = "ifany")

invalid_proc_records <- adm_clean %>%
  filter(
    !is.na(procedure_code) &
      !str_detect(
        procedure_code,
        "^[0-9][A-Z]{2}[0-9]{2}$"
      )
  )

invalid_proc_n <- nrow(
  invalid_proc_records
)

cat("Invalid procedure codes identified:",
    invalid_proc_n,
    "\n")

invalid_proc_records %>%
  select(patient_id, procedure_code)


write.csv(
  invalid_proc_records,
  "data_audit/admissions_invalid_procedure_codes.csv",
  row.names = FALSE
)

#For every non-missing procedure code that does NOT follow the valid format, replace it with NA
adm_clean$procedure_code[
  !is.na(adm_clean$procedure_code) &
    !str_detect(
      adm_clean$procedure_code,
      "^[0-9][A-Z]{2}[0-9]{2}$"
    )
] <- NA

table(adm_clean$procedure_code,
      useNA = "ifany")


############################################################
# IDENTIFY NEGATIVE LENGTH OF STAY
############################################################

negative_los_records <- adm_clean %>%
  filter(discharge_date < admit_date)

negative_los_n <- nrow(
  negative_los_records
)

cat("\nNegative LOS records identified:",
    negative_los_n,
    "\n")

write.csv(
  negative_los_records,
  "data_audit/admissions_negative_los.csv",
  row.names = FALSE
)

############################################################
# REMOVE NEGATIVE LENGTH OF STAY RECORDS
############################################################

before_los <- nrow(adm_clean)

adm_clean <- adm_clean %>%
  filter(discharge_date >= admit_date)

after_los <- nrow(adm_clean)

cat("Rows before LOS cleaning:",
    before_los,
    "\n")

cat("Rows after LOS cleaning:",
    after_los,
    "\n")

cat("Rows removed:",
    before_los - after_los,
    "\n")

############################################################
# IDENTIFY ADMISSIONS AFTER DEATH
############################################################

after_death_records <- adm_clean %>%
  filter(
    !is.na(death_date) &
      admit_date > death_date
  )

after_death_n <- nrow(after_death_records)

cat("\nAdmissions after death identified:",
    after_death_n,
    "\n")

write.csv(
  after_death_records,
  "data_audit/admissions_after_death.csv",
  row.names = FALSE
)

############################################################
# REMOVE ADMISSIONS AFTER DEATH
############################################################

before_death_clean <- nrow(adm_clean)

adm_clean <- adm_clean %>%
  filter(
    is.na(death_date) |
      admit_date <= death_date
  )

after_death_clean <- nrow(adm_clean)

cat("Rows before death-date cleaning:",
    before_death_clean,
    "\n")

cat("Rows after death-date cleaning:",
    after_death_clean,
    "\n")

cat("Rows removed:",
    before_death_clean -
      after_death_clean,
    "\n")

############################################################
# IDENTIFY ADMISSIONS OUTSIDE ENROLLMENT
############################################################

outside_enrollment_records <- adm_clean %>%
  filter(
    admit_date < enrollment_start |
      admit_date > enrollment_end
  )

outside_enrollment_n <- nrow(
  outside_enrollment_records
)

cat("\nAdmissions outside enrollment identified:",
    outside_enrollment_n,
    "\n")

write.csv(
  outside_enrollment_records,
  "data_audit/admissions_outside_enrollment.csv",
  row.names = FALSE
)

############################################################
# REMOVE ADMISSIONS OUTSIDE ENROLLMENT
############################################################

before_enrollment <- nrow(adm_clean)

adm_clean <- adm_clean %>%
  filter(
    admit_date >= enrollment_start &
      admit_date <= enrollment_end
  )

after_enrollment <- nrow(adm_clean)

cat("Rows before enrollment cleaning:",
    before_enrollment,
    "\n")

cat("Rows after enrollment cleaning:",
    after_enrollment,
    "\n")

cat("Rows removed:",
    before_enrollment -
      after_enrollment,
    "\n")

############################################################
# CREATE LENGTH OF STAY VARIABLE
############################################################

adm_clean <- adm_clean %>%
  mutate(
    length_of_stay =
      as.numeric(
        discharge_date - admit_date
      )
  )

summary(adm_clean$length_of_stay)

############################################################
# SORT LONGITUDINALLY
############################################################

adm_clean <- adm_clean %>%
  arrange(patient_id, admit_date)
#View(adm_clean)
############################################################
# CREATE READMISSION VARIABLES
############################################################
#group_by command is MOST imp in longitudinal data
adm_clean <- adm_clean %>%
  group_by(patient_id) %>%
  mutate(
    
    previous_discharge =
      lag(discharge_date),
    
    days_since_last_discharge =
      as.numeric(
        admit_date -
          previous_discharge
      ),
    
    readmission_30d =
      case_when(
        
        !is.na(days_since_last_discharge) &
          days_since_last_discharge >= 0 &
          days_since_last_discharge <= 30
        ~ 1,
        
        TRUE ~ 0
      )
    
  ) %>%
  ungroup()

table(adm_clean$readmission_30d)

View(
  adm_clean %>%
    filter(days_since_last_discharge < 0)
)

#adm_clean %>%
  filter(
    readmission_30d == 1 &
      days_since_last_discharge < 0
  )
  
#no negative intervals are classified as readmissions so validate that:
adm_clean %>%
  filter(
    readmission_30d == 1 &
      days_since_last_discharge < 0
  )

############################################################
# CREATE ICU FLAG VARIABLE
############################################################
View(adm_clean)
str(adm_clean$icu_flag)

adm_clean <- adm_clean %>%
  mutate(
    icu_flag =
      as.numeric(icu_flag)
  )

table(adm_clean$icu_flag,
      useNA = "ifany")

############################################################
# FINAL VALIDATION
############################################################

cat("\n====================================\n")
cat("CLEAN HOSPITAL ADMISSIONS SUMMARY\n")
cat("====================================\n")

cat("Final rows:",
    nrow(adm_clean),
    "\n")

cat("Unique patients:",
    length(unique(adm_clean$patient_id)),
    "\n")

cat("Admissions with ICU stay:",
    sum(adm_clean$icu_flag == 1,
        na.rm = TRUE),
    "\n")

cat("30-day readmissions:",
    sum(adm_clean$readmission_30d == 1,
        na.rm = TRUE),
    "\n")

cat("Mean length of stay:",
    round(
      mean(
        adm_clean$length_of_stay,
        na.rm = TRUE
      ),
      2
    ),
    "\n")

############################################################
# SAVE CLEAN DATASET
############################################################
getwd()
write.csv(
  adm_clean,
  "data_clean/hospital_admissions_clean.csv",
  row.names = FALSE
)

saveRDS(
  adm_clean,
  "data_clean/hospital_admissions_clean.rds"
)

cat("\nClean hospital admissions dataset saved:\n")

cat(
  "data_clean/hospital_admissions_clean.csv\n"
)

############################################################
# END OF SCRIPT
############################################################
