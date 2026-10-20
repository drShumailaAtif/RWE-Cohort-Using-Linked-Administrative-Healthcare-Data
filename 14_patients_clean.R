############################################################
# FILE: 14_patients_clean.R
# PURPOSE:
# Clean and validate simulated patient registry data
# for longitudinal colorectal cancer RWE cohort
# construction using linked administrative datasets
#
# INPUT:
# data_raw/patients.csv
#
# OUTPUT:
# data_clean/patients_clean.csv
# data_clean/patients_clean.rds
# data_clean/patient_data_quality_summary.csv
# data_clean/duplicate_patient_ids.csv
#
# WORKFLOW LOGIC:
# 1. Understand dataset
# 2. Identify linkage key
# 3. Inspect structure
# 4. Standardize names
# 5. Fix datatypes
# 6. Standardize coding
# 7. Investigate duplicates
# 8. Assess missingness
# 9. Validate temporal logic
# 10. Validate cross-variable consistency
# 11. Derive analytic variables
# 12. Validate final cohort
# 13. Save reproducible outputs
############################################################

############################################################
# LOAD PROJECT SETUP
############################################################
source("scripts/00_setup.R")

############################################################
# LIBRARIES
############################################################
library(dplyr)
library(lubridate)
library(stringr)
library(janitor)
library(readr)

############################################################
# LOAD RAW DATA
############################################################
patients_raw <- read_csv(
  "data_raw/patients.csv",
  show_col_types = FALSE
)

############################################################
# UNDERSTAND DATASET
############################################################
cat("========================================\n")
cat("RAW PATIENT DATA OVERVIEW\n")
cat("========================================\n")

cat("Rows:", nrow(patients_raw), "\n")
cat("Columns:", ncol(patients_raw), "\n")

cat("\nVariable Names:\n")
print(names(patients_raw))

############################################################
# IDENTIFY LINKAGE KEY
############################################################
cat("\n========================================\n")
cat("LINKAGE KEY CHECK\n")
cat("========================================\n")

cat(
  "Unique patient IDs:",
  n_distinct(patients_raw$patient_id),
  "\n"
)

############################################################
# STANDARDIZE VARIABLE NAMES
############################################################
patients <- patients_raw %>%
  clean_names()

############################################################
# INSPECT STRUCTURE
############################################################
cat("\n========================================\n")
cat("DATA STRUCTURE\n")
cat("========================================\n")

str(patients)

############################################################
# FIX DATA TYPES
############################################################
patients <- patients %>%
  mutate(
    
    ########################################################
    # CHARACTER VARIABLES
    ########################################################
    patient_id = as.character(patient_id),
    sex = as.character(sex),
    postal_code = as.character(postal_code),
    data_quality_flag = as.character(data_quality_flag),
    
    ########################################################
    # INTEGER VARIABLES
    ########################################################
    birth_year = as.integer(birth_year),
    rural_flag = as.integer(rural_flag),
    death_flag = as.integer(death_flag),
    income_quintile = as.integer(income_quintile),
    
    ########################################################
    # DATE VARIABLES
    ########################################################
    enrollment_start = as.Date(enrollment_start),
    enrollment_end = as.Date(enrollment_end),
    death_date = as.Date(death_date)
  )
str(patients)


############################################################
# REMOVE LEADING/TRAILING SPACES from character or text variables
############################################################
patients <- patients %>%
  mutate(
    across(
      c(
        patient_id,
        sex,
        postal_code,
        data_quality_flag
      ),
      str_trim
    )
  )

############################################################
# STANDARDIZE CODING
############################################################

############################################################
# SEX VARIABLE
############################################################
unique(patients$sex)
patients %>%
  count(sex)
patients %>%
  count(sex, sort = TRUE)

patients <- patients %>%
  mutate(
    sex = case_when(
      sex %in% c("M", "Male", "male") ~ "M",
      sex %in% c("F", "Female", "female") ~ "F",
      TRUE ~ "Unknown"
    )
  )

############################################################
# POSTAL CODE FORMAT (high-cardinality variables)
############################################################
patients <- patients %>%
  mutate(
    postal_code = str_to_upper(postal_code),
    postal_code = str_replace_all(postal_code, " ", "")
  )

############################################################
# INCOME QUINTILE
############################################################
patients <- patients %>%
  mutate(
    income_quintile = ifelse(
      income_quintile %in% 1:5,
      income_quintile,
      NA
    )
  )

############################################################
# RURAL FLAG
############################################################
patients %>%
  count(rural_flag)
patients <- patients %>%
  mutate(
    rural_flag = ifelse(
      rural_flag %in% c(0,1),
      rural_flag,
      NA
    )
  )

############################################################
# DATA QUALITY FLAG
############################################################
patients %>%
  count(data_quality_flag)
patients <- patients %>%
  mutate(
    data_quality_flag = case_when(
      data_quality_flag == "OK" ~ "OK",
      TRUE ~ "LINKAGE_ISSUE"
    )
  )

############################################################
# DUPLICATE INVESTIGATION
############################################################
cat("\n========================================\n")
cat("DUPLICATE PATIENT ID CHECK\n")
cat("========================================\n")

# IDENTIFY DUPLICATE IDS
duplicate_summary <- patients %>%
  count(patient_id) %>%
  filter(n > 1)
nrow(duplicate_summary)

# EXTRACT FULL DUPLICATE RECORDS
#Is each patient_id from patients found in the duplicate_summary patient_id column?
duplicate_patients <- patients %>%
  filter(patient_id %in% duplicate_summary$patient_id) %>%
  arrange(patient_id)
nrow(duplicate_patients)
View(duplicate_patients)

############################################################
# SAVE DUPLICATE AUDIT TABLE
############################################################
write.csv(
  duplicate_patients,
  "data_clean/duplicate_patients.csv",
  row.names = FALSE
)

############################################################
# CREATE DUPLICATE FLAG
############################################################
patients <- patients %>%
  mutate(
    duplicate_patient_flag = ifelse(
      patient_id %in% duplicate_patient_ids$patient_id,
      1,
      0
    )
  )
patients %>%
  count(duplicate_patient_flag)
############################################################
# ASSESS MISSINGNESS
############################################################
cat("\n========================================\n")
cat("MISSINGNESS ASSESSMENT\n")
cat("========================================\n")
#Create a table showing how many missing values each variable contains.
missing_summary <- data.frame(
  variable = names(patients),
  missing_n = sapply(
    patients,
    function(x) sum(is.na(x))
  )
)

#Calculate what percentage of each variable is missing and save it as a new column.2 decimal places
missing_summary$missing_pct <-
  round(
    missing_summary$missing_n /
      nrow(patients) * 100,
    2
  )

print(missing_summary)

View(missing_summary)
View(patients)
############################################################
# TEMPORAL LOGIC VALIDATION
############################################################

############################################################
# IMPOSSIBLE BIRTH YEAR
############################################################
patients <- patients %>%
  mutate(
    invalid_birth_year_flag = ifelse(
      birth_year < 1900 |
        birth_year > year(Sys.Date()),
      1,
      0
    )
  )
patients %>%
  count(invalid_birth_year_flag)
############################################################
# DEATH BEFORE ENROLLMENT
############################################################
patients <- patients %>%
  mutate(
    death_before_enrollment_flag = ifelse(
      !is.na(death_date) &
        death_date < enrollment_start,
      1,
      0
    )
  )
patients %>%
  count(death_before_enrollment_flag)
############################################################
# ENROLLMENT AFTER STUDY END
############################################################
#study_start <- as.Date("2010-01-01")
study_end   <- as.Date("2023-12-31")
summary(patients$enrollment_start)
summary(patients$death_date)


patients <- patients %>%
  mutate(
    enrollment_after_study_end_flag = ifelse(
      enrollment_start > study_end,
      1,
      0
    )
  )
patients %>%
  count(enrollment_after_study_end_flag)
############################################################
# NEGATIVE FOLLOW-UP
############################################################
patients <- patients %>%
  mutate(
    negative_follow_up_flag = ifelse(
      enrollment_end < enrollment_start,
      1,
      0
    )
  )
patients %>%
  count(negative_follow_up_flag)
patients %>%
  filter(negative_follow_up_flag == 1)
############################################################
# CROSS-VARIABLE CONSISTENCY
############################################################

############################################################
# DEATH FLAG VS DEATH DATE
############################################################
patients <- patients %>%
  mutate(
    death_flag_inconsistency = ifelse(
      (death_flag == 1 & is.na(death_date)) |
        (death_flag == 0 & !is.na(death_date)),
      1,
      0
    )
  )
patients %>%
  count(death_flag_inconsistency)
############################################################
# AGE VS BIRTH YEAR
############################################################
patients <- patients %>%
  mutate(
    age_at_enrollment =
      year(enrollment_start) - birth_year
  )

patients <- patients %>%
  mutate(
    invalid_age_flag = ifelse(
      age_at_enrollment < 0 |
        age_at_enrollment > 115,
      1,
      0
    )
  )
patients %>%
  count(invalid_age_flag)
View(patients)
############################################################
# ENROLLMENT END VS DEATH DATE
############################################################
patients <- patients %>%
  mutate(
    enrollment_death_mismatch_flag = ifelse(
      death_flag == 1 &
        enrollment_end != death_date,
      1,
      0
    )
  )
patients %>%
  count(enrollment_death_mismatch_flag)
patients %>%
  filter(enrollment_death_mismatch_flag == 1)
############################################################
# CLEAN INVALID VALUES
############################################################

############################################################
# FIX INVALID BIRTH YEAR
############################################################
#If birth year is invalid, replace it with NA. Otherwise keep the original birth year.
patients <- patients %>%
  mutate(
    birth_year = ifelse(
      invalid_birth_year_flag == 1,
      NA,
      birth_year
    )
  )
patients %>%
  count(birth_year)
############################################################
# FIX INVALID DEATH DATE
############################################################
patients <- patients %>%
  mutate(
    death_date = ifelse(
      death_before_enrollment_flag == 1,
      NA,
      death_date
    ),
    str(patients$death_date)  
    death_date = as.Date(
      death_date,
      origin = "1970-01-01"
    )
  )
patients %>%
  filter(death_before_enrollment_flag == 1)
patients %>%
  count(death_date)
############################################################
# FIX NEGATIVE FOLLOW-UP
############################################################
patients <- patients %>%
  mutate(
    enrollment_end = ifelse(
      negative_follow_up_flag == 1,
      NA,
      enrollment_end
    ),
    
    enrollment_end = as.Date(
      enrollment_end,
      origin = "1970-01-01"
    )
  )

############################################################
# RECALCULATE DEATH FLAG
############################################################
patients %>%
  count(death_flag, is.na(death_date))

patients <- patients %>%
  mutate(
    death_flag = case_when(
      !is.na(death_date) ~ 1,
      TRUE ~ 0
    )
  )


############################################################
# DERIVE ANALYTIC VARIABLES
############################################################

############################################################
# FOLLOW-UP TIME
############################################################
patients <- patients %>%
  mutate(
    follow_up_days =
      as.numeric(
        enrollment_end - enrollment_start
      )
  )

summary(patients$follow_up_days)

patients %>%
  select(
    enrollment_start,
    enrollment_end,
    follow_up_days
  ) %>%
  head()

patients %>%
  filter(follow_up_days < 0)

patients <- patients %>%
  mutate(
    follow_up_years =
      round(
        follow_up_days / 365.25,
        2
      )
  )

summary(patients$follow_up_years)
patients %>%
  filter(follow_up_years < 0)
############################################################
# SURVIVAL TIME
############################################################
#Check death_flag,death_date Missingness,follow_up_days,Impossible Death Dates
patients <- patients %>%
  mutate(
    survival_time_days =
      ifelse(
        death_flag == 1,
        as.numeric(
          death_date - enrollment_start
        ),
        follow_up_days
      )
  )
summary(patients$survival_time_days)
patients %>%
  filter(survival_time_days < 0)
patients %>%
  select(
    patient_id,
    enrollment_start,
    death_date,
    death_flag,
    follow_up_days,
    survival_time_days
  ) %>%
  head(10)
############################################################
# CENSORING INDICATOR
############################################################
patients %>%
  count(death_flag)
patients %>%
  count(death_flag, is.na(death_date))
sum(is.na(patients$death_flag))
patients %>%
  filter(
    death_flag == 1 &
      death_date < enrollment_start
  )
patients %>%
  filter(
    death_flag == 1 &
      enrollment_end != death_date
  )

patients <- patients %>%
  mutate(
    censor_flag = ifelse(
      death_flag == 1,
      0,
      1
    )
  )

############################################################
# COHORT ELIGIBILITY FLAG
############################################################
patients <- patients %>%
  mutate(
    cohort_eligible_flag = ifelse(
      invalid_birth_year_flag == 0 &
        death_before_enrollment_flag == 0 &
        negative_follow_up_flag == 0 &
        enrollment_after_study_end_flag == 0,
      1,
      0
    )
  )
patients %>%
  count(cohort_eligible_flag)
patients %>%
  filter(cohort_eligible_flag == 0)

############################################################
# RESOLVE DUPLICATE PATIENT IDS
# KEEP MOST COMPLETE RECORD
############################################################
dim(patients)
missing_summary
colSums(is.na(patients))

patients_clean <- patients %>%
  count(patient_id) %>%
  filter(n > 1)

View(patients_clean)

patients_clean <- patients %>%
  mutate(
    completeness_score =
      rowSums(!is.na(.))
  )
  
View(
    patients_clean %>%
      select(
        patient_id,
        completeness_score
      )
  )
  duplicate_patients <- patients_clean %>%
    filter(
      patient_id %in%
        duplicate_summary$patient_id
    ) %>%
    
    arrange(
      patient_id,
      desc(completeness_score)
    )

  View(
    duplicate_patients %>%
      select(
        patient_id,
        completeness_score
      )
  )
  
  patients_clean <- patients_clean %>%
    
    arrange(
      patient_id,
      desc(completeness_score)
    ) %>%
    
    distinct(
      patient_id,
      .keep_all = TRUE
    )
  dim(patients_clean)
  patients_clean <- patients_clean %>%
    select(-completeness_score)
  dim(patients_clean)

############################################################
# FINAL COHORT VALIDATION
############################################################
cat("\n========================================\n")
cat("FINAL COHORT VALIDATION\n")
cat("========================================\n")

cat(
  "Final rows:",
  nrow(patients_clean),
  "\n"
)

cat(
  "Unique patient IDs:",
  n_distinct(patients_clean$patient_id),
  "\n"
)

cat(
  "Duplicate IDs remaining:",
  sum(duplicated(patients_clean$patient_id)),
  "\n"
)

cat(
  "Invalid birth years:",
  sum(
    patients_clean$invalid_birth_year_flag,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Death before enrollment:",
  sum(
    patients_clean$death_before_enrollment_flag,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Negative follow-up:",
  sum(
    patients_clean$negative_follow_up_flag,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Cohort eligible patients:",
  sum(
    patients_clean$cohort_eligible_flag,
    na.rm = TRUE
  ),
  "\n"
)

############################################################
# DATA QUALITY SUMMARY
############################################################
data_quality_summary <- data.frame(
  
  metric = c(
    "raw_rows",
    "final_rows",
    "unique_patients",
    "duplicate_patient_ids",
    "missing_birth_year",
    "missing_income_quintile",
    "invalid_birth_year",
    "death_before_enrollment",
    "negative_follow_up",
    "cohort_eligible_patients"
  ),
  
  value = c(
    nrow(patients_raw),
    nrow(patients_clean),
    n_distinct(patients_clean$patient_id),
    nrow(duplicate_patient_ids),
    sum(is.na(patients_clean$birth_year)),
    sum(is.na(patients_clean$income_quintile)),
    sum(patients_clean$invalid_birth_year_flag),
    sum(patients_clean$death_before_enrollment_flag),
    sum(patients_clean$negative_follow_up_flag),
    sum(patients_clean$cohort_eligible_flag)
  )
)

############################################################
# SAVE CLEAN DATA
############################################################
getwd()
write.csv(
  patients_clean,
  "data_clean/patients_clean.csv",
  row.names = FALSE
)

saveRDS(
  patients_clean,
  "data_clean/patients_clean.rds"
)

write.csv(
  data_quality_summary,
  "data_clean/patient_data_quality_summary.csv",
  row.names = FALSE
)

############################################################
# COMPLETION MESSAGE
############################################################
cat("\n========================================\n")
cat("PATIENT DATA CLEANING COMPLETED\n")
cat("========================================\n")

cat("Files generated:\n")
cat("- data_clean/patients_clean.csv\n")
cat("- data_clean/patients_clean.rds\n")
cat("- data_clean/patient_data_quality_summary.csv\n")
cat("- data_clean/duplicate_patient_ids.csv\n")

cat("========================================\n")
