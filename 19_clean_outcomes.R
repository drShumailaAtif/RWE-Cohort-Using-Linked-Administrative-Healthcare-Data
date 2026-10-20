```r
############################################################
# FILE: 19_clean_outcomes.R
# PURPOSE:
# Clean and standardize colorectal cancer outcomes
# dataset for oncology RWE analyses
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
# LOAD DATA
############################################################

outcomes_raw <- read.csv(
  "data_raw/outcomes.csv",
  stringsAsFactors = FALSE
)

patients_clean <- read.csv(
  "data_clean/patients_clean.csv",
  stringsAsFactors = FALSE
)

############################################################
# STANDARDIZE COLUMN NAMES
############################################################

names(outcomes_raw) <- tolower(names(outcomes_raw))
names(patients_clean) <- tolower(names(patients_clean))

############################################################
# INITIAL DATA REVIEW
############################################################

cat("====================================\n")
cat("RAW OUTCOMES DATA OVERVIEW\n")
cat("====================================\n")

cat(
  "Rows:",
  nrow(outcomes_raw),
  "\n"
)

cat(
  "Columns:",
  ncol(outcomes_raw),
  "\n"
)

cat(
  "Unique patients:",
  length(
    unique(
      outcomes_raw$patient_id
    )
  ),
  "\n"
)

############################################################
# CREATE WORKING COPY
############################################################

outcomes_clean <- outcomes_raw
str(outcomes_clean)
############################################################
# CONVERT DATE VARIABLES
############################################################

outcomes_clean$diagnosis_date <-
  as.Date(
    outcomes_clean$diagnosis_date
  )

outcomes_clean$death_date <-
  as.Date(
    outcomes_clean$death_date
  )

outcomes_clean$enrollment_start <-
  as.Date(
    outcomes_clean$enrollment_start
  )

outcomes_clean$enrollment_end <-
  as.Date(
    outcomes_clean$enrollment_end
  )

outcomes_clean$last_followup_date <-
  as.Date(
    outcomes_clean$last_followup_date
  )

############################################################
# MISSING PATIENT ID ASSESSMENT
############################################################

total_rows <- nrow(outcomes_clean)

missing_id_n <- sum(
  is.na(outcomes_clean$patient_id)
)

missing_id_pct <- round(
  100 * missing_id_n / total_rows,
  2
)

cat(
  "Total rows:",
  total_rows,
  "\n"
)

cat(
  "Missing patient IDs:",
  missing_id_n,
  "\n"
)

cat(
  "Percent missing:",
  missing_id_pct,
  "%\n"
)

############################################################
# REVIEW RECORDS WITH MISSING IDs
############################################################
missing_id_records <- outcomes_clean %>%
  filter(is.na(patient_id))

nrow(missing_id_records)

############################################################
# SAVE AUDIT FILE
############################################################

write.csv(
  missing_id_records,
  "data_audit/outcomes_missing_patient_ids.csv",
  row.names = FALSE
)

############################################################
# REMOVE RECORDS WITH MISSING IDs
############################################################

outcomes_clean <- outcomes_clean %>%
  filter(!is.na(patient_id))

cat(
  "Rows remaining:",
  nrow(outcomes_clean),
  "\n"
)

cat(
  "Rows removed:",
  missing_id_n,
  "\n"
)
############################################################
# VALIDATE LINKAGE TO PATIENT FILE
############################################################

invalid_linkage_records <- outcomes_clean %>%
  filter(
    !patient_id %in%
      patients_clean$patient_id
  )

write.csv(
  invalid_linkage_records,
  "data_audit/outcomes_invalid_linkage_records.csv",
  row.names = FALSE
)

cat(
  "Invalid linkage records:",
  nrow(
    invalid_linkage_records
  ),
  "\n"
)

nrow(outcomes_clean)
outcomes_clean <- outcomes_clean %>%
  filter(
    patient_id %in%
      patients_clean$patient_id
  )
nrow(outcomes_clean)
############################################################
# REMOVE EXACT DUPLICATES
############################################################

duplicate_records <- outcomes_clean %>%
  group_by(
    across(
      everything()
    )
  ) %>%
  filter(
    n() > 1
  ) %>% 
  ungroup()
nrow(duplicate_records)
write.csv(
  duplicate_records,
  "data_audit/outcomes_exact_duplicates.csv",
  row.names = FALSE
)

before_dup <- nrow(
  outcomes_clean
)

outcomes_clean <- outcomes_clean %>%
  distinct()

after_dup <- nrow(
  outcomes_clean
)

cat(
  "Exact duplicates removed:",
  before_dup - after_dup,
  "\n"
)

############################################################
# REMOVE NEGATIVE SURVIVAL TIMES
############################################################
summary(outcomes_clean$survival_days)
negative_survival_records <- outcomes_clean %>%
  filter(
    !is.na(survival_days) &
      survival_days < 0
  )

nrow(negative_survival_records)
dim(negative_survival_records)

write.csv(
  negative_survival_records,
  "data_audit/outcomes_negative_survival.csv",
  row.names = FALSE
)

table(outcomes_clean$survival_days < 0, useNA = "ifany")
outcomes_clean$survival_days[
  outcomes_clean$survival_days < 0
] <- NA

table(outcomes_clean$survival_days < 0, useNA = "ifany")
table(outcomes_clean$survival_days < 0, useNA = "ifany")
table(is.na(outcomes_clean$survival_days))
############################################################
# REMOVE NEGATIVE PROGRESSION TIMES
############################################################

negative_progression_records <- outcomes_clean %>%
  filter(
    !is.na(
      time_to_progression
    ) &
      time_to_progression < 0
  )

nrow(negative_progression_records)
dim(negative_progression_records)

write.csv(
  negative_progression_records,
  "data_audit/outcomes_negative_progression.csv",
  row.names = FALSE
)

outcomes_clean$time_to_progression[
  outcomes_clean$time_to_progression < 0
] <- NA

table(outcomes_clean$time_to_progression < 0, useNA = "ifany")

############################################################
# STANDARDIZE RESPONSE STATUS
############################################################

outcomes_clean$response_status <-
  str_trim(
    outcomes_clean$response_status
  )

outcomes_clean$response_status <-
  str_to_title(
    outcomes_clean$response_status
  )

valid_response_status <- c(
  "Complete",
  "Partial",
  "Stable",
  "Progressive"
)

outcomes_clean$response_status[
  !is.na(
    outcomes_clean$response_status
  ) &
    !outcomes_clean$response_status %in%
    valid_response_status
] <- NA

table(outcomes_clean$response_status, useNA = "ifany")

############################################################
# FIX RESPONSE / PROGRESSION INCONSISTENCIES
############################################################

response_progression_inconsistency <- outcomes_clean %>%
  filter(
    response_status ==
      "Progressive" &
      progression_flag == 0
  )

write.csv(
  response_progression_inconsistency,
  "data_audit/outcomes_response_progression_inconsistency.csv",
  row.names = FALSE
)

outcomes_clean$progression_flag[
  outcomes_clean$response_status ==
    "Progressive"
] <- 1

############################################################
# VALIDATE FOLLOW-UP DATES
############################################################

invalid_followup_records <- outcomes_clean %>%
  filter(
    !is.na(
      last_followup_date
    ) &
      !is.na(
        diagnosis_date
      ) &
      last_followup_date <
      diagnosis_date
  )
nrow(invalid_followup_records)


write.csv(
  invalid_followup_records,
  "data_audit/outcomes_invalid_followup_dates.csv",
  row.names = FALSE
)

nrow(outcomes_clean)
outcomes_clean <- outcomes_clean %>%
  filter(
    is.na(
      last_followup_date
    ) |
      is.na(
        diagnosis_date
      ) |
      last_followup_date >=
      diagnosis_date
  )
nrow(outcomes_clean)

outcomes_clean %>%
  filter(
    !is.na(last_followup_date) &
      !is.na(diagnosis_date) &
      last_followup_date < diagnosis_date
  ) %>%
  nrow()

############################################################
# CREATE ANALYSIS FLAGS
############################################################

outcomes_clean <- outcomes_clean %>%
  mutate(
    death_flag =
      case_when(
        death_event == 1 ~ 1,
        death_event == 0 ~ 0,
        TRUE ~ NA_real_
      ),
    
    progressed_flag =
      case_when(
        progression_flag == 1 ~ 1,
        progression_flag == 0 ~ 0,
        TRUE ~ NA_real_
      )
  )

table(outcomes_clean$death_event,
      outcomes_clean$death_flag,
      useNA = "ifany")

table(outcomes_clean$progression_flag,
      outcomes_clean$progressed_flag,
      useNA = "ifany")


############################################################
# FINAL SORT
############################################################

outcomes_clean <- outcomes_clean %>%
  arrange(
    patient_id,
    diagnosis_date
  )

############################################################
# FINAL VALIDATION
############################################################

cat("\n====================================\n")
cat("CLEAN OUTCOMES DATA SUMMARY\n")
cat("====================================\n")

cat(
  "Final rows:",
  nrow(outcomes_clean),
  "\n"
)

cat(
  "Unique patients:",
  length(
    unique(
      outcomes_clean$patient_id
    )
  ),
  "\n"
)

cat(
  "Deaths:",
  sum(
    outcomes_clean$death_flag,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Progressions:",
  sum(
    outcomes_clean$progressed_flag,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Missing survival values:",
  sum(
    is.na(
      outcomes_clean$survival_days
    )
  ),
  "\n"
)

############################################################
# SAVE CLEAN DATASET
############################################################

write.csv(
  outcomes_clean,
  "data_clean/outcomes_clean.csv",
  row.names = FALSE
)

saveRDS(
  outcomes_clean,
  "data_clean/outcomes_clean.rds"
)

cat(
  "\nClean outcomes dataset saved:\n"
)

cat(
  "data_clean/outcomes_clean.csv\n"
)

############################################################
# END OF SCRIPT
############################################################
```
