############################################################
# FILE: 17_clean_physician_claims.R
# PURPOSE:
# Clean and standardize longitudinal physician claims
# administrative healthcare data for oncology
# real-world evidence analyses
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
library(janitor)

############################################################
# LOAD RAW DATA
############################################################

claims_raw <- read.csv(
  "data_raw/physician_claims.csv",
  stringsAsFactors = FALSE
)

patients_clean <- read.csv(
  "data_clean/patients_clean.csv",
  stringsAsFactors = FALSE
)

############################################################
# STANDARDIZE COLUMN NAMES
############################################################

claims_raw <- clean_names(claims_raw)

patients_clean <- clean_names(
  patients_clean
)

############################################################
# INITIAL DATA INSPECTION
############################################################
nrow(claims_raw)
str(claims_raw)
summary(claims_raw)
#table()
#unique()


cat("====================================\n")
cat("RAW PHYSICIAN CLAIMS OVERVIEW\n")
cat("====================================\n")
str(claims_raw)
cat("Rows:",
    nrow(claims_raw),
    "\n")

cat("Columns:",
    ncol(claims_raw),
    "\n")

cat("Unique patient IDs:",
    length(unique(claims_raw$patient_id)),
    "\n")

cat("Unique provider IDs:",
    length(unique(claims_raw$provider_id)),
    "\n")

cat("Patients with multiple claims:",
    sum(table(claims_raw$patient_id) > 1,
        na.rm = TRUE),
    "\n")

str(claims_raw)

############################################################
# CREATE WORKING COPY
############################################################

claims_clean <- claims_raw

############################################################
# CONVERT DATE VARIABLES
############################################################

claims_clean$service_date <-
  as.Date(claims_clean$service_date)

claims_clean$enrollment_start <-
  as.Date(claims_clean$enrollment_start)

claims_clean$enrollment_end <-
  as.Date(claims_clean$enrollment_end)

patients_clean$death_date <-
  as.Date(patients_clean$death_date)

############################################################
# REMOVE RECORDS WITH MISSING PATIENT IDs
############################################################

missing_id_records <- claims_clean %>%
  filter(is.na(patient_id))

missing_id_n <- nrow(
  missing_id_records
)

cat("\nMissing patient ID records identified:",
    missing_id_n,
    "\n")

dir.create(
  "data_audit",
  showWarnings = FALSE
)

write.csv(
  missing_id_records,
  "data_audit/claims_missing_patient_ids.csv",
  row.names = FALSE
)

before_missing_id <- nrow(
  claims_clean
)

claims_clean <- claims_clean %>%
  filter(!is.na(patient_id))

after_missing_id <- nrow(
  claims_clean
)

cat("Rows before missing ID cleaning:",
    before_missing_id,
    "\n")

cat("Rows after missing ID cleaning:",
    after_missing_id,
    "\n")

cat("Rows removed:",
    before_missing_id -
      after_missing_id,
    "\n")

############################################################
# KEEP ONLY VALID LINKABLE PATIENTS
############################################################

invalid_linkage_records <- claims_clean %>%
  filter(
    !patient_id %in%
      patients_clean$patient_id
  )

invalid_linkage_n <- nrow(
  invalid_linkage_records
)

cat("\nInvalid linkage records identified:",
    invalid_linkage_n,
    "\n")

write.csv(
  invalid_linkage_records,
  "data_audit/claims_invalid_linkage_records.csv",
  row.names = FALSE
)

before_linkage <- nrow(
  claims_clean
)

claims_clean <- claims_clean %>%
  filter(
    patient_id %in%
      patients_clean$patient_id
  )

after_linkage <- nrow(
  claims_clean
)

cat("Rows before linkage validation:",
    before_linkage,
    "\n")

cat("Rows after linkage validation:",
    after_linkage,
    "\n")

cat("Rows removed:",
    before_linkage -
      after_linkage,
    "\n")

############################################################
# REMOVE EXACT DUPLICATE ROWS
############################################################

claims_clean <- claims_clean %>%
  arrange(
    patient_id,
    service_date,
    provider_id
  )

duplicate_records <- claims_clean %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

duplicate_n <- nrow(
  duplicate_records
)

cat("\nExact duplicate rows identified:",
    duplicate_n,
    "\n")

write.csv(
  duplicate_records,
  "data_audit/claims_exact_duplicates.csv",
  row.names = FALSE
)

before_dup <- nrow(claims_clean)

claims_clean <- claims_clean %>%
  distinct()

after_dup <- nrow(claims_clean)

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

claims_clean <- claims_clean %>%
  mutate(
    
    diagnosis_code =
      str_trim(diagnosis_code),
    
    diagnosis_code =
      str_to_upper(diagnosis_code),
    
    fee_code =
      str_trim(fee_code),
    
    fee_code =
      str_to_upper(fee_code),
    
    provider_id =
      str_trim(provider_id),
    
    provider_id =
      str_to_upper(provider_id)
    
  )

############################################################
# HANDLE BLANK VALUES
############################################################

claims_clean$diagnosis_code[
  claims_clean$diagnosis_code == ""
] <- NA

claims_clean$fee_code[
  claims_clean$fee_code == ""
] <- NA

############################################################
# FIX COMMON DIAGNOSIS CODE ISSUES
############################################################
table(claims_clean$diagnosis_code)
table(claims_clean$diagnosis_code,
      useNA = "ifany")

claims_clean$diagnosis_code <-
  case_when(
    
    claims_clean$diagnosis_code == "I1O"
    ~ "I10",
    
    claims_clean$diagnosis_code == "E1L"
    ~ "E11",
    
    TRUE ~
      claims_clean$diagnosis_code
  )

############################################################
# FIX COMMON FEE CODE ISSUES
############################################################
table(claims_clean$fee_code)
table(claims_clean$fee_code,
      useNA = "ifany")


claims_clean$fee_code <-
  case_when(
    
    claims_clean$fee_code == "B2OO"
    ~ "B200",
    
    TRUE ~
      claims_clean$fee_code
  )

############################################################
# IDENTIFY INVALID DIAGNOSIS CODES
############################################################

invalid_dx_records <- claims_clean %>%
  filter(
    !is.na(diagnosis_code) &
      !str_detect(
        diagnosis_code,
        "^[A-Z][0-9]{2}$"
      )
  )

invalid_dx_n <- nrow(
  invalid_dx_records
)

cat("\nInvalid diagnosis codes identified:",
    invalid_dx_n,
    "\n")

write.csv(
  invalid_dx_records,
  "data_audit/claims_invalid_dx_codes.csv",
  row.names = FALSE
)

############################################################
# REMOVE INVALID DIAGNOSIS CODES
############################################################

before_dx <- nrow(claims_clean)

claims_clean <- claims_clean %>%
  filter(
    is.na(diagnosis_code) |
      str_detect(
        diagnosis_code,
        "^[A-Z][0-9]{2}$"
      )
  )

after_dx <- nrow(claims_clean)

cat("Rows before diagnosis cleaning:",
    before_dx,
    "\n")

cat("Rows after diagnosis cleaning:",
    after_dx,
    "\n")

cat("Rows removed:",
    before_dx - after_dx,
    "\n")

############################################################
# IDENTIFY INVALID FEE CODES
############################################################
table(claims_clean$fee_code)
invalid_fee_records <- claims_clean %>%
  filter(
    !is.na(fee_code) &
      !str_detect(
        fee_code,
        "^[A-Z][0-9]{3}$"
      )
  )

invalid_fee_n <- nrow(
  invalid_fee_records
)

cat("\nInvalid fee codes identified:",
    invalid_fee_n,
    "\n")

write.csv(
  invalid_fee_records,
  "data_audit/claims_invalid_fee_codes.csv",
  row.names = FALSE
)

############################################################
# REMOVE INVALID FEE CODES
############################################################

before_fee <- nrow(claims_clean)

claims_clean <- claims_clean %>%
  filter(
    is.na(fee_code) |
      str_detect(
        fee_code,
        "^[A-Z][0-9]{3}$"
      )
  )

after_fee <- nrow(claims_clean)

cat("Rows before fee code cleaning:",
    before_fee,
    "\n")

cat("Rows after fee code cleaning:",
    after_fee,
    "\n")

cat("Rows removed:",
    before_fee - after_fee,
    "\n")

############################################################
# CLEAN FINANCIAL VARIABLES
############################################################

claims_clean <- claims_clean %>%
  mutate(
    amount_billed =
      as.numeric(amount_billed),
    
    amount_paid =
      as.numeric(amount_paid)
  )

############################################################
# IDENTIFY NEGATIVE FINANCIAL VALUES
############################################################

negative_payment_records <- claims_clean %>%
  filter(
    amount_billed < 0 |
      amount_paid < 0
  )

negative_payment_n <- nrow(
  negative_payment_records
)

cat("\nNegative payment records identified:",
    negative_payment_n,
    "\n")

write.csv(
  negative_payment_records,
  "data_audit/claims_negative_payments.csv",
  row.names = FALSE
)

############################################################
# REMOVE NEGATIVE FINANCIAL VALUES
############################################################

before_payment <- nrow(claims_clean)

claims_clean <- claims_clean %>%
  filter(
    amount_billed >= 0 &
      amount_paid >= 0
  )

after_payment <- nrow(claims_clean)

cat("Rows before payment cleaning:",
    before_payment,
    "\n")

cat("Rows after payment cleaning:",
    after_payment,
    "\n")

cat("Rows removed:",
    before_payment -
      after_payment,
    "\n")

############################################################
# CREATE BILLING FLAGS
############################################################
str(claims_clean)
summary(claims_clean$amount_billed)
claims_clean %>%
  filter(amount_paid > amount_billed) %>%
  select(
    patient_id,
    amount_billed,
    amount_paid
  )

claims_clean <- claims_clean %>%
  mutate(
    
    extreme_billing_flag =
      case_when(
        amount_billed > 3000 ~ 1,
        TRUE ~ 0
      ),
    
    overpayment_flag =
      case_when(
        amount_paid >
          amount_billed ~ 1,
        
        TRUE ~ 0
      )
    
  )
table(claims_clean$overpayment_flag)
############################################################
# IDENTIFY CLAIMS OUTSIDE ENROLLMENT
############################################################

outside_enrollment_records <- claims_clean %>%
  filter(
    service_date < enrollment_start |
      service_date > enrollment_end
  )

outside_enrollment_n <- nrow(
  outside_enrollment_records
)

cat("\nClaims outside enrollment identified:",
    outside_enrollment_n,
    "\n")

write.csv(
  outside_enrollment_records,
  "data_audit/claims_outside_enrollment.csv",
  row.names = FALSE
)

############################################################
# REMOVE CLAIMS OUTSIDE ENROLLMENT
############################################################

before_enrollment <- nrow(
  claims_clean
)

claims_clean <- claims_clean %>%
  filter(
    service_date >= enrollment_start &
      service_date <= enrollment_end
  )

after_enrollment <- nrow(
  claims_clean
)

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
# MERGE DEATH INFORMATION
# PURPOSE:
# Link patient-level mortality information
# into longitudinal physician claims data
############################################################

############################################################
# PRE-MERGE DATA INSPECTION - death date from patient Registry
############################################################

cat("====================================\n")
cat("PRE-MERGE INSPECTION\n")
cat("====================================\n")


# CLAIMS DATA PREVIEW (LEFT DATASET)

cat("\nClaims dataset preview:\n")

claims_clean %>%
  select(
    patient_id,
    service_date
  ) %>%
  head()


# PATIENT REGISTRY PREVIEW (RIGHT DATASET)

cat("\nPatient registry preview:\n")

patients_clean %>%
  select(
    patient_id,
    death_date
  ) %>%
  head()


# CHECK UNIQUE PATIENT COUNTS

claims_unique_patients <-
  length(
    unique(claims_clean$patient_id)
  )

registry_unique_patients <-
  length(
    unique(patients_clean$patient_id)
  )

cat("\nUnique patients in claims dataset:",
    claims_unique_patients,
    "\n")

cat("Unique patients in registry dataset:",
    registry_unique_patients,
    "\n")


# CHECK FOR DUPLICATE IDS IN REGISTRY


duplicate_registry_ids <- patients_clean %>%
  group_by(patient_id) %>%
  filter(n() > 1) %>%
  ungroup()

cat("\nDuplicate patient IDs in registry:",
    nrow(duplicate_registry_ids),
    "\n")

# CHECK FOR UNMATCHED CLAIM IDS

unmatched_claim_ids <- claims_clean %>%
  filter(
    !patient_id %in%
      patients_clean$patient_id
  )

cat("Claims with unmatched patient IDs:",
    nrow(unmatched_claim_ids),
    "\n")


# CHECK WHETHER DEATH DATE ALREADY EXISTS

if("death_date" %in% names(claims_clean)) {
  
  cat("\nExisting death_date column detected.\n")
  
  claims_clean <- claims_clean %>%
    select(-death_date)
  
  cat("Old death_date column removed before merge.\n")
  
}


# STORE PRE-MERGE ROW COUNT

before_merge_rows <-
  nrow(claims_clean)

cat("\nRows before merge:",
    before_merge_rows,
    "\n")


# MERGE DEATH INFORMATION

claims_clean <- claims_clean %>%
  left_join(
    patients_clean %>%
      select(
        patient_id,
        death_date
      ),
    by = "patient_id"
  )


# STORE POST-MERGE ROW COUNT

after_merge_rows <-
  nrow(claims_clean)

cat("Rows after merge:",
    after_merge_rows,
    "\n")

# CHECK WHETHER ROW COUNTS CHANGED

cat("Row count difference:",
    before_merge_rows -
      after_merge_rows,
    "\n")


# CHECK UNIQUE PATIENT COUNT AFTER MERGE

post_merge_unique_patients <-
  length(
    unique(claims_clean$patient_id)
  )

cat("Unique patients after merge:",
    post_merge_unique_patients,
    "\n")


# CONFIRM NEW VARIABLE EXISTS

cat("\nVariables after merge:\n")

names(claims_clean)


# INSPECT MERGED OUTPUT

cat("\nMerged dataset preview:\n")

claims_clean %>%
  select(
    patient_id,
    service_date,
    death_date
  ) %>%
  head()


# CHECK MISSINGNESS OF DEATH DATE

cat("\nDeath date missingness:\n")

table(
  is.na(claims_clean$death_date)
)


# COUNT UNIQUE PATIENTS WITH DEATH RECORDS

patients_with_death <- claims_clean %>%
  filter(!is.na(death_date)) %>%
  distinct(patient_id) %>%
  nrow()

patients_without_death <- claims_clean %>%
  filter(is.na(death_date)) %>%
  distinct(patient_id) %>%
  nrow()

cat("\nUnique patients with death records:",
    patients_with_death,
    "\n")

cat("Unique patients without death records:",
    patients_without_death,
    "\n")


# END OF MERGE QA

############################################################
# IDENTIFY CLAIMS AFTER DEATH
############################################################

after_death_records <- claims_clean %>%
  filter(
    !is.na(death_date) &
      service_date > death_date
  )

after_death_n <- nrow(
  after_death_records
)

cat("\nClaims after death identified:",
    after_death_n,
    "\n")

write.csv(
  after_death_records,
  "data_audit/claims_after_death.csv",
  row.names = FALSE
)

############################################################
# REMOVE CLAIMS AFTER DEATH
############################################################

before_death <- nrow(
  claims_clean
)

claims_clean <- claims_clean %>%
  filter(
    is.na(death_date) |
      service_date <= death_date
  )

after_death <- nrow(
  claims_clean
)

cat("Rows before death-date cleaning:",
    before_death,
    "\n")

cat("Rows after death-date cleaning:",
    after_death,
    "\n")

cat("Rows removed:",
    before_death -
      after_death,
    "\n")

############################################################
# SORT LONGITUDINALLY
############################################################

claims_clean <- claims_clean %>%
  arrange(
    patient_id,
    service_date
  )

############################################################
# CREATE LONGITUDINAL UTILIZATION VARIABLES
############################################################

claims_clean <- claims_clean %>%
  group_by(patient_id) %>%
  mutate(
    
    total_claims_patient =
      n(),
    
    first_claim_date =
      min(service_date),
    
    last_claim_date =
      max(service_date),
    
    days_since_previous_claim =
      as.numeric(
        service_date -
          lag(service_date)
      )
    
  ) %>%
  ungroup()

############################################################
# INSPECT LONGITUDINAL UTILIZATION VARIABLES
############################################################

cat("\n====================================\n")
cat("LONGITUDINAL UTILIZATION PREVIEW\n")
cat("====================================\n")

claims_clean %>%
  select(
    patient_id,
    service_date,
    total_claims_patient,
    first_claim_date,
    last_claim_date,
    days_since_previous_claim
  ) %>%
  arrange(patient_id, service_date) %>%
  head(20)
############################################################
# FINAL VALIDATION
############################################################

cat("\n====================================\n")
cat("CLEAN PHYSICIAN CLAIMS SUMMARY\n")
cat("====================================\n")

cat("Final rows:",
    nrow(claims_clean),
    "\n")

cat("Unique patients:",
    length(unique(claims_clean$patient_id)),
    "\n")

cat("Unique providers:",
    length(unique(claims_clean$provider_id)),
    "\n")

cat("Extreme billing flags:",
    sum(
      claims_clean$extreme_billing_flag == 1,
      na.rm = TRUE
    ),
    "\n")

claims_clean %>%
  filter(extreme_billing_flag == 1)

cat("Overpayment flags:",
    sum(
      claims_clean$overpayment_flag == 1,
      na.rm = TRUE
    ),
    "\n")

claims_clean %>%
  filter(overpayment_flag == 1)

claims_clean %>%
  mutate(
    overpayment_amount =
      amount_paid - amount_billed
  ) %>%
  filter(overpayment_flag == 1) %>%
  select(
    patient_id,
    amount_billed,
    amount_paid,
    overpayment_amount
  )


cat("Mean claims per patient:",
    round(
      mean(
        claims_clean$total_claims_patient,
        na.rm = TRUE
      ),
      2
    ),
    "\n")

claims_clean %>%
  distinct(patient_id, total_claims_patient) %>%
  summarise(
    mean_claims =
      mean(total_claims_patient)
  )

############################################################
# SAVE CLEAN DATASET
############################################################
getwd()
write.csv(
  claims_clean,
  "data_clean/physician_claims_clean.csv",
  row.names = FALSE
)

saveRDS(
  claims_clean,
  "data_clean/physician_claims_clean.rds"
)

cat("\nClean physician claims dataset saved:\n")

cat(
  "data_clean/physician_claims_clean.csv\n"
)

############################################################
# END OF SCRIPT
############################################################
