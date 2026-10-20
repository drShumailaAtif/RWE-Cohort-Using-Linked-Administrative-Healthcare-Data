############################################################
# FILE: 18_clean_drug_claims.R
# PURPOSE:
# Clean and standardize longitudinal drug claims
# administrative healthcare data for oncology
# real-world evidence analyses
############################################################
#That is why pharmacy RWE often combines:
#DIN, quantity, days_supply & dispense_date
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

getwd()
############################################################
# LOAD RAW DATA
############################################################

rx_raw <- read.csv(
  "data_raw/drug_claims.csv",
  stringsAsFactors = FALSE
)

patients_clean <- read.csv(
  "data_clean/patients_clean.csv",
  stringsAsFactors = FALSE
)

############################################################
# STANDARDIZE COLUMN NAMES
############################################################

rx_raw <- clean_names(rx_raw)

patients_clean <- clean_names(
  patients_clean
)

############################################################
# INITIAL DATA INSPECTION
############################################################

nrow(rx_raw)
str(rx_raw)
summary(rx_raw)

cat("====================================\n")
cat("RAW DRUG CLAIMS OVERVIEW\n")
cat("====================================\n")

cat("Rows:",
    nrow(rx_raw),
    "\n")

cat("Columns:",
    ncol(rx_raw),
    "\n")

cat("Unique patient IDs:",
    length(unique(rx_raw$patient_id)),
    "\n")

cat("Unique DINs:",
    length(unique(rx_raw$din)),
    "\n")

cat("Unique pharmacies:",
    length(unique(rx_raw$pharmacy_id)),
    "\n")

cat("Patients with multiple claims:",
    sum(table(rx_raw$patient_id) > 1,
        na.rm = TRUE),
    "\n")

str(rx_raw)

############################################################
# CREATE WORKING COPY
############################################################

rx_clean <- rx_raw

############################################################
# CONVERT DATE VARIABLES
############################################################

rx_clean$dispense_date <-
  as.Date(rx_clean$dispense_date)

rx_clean$enrollment_start <-
  as.Date(rx_clean$enrollment_start)

rx_clean$enrollment_end <-
  as.Date(rx_clean$enrollment_end)

rx_clean$death_date <-
  as.Date(rx_clean$death_date)

patients_clean$death_date <-
  as.Date(patients_clean$death_date)

str(rx_clean)
str(patients_clean)

############################################################
# STANDARDIZE CHARACTER VARIABLES
############################################################

rx_clean <- rx_clean %>%
  mutate(
    
    patient_id =
      str_trim(patient_id),
    
    din =
      str_trim(din),
    
    drug_name =
      str_trim(drug_name),
    
    prescriber_id =
      str_trim(prescriber_id),
    
    pharmacy_id =
      str_trim(pharmacy_id),
    
    drug_name =
      str_to_upper(drug_name),
    
    prescriber_id =
      str_to_upper(prescriber_id),
    
    pharmacy_id =
      str_to_upper(pharmacy_id)
    
  )

############################################################
# HANDLE BLANK VALUES
############################################################

character_vars <- c(
  "patient_id",
  "din",
  "drug_name",
  "prescriber_id",
  "pharmacy_id"
)

rx_clean[character_vars] <- lapply(
  rx_clean[character_vars],
  function(x) {
    
    x <- str_trim(x)
    x[x == ""] <- NA
    
    return(x)
  }
)
############################################################
# CREATE AUDIT DIRECTORY
############################################################

dir.create(
  "data_audit",
  showWarnings = FALSE
)

############################################################
# REMOVE RECORDS WITH MISSING PATIENT IDs
############################################################

missing_id_records <- rx_clean %>%
  filter(is.na(patient_id))

missing_id_n <- nrow(
  missing_id_records
)

cat("\nMissing patient ID records identified:",
    missing_id_n,
    "\n")

write.csv(
  missing_id_records,
  "data_audit/drug_claims_missing_patient_ids.csv",
  row.names = FALSE
)

before_missing_id <- nrow(
  rx_clean
)

rx_clean <- rx_clean %>%
  filter(!is.na(patient_id))

after_missing_id <- nrow(
  rx_clean
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

invalid_linkage_records <- rx_clean %>%
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
  "data_audit/drug_claims_invalid_linkage_records.csv",
  row.names = FALSE
)

before_linkage <- nrow(
  rx_clean
)

rx_clean <- rx_clean %>%
  filter(
    patient_id %in%
      patients_clean$patient_id
  )

after_linkage <- nrow(
  rx_clean
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

rx_clean <- rx_clean %>%
  arrange(
    patient_id,
    dispense_date,
    din
  )

duplicate_records <- rx_clean %>%
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
  "data_audit/drug_claims_exact_duplicates.csv",
  row.names = FALSE
)

before_dup <- nrow(rx_clean)

rx_clean <- rx_clean %>%
  distinct()

after_dup <- nrow(rx_clean)

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
# CLEAN DIN VARIABLES
############################################################

table(rx_clean$din,
      useNA = "ifany")

############################################################
# IDENTIFY INVALID DIN VALUES
############################################################

invalid_din_records <- rx_clean %>%
  filter(
    !is.na(din) &
      !str_detect(
        din,
        "^[0-9]{8}$"
      )
  )

invalid_din_n <- nrow(
  invalid_din_records
)

cat("\nInvalid DIN records identified:",
    invalid_din_n,
    "\n")

write.csv(
  invalid_din_records,
  "data_audit/drug_claims_invalid_dins.csv",
  row.names = FALSE
)

############################################################
# REMOVE INVALID DIN VALUES
############################################################

before_din <- nrow(rx_clean)

rx_clean <- rx_clean %>%
  filter(
    is.na(din) |
      str_detect(
        din,
        "^[0-9]{8}$"
      )
  )

after_din <- nrow(rx_clean)

cat("Rows before DIN cleaning:",
    before_din,
    "\n")

cat("Rows after DIN cleaning:",
    after_din,
    "\n")

cat("Rows removed:",
    before_din -
      after_din,
    "\n")

############################################################
# STANDARDIZE DRUG NAMES
############################################################
table(
  rx_clean$drug_name,
  useNA = "ifany"
)


rx_clean$drug_name <-
  case_when(
    
    rx_clean$drug_name == "METFORMIN"
    ~ "METFORMIN",
    
    rx_clean$drug_name == "ATORVA"
    ~ "ATORVASTATIN",
    
    TRUE ~
      rx_clean$drug_name
  )

############################################################
# CLEAN NUMERIC VARIABLES
############################################################
str(rx_clean)
rx_clean <- rx_clean %>%
  mutate(
    
    days_supply =
      as.numeric(days_supply),
    
    quantity =
      as.numeric(quantity),
    
    drug_cost =
      as.numeric(drug_cost)
    
  )

############################################################
# IDENTIFY NEGATIVE VALUES
############################################################
sum(rx_clean$quantity < 0, na.rm = TRUE)

sum(rx_clean$drug_cost < 0, na.rm = TRUE)

sum(rx_clean$days_supply < 0, na.rm = TRUE)


negative_value_records <- rx_clean %>%
  filter(
    quantity < 0 |
      drug_cost < 0 |
      days_supply < 0
  )

negative_value_n <- nrow(
  negative_value_records
)

cat("\nNegative value records identified:",
    negative_value_n,
    "\n")

write.csv(
  negative_value_records,
  "data_audit/drug_claims_negative_values.csv",
  row.names = FALSE
)

############################################################
# REMOVE NEGATIVE VALUES
############################################################

before_negative <- nrow(rx_clean)

rx_clean <- rx_clean %>%
  filter(
    
    is.na(quantity) |
      quantity >= 0,
    
    is.na(drug_cost) |
      drug_cost >= 0,
    
    is.na(days_supply) |
      days_supply >= 0
    
  )

after_negative <- nrow(rx_clean)

cat("Rows before negative value cleaning:",
    before_negative,
    "\n")

cat("Rows after negative value cleaning:",
    after_negative,
    "\n")

cat("Rows removed:",
    before_negative -
      after_negative,
    "\n")

############################################################
# CREATE COST FLAGS
############################################################

rx_clean <- rx_clean %>%
  mutate(
    
    zero_cost_flag =
      case_when(
        drug_cost == 0 ~ 1,
        TRUE ~ 0
      ),
    
    extreme_cost_flag =
      case_when(
        drug_cost > 1000 ~ 1,
        TRUE ~ 0
      )
    
  )

############################################################
# IDENTIFY MISSING ENROLLMENT INFORMATION
############################################################

missing_enrollment_records <- rx_clean %>%
  filter(
    is.na(enrollment_start) |
      is.na(enrollment_end)
  )

missing_enrollment_n <- nrow(
  missing_enrollment_records
)

cat(
  "\nClaims with missing enrollment dates identified:",
  missing_enrollment_n,
  "\n"
)

write.csv(
  missing_enrollment_records,
  "data_audit/drug_claims_missing_enrollment_dates.csv",
  row.names = FALSE
)

############################################################
# REMOVE MISSING ENROLLMENT RECORDS
############################################################

before_missing_enrollment <- nrow(
  rx_clean
)

rx_clean <- rx_clean %>%
  filter(
    !is.na(enrollment_start),
    !is.na(enrollment_end)
  )

after_missing_enrollment <- nrow(
  rx_clean
)

cat(
  "Rows before missing enrollment cleaning:",
  before_missing_enrollment,
  "\n"
)

cat(
  "Rows after missing enrollment cleaning:",
  after_missing_enrollment,
  "\n"
)

cat(
  "Rows removed:",
  before_missing_enrollment -
    after_missing_enrollment,
  "\n"
)

############################################################
# IDENTIFY CLAIMS OUTSIDE ENROLLMENT
############################################################
#The normal order is:
#enrollment_start  --->  dispense_date  --->  enrollment_end

outside_enrollment_records <- rx_clean %>%
  filter(
    dispense_date < enrollment_start |
      dispense_date > enrollment_end
  )

outside_enrollment_n <- nrow(
  outside_enrollment_records
)

cat(
  "\nClaims outside enrollment identified:",
  outside_enrollment_n,
  "\n"
)

write.csv(
  outside_enrollment_records,
  "data_audit/drug_claims_outside_enrollment.csv",
  row.names = FALSE
)

############################################################
# REMOVE CLAIMS OUTSIDE ENROLLMENT
############################################################

before_enrollment <- nrow(
  rx_clean
)

rx_clean <- rx_clean %>%
  filter(
    dispense_date >= enrollment_start &
      dispense_date <= enrollment_end
  )

after_enrollment <- nrow(
  rx_clean
)

cat(
  "Rows before enrollment cleaning:",
  before_enrollment,
  "\n"
)

cat(
  "Rows after enrollment cleaning:",
  after_enrollment,
  "\n"
)

cat(
  "Rows removed:",
  before_enrollment -
    after_enrollment,
  "\n"
)

############################################################
# MERGE DEATH INFORMATION
############################################################
#“Keep ALL rows from the LEFT dataset, and attach matching information from the RIGHT dataset.”

View(rx_clean)
View(patients_clean)
table(
  patients_clean$death_flag,
  is.na(patients_clean$death_date)
)


if("death_date" %in% names(rx_clean)) {
  
  rx_clean <- rx_clean %>%
    select(-death_date)
  
}

before_merge_rows <-
  nrow(rx_clean)
before_merge_rows

rx_clean <- rx_clean %>%
  left_join(
    patients_clean %>%
      select(
        patient_id,
        death_date
      ),
    by = "patient_id"
  )

after_merge_rows <-
  nrow(rx_clean)

cat("\nRows before death merge:",
    before_merge_rows,
    "\n")

cat("Rows after death merge:",
    after_merge_rows,
    "\n")

############################################################
# IDENTIFY CLAIMS AFTER DEATH
############################################################

after_death_records <- rx_clean %>%
  filter(
    !is.na(death_date) &
      dispense_date > death_date
  )

after_death_n <- nrow(
  after_death_records
)

cat("\nDrug claims after death identified:",
    after_death_n,
    "\n")

write.csv(
  after_death_records,
  "data_audit/drug_claims_after_death.csv",
  row.names = FALSE
)

############################################################
# REMOVE CLAIMS AFTER DEATH
############################################################

before_death <- nrow(
  rx_clean
)

rx_clean <- rx_clean %>%
  filter(
    is.na(death_date) |
      dispense_date <= death_date
  )

after_death <- nrow(
  rx_clean
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
# CREATE LONGITUDINAL UTILIZATION VARIABLES
############################################################

# SORT DATA
rx_clean <- rx_clean %>%
  arrange(
    patient_id,
    drug_name,
    dispense_date
  )

View(rx_clean)

rx_clean %>%
  select(
    patient_id,
    drug_name,
    dispense_date
  ) %>%
  print(n = 100)

############################################################
# TOTAL PRESCRIPTIONS ACROSS ALL DRUGS
############################################################

rx_clean <- rx_clean %>%
  
  group_by(patient_id) %>%
  
  mutate(
    total_rx_patient =
      n()
  ) %>%
  
  ungroup()

rx_clean %>%
  select(
    patient_id,
    drug_name,
    dispense_date,
    total_rx_patient
  ) %>%
  View()

############################################################
# GROUP BY PATIENT + DRUG
############################################################

rx_clean <- rx_clean %>%
  group_by(
    patient_id,
    drug_name
  )

############################################################
# TOTAL PRESCRIPTIONS FOR SAME DRUG
############################################################

rx_clean <- rx_clean %>%
  mutate(
    total_rx_drug =
      n()
  )

rx_clean %>%
  select(
    patient_id,
    drug_name,
    dispense_date,
    total_rx_patient,
    total_rx_drug
  ) %>%
  View()

############################################################
# FIRST PRESCRIPTION DATE FOR SAME DRUG
############################################################

rx_clean <- rx_clean %>%
  mutate(
    first_rx_date =
      min(dispense_date)
  )

rx_clean %>%
  select(
    patient_id,
    drug_name,
    dispense_date,
    first_rx_date
  ) %>%
  View()

############################################################
# LAST PRESCRIPTION DATE FOR SAME DRUG
############################################################

rx_clean <- rx_clean %>%
  mutate(
    last_rx_date =
      max(dispense_date)
  )

rx_clean %>%
  select(
    patient_id,
    drug_name,
    dispense_date,
    last_rx_date
  ) %>%
  View()

############################################################
# DAYS SINCE PREVIOUS PRESCRIPTION
############################################################

# GROUP BY PATIENT + DRUG

rx_clean <- rx_clean %>%
  arrange(
    patient_id,
    drug_name,
    dispense_date
  ) %>%
  group_by(
    patient_id,
    drug_name
  )
rx_clean <- rx_clean %>%
  mutate(
    
    days_since_previous_rx =
      as.numeric(
        dispense_date -
          lag(dispense_date)
      )
    
  )

rx_clean %>%
  select(
    patient_id,
    drug_name,
    dispense_date,
    days_since_previous_rx
  ) %>%
  View()

summary(rx_clean$days_since_previous_rx)

table(
  cut(
    rx_clean$days_since_previous_rx,
    breaks = c(0,30,90,180,365,1000,5000)
  ),
  useNA = "ifany"
)
table(rx_clean$total_rx_drug)
summary(rx_clean$total_rx_drug)
############################################################
# PREVIOUS DAYS SUPPLY
############################################################
#sort again specifically for the variables that depend on row order.
#lag() Take the value from the row immediately above.
#lead()
#first()
#last()
#cumsum()

#FOR THESE
#total_rx_drug
#first_rx_date
#last_rx_date
#days_since_previous_rx
#previous_days_supply
#overlap_flag

#sorting
rx_clean <- rx_clean %>%
  arrange(
    patient_id,
    drug_name,
    dispense_date
  ) %>%
  group_by(
    patient_id,
    drug_name
  )


rx_clean <- rx_clean %>%
  mutate(
    previous_days_supply =
      lag(days_supply)
  )

rx_clean %>%
  select(
    patient_id,
    drug_name,
    dispense_date,
    days_supply,
    previous_days_supply
  ) %>%
  View()

############################################################
# OVERLAP FLAG
############################################################

rx_clean <- rx_clean %>%
  mutate(
    
    overlap_flag =
      case_when(
        
        !is.na(days_since_previous_rx) &
          !is.na(previous_days_supply) &
          days_since_previous_rx <
          previous_days_supply
        ~ 1,
        
        TRUE ~ 0
      )
    
  )

rx_clean %>%
  select(
    patient_id,
    drug_name,
    dispense_date,
    days_since_previous_rx,
    previous_days_supply,
    overlap_flag
  ) %>%
  View()

round(
  prop.table(
    table(
      rx_clean$overlap_flag,
      useNA = "ifany"
    )
  ) * 100,
  2
)
############################################################
# REMOVE GROUPING
############################################################

rx_clean <- rx_clean %>%
  ungroup()
############################################################
# INSPECT LONGITUDINAL UTILIZATION VARIABLES
############################################################

cat("\n====================================\n")
cat("LONGITUDINAL DRUG UTILIZATION PREVIEW\n")
cat("====================================\n")

rx_clean %>%
  select(
    patient_id,
    drug_name,
    dispense_date,
    total_rx_patient,
    total_rx_drug,
    first_rx_date,
    last_rx_date,
    days_since_previous_rx,
    previous_days_supply,
    overlap_flag
  ) %>%
  arrange(
    patient_id,
    drug_name,
    dispense_date
  ) %>%
  head(20)

############################################################
# FINAL VALIDATION
############################################################

cat("\n====================================\n")
cat("CLEAN DRUG CLAIMS SUMMARY\n")
cat("====================================\n")

cat("Final rows:",
    nrow(rx_clean),
    "\n")

cat("Unique patients:",
    length(unique(rx_clean$patient_id)),
    "\n")

cat("Unique DINs:",
    length(unique(rx_clean$din)),
    "\n")

cat("Unique pharmacies:",
    length(unique(rx_clean$pharmacy_id)),
    "\n")

cat("Extreme cost flags:",
    sum(
      rx_clean$extreme_cost_flag == 1,
      na.rm = TRUE
    ),
    "\n")

cat("Zero cost flags:",
    sum(
      rx_clean$zero_cost_flag == 1,
      na.rm = TRUE
    ),
    "\n")

cat("Prescription overlap flags:",
    sum(
      rx_clean$overlap_flag == 1,
      na.rm = TRUE
    ),
    "\n")

cat("Missing refill gaps:",
    sum(
      is.na(
        rx_clean$days_since_previous_rx
      )
    ),
    "\n")

cat("Missing previous supply:",
    sum(
      is.na(
        rx_clean$previous_days_supply
      )
    ),
    "\n")

cat("Mean prescriptions per patient:",
    round(
      nrow(rx_clean) /
        length(
          unique(
            rx_clean$patient_id
          )
        ),
      2
    ),
    "\n")
############################################################
# SAVE CLEAN DATASET
############################################################
getwd()
write.csv(
  rx_clean,
  "data_clean/drug_claims_clean.csv",
  row.names = FALSE
)

saveRDS(
  rx_clean,
  "data_clean/drug_claims_clean.rds"
)

cat("\nClean drug claims dataset saved:\n")

cat(
  "data_clean/drug_claims_clean.csv\n"
)

############################################################
# END OF SCRIPT
############################################################
