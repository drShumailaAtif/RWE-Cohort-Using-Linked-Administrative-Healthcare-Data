# Oncology Real-World Evidence (RWE): Data Preparation and Analytical Framework

## Project overview

This portfolio project demonstrates an R workflow for preparing synthetic oncology data for real-world evidence (RWE) analyses. The current implementation uses a patient-level cancer registry linked to longitudinal treatment records. It also documents how additional healthcare datasets could support future research questions.

The project is intended to demonstrate skills relevant to epidemiology, health data analytics and oncology RWE roles in Canada. All data are synthetic; no real patient records or Ontario administrative healthcare data are used.

## Aim

Prepare and validate a reproducible oncology cohort and treatment dataset, then use the resulting analysis-ready data to address defined research questions. The current analysis focuses on cohort construction, treatment patterns, data quality and exploratory overall survival.

## Data sources

### Included in the current project

| Dataset | Structure | Main content and purpose |
|---|---|---|
| Patient / cancer registry | Patient-level; one row per patient in this synthetic example | Patient ID, age, sex, cancer type, diagnosis date, death date and last-contact date |
| Longitudinal treatment records | Event-level; multiple records per patient may occur | Patient ID, treatment name and treatment date |

The `patient_id` field links treatment events to the patient registry. The registry provides the patient-level cohort backbone, while the treatment table preserves treatment chronology. The workflow assesses identifiers, duplicates, missingness, data types, date validity, temporal consistency and cohort counts.

### Additional sources relevant to a broader linked-data RWE study

These sources are described as possible extensions. They should only be presented as analysed or linked datasets if they are actually added to the repository and processed.

| Potential data source | Typical structure | Example research use |
|---|---|---|
| Hospital admissions / inpatient records | Longitudinal; multiple admissions per patient | Admission counts, first admission, length of stay and inpatient utilization |
| Emergency department records | Longitudinal; multiple visits per patient | ED visit counts, timing and disposition |
| Physician claims | Longitudinal; multiple billed services per patient | Outpatient visits and healthcare utilization |
| Laboratory and imaging records | Longitudinal; multiple tests or procedures per patient | Monitoring, testing patterns and diagnostic pathways, where variables allow |
| Mortality records | Patient-level or death-event records | Death ascertainment and mortality outcomes; the current synthetic registry already includes a death date |

In linked-data studies, each source needs assessment of record structure, linkage keys, date coverage, coding, missingness and analytical purpose. Event-level datasets should not be joined to one another on patient ID alone without considering unintended row multiplication. Depending on the question, longitudinal records can be summarized to patient level or linked using appropriate event dates and study windows.

## Current analytical questions

The two included datasets support the following questions:

1. **Cohort construction:** How many patients meet the eligibility criteria, and at which steps are records excluded?
2. **Data quality:** Are there duplicate identifiers or treatment records, missing values, invalid dates or inconsistencies requiring review?
3. **Treatment patterns:** Which qualifying treatment is recorded first, and what characterizes the treatment cohort?
4. **Baseline description:** How do available patient characteristics differ between initial treatment groups?
5. **Overall survival:** What are the exploratory Kaplan–Meier estimates by initial treatment group, using defined index-date and censoring rules?
6. **Follow-up derivation:** Are death events, censoring dates and follow-up durations defined consistently and checked for invalid intervals?

Questions about hospital utilization, ED visits, physician services, laboratory monitoring or imaging pathways require those additional datasets and are not claimed as completed analyses in this version.

## Methods and quality assurance

- Import raw files while keeping source data distinct from cleaned and derived data.
- Review dataset dimensions, variable names, data types and record structure.
- Check patient identifiers, duplicates and linkage coverage.
- Standardize selected fields and convert date variables to appropriate formats.
- Assess missingness, invalid dates and temporal or cross-variable inconsistencies.
- Apply transparent eligibility criteria and track cohort attrition.
- Identify the first qualifying treatment and index date according to the analysis definition.
- Derive death-event status, censoring date and follow-up duration.
- Check row counts before and after filtering and joins to identify unintended record loss or row multiplication.
- Produce descriptive summaries and exploratory survival outputs.
- Document assumptions, exclusions, limitations and reproducibility steps.

## Current survival analysis

The current exploratory analysis uses an eligible synthetic lung cancer cohort and compares overall survival between initial treatment groups. Follow-up begins at the defined treatment index date and ends at death or censoring, subject to the study's date rules.

In the current run, 23 patients were included in the survival analysis and 6 deaths were observed. The exploratory log-rank test returned a p-value of approximately 0.801. The dataset is small and simulated; this result is illustrative only. It does not demonstrate equivalence, establish a treatment effect or support clinical conclusions.

## Software

- R and RStudio
- `dplyr`
- `survival`
- `ggplot2`

## Repository structure

Update this outline to match the exact files committed to GitHub.

```text
oncology-rwe-survival-analysis/
├── data/
│   ├── patient_registry.csv
│   └── treatment_records.csv
├── R/
│   ├── 00_setup.R
│   └── oncology_rwe_analysis.R
├── outputs/
│   └── quality-control summaries and analysis outputs
├── docs/
│   └── analysis plan and supporting documentation
├── README.md
└── oncology_rwe_survival_analysis.Rproj
```

Keep raw synthetic input data unchanged. Save cleaned datasets and derived analytical files separately. Only list output filenames after confirming they exist in the repository.

## Reproducibility

1. Install R and RStudio.
2. Open the project `.Rproj` file in RStudio.
3. Install required packages if needed:

   ```r
   install.packages(c("dplyr", "survival", "ggplot2"))
   ```

4. From the project root, run the script using the path and order documented in the repository, for example:

   ```r
   source("R/oncology_rwe_analysis.R")
   ```

5. Review quality-control summaries, cohort counts and analysis outputs.

Check script paths and filenames against the final repository so the instructions work from a fresh R session.

## Limitations

- The data are synthetic and the sample size is small.
- The current implementation uses a patient registry and longitudinal treatment records; it does not claim to link hospital, ED, physician-claims, laboratory or imaging datasets.
- The death date is included in the synthetic registry rather than supplied as a separate external mortality file.
- Treatment groups are not randomized, and the analysis is not a causal treatment-effect estimate.
- The exploratory log-rank comparison is not adjusted for confounding.
- Real-world studies also require a prespecified protocol, data provenance, validated code lists, missing-data assessment, sensitivity analyses and appropriate governance for real healthcare data.

## Skills demonstrated

- R data cleaning and transformation
- Patient-level and longitudinal data structures
- Data quality assurance and validation
- Cohort definition and attrition tracking
- Linkage-key assessment and join validation
- Treatment chronology and index-date construction
- Follow-up, event and censoring derivation
- Descriptive analysis and Kaplan–Meier survival analysis
- Log-rank testing and cautious interpretation
- Reproducible project organization and technical documentation

## CV project description

**Oncology Real-World Evidence Data Preparation and Survival Analysis | R**

- Developed a reproducible R workflow using synthetic patient-registry and longitudinal treatment data.
- Performed data-quality checks, validated identifiers and dates, constructed an eligible cohort, and derived treatment index dates and survival follow-up variables.
- Produced descriptive treatment-group summaries and an exploratory Kaplan–Meier survival analysis.
- Documented how hospital, emergency, physician-claims, laboratory/imaging and mortality data could be incorporated into a broader linked-data RWE framework, distinguishing planned extensions from completed work.

Portfolio project using synthetic data only. The broader data-source framework is conceptual unless those additional datasets are added and analysed.
