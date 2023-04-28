# package libraries
library(here) # project oriented workflow
library(tidyverse) # data reading, wrangling, and tidying
library(lubridate) # dates
# library(haven) # SAS; import
library(janitor) # clean
library(pins) # data access

# source script for pin board
source(
  file = "scripts/01-setup-pin-board.R",
  echo = TRUE
)

# read data  ####
# from pin
hdd_df <- pin_read(
  board = hdd_pb,
  name = "hdd-raw-2007-2021"
)

# inspect
glimpse(hdd_df)

# check for and remove duplicates
hdd_df_tidy <- hdd_df %>%
  distinct(
    az_fac_id,
    pt_medical_rec_number,
    birth_date,
    admission_date,
    admitting_diagnosis,
    .keep_all = TRUE
  )

# dates
# which variables contain a date?
hdd_df_tidy %>%
  select(contains("date")) %>%
  glimpse()

# change all date variables from character to date
hdd_df_tidy <- hdd_df_tidy %>%
  mutate(across(
    .cols = contains("_date"),
    .fns = ymd
  )) %>%
  mutate(
    birth_year = year(birth_date),
    admission_year = year(admission_date),
    discharge_year = year(discharge_date)
  )

# description for variables
unique(hdd_df_tidy$sex)
unique(hdd_df_tidy$race_ethnicity)

# race years
race_desc_eth <- seq(
  min(hdd_df_tidy$admission_year),
  2013,
  1
) %>%
  as.character() %>%
  paste0(collapse = "|")

race_desc_only <- seq(
  2014,
  max(hdd_df_tidy$admission_year),
  1
) %>%
  as.character() %>%
  paste0(collapse = "|")

hdd_df_tidy <- hdd_df_tidy %>%
  mutate(
    # sex
    sex_desc = case_when(
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      sex == "U" ~ "Unknown"
    ),


    # race
    # "From ASCII source file;
    # discharges 2013 and earlier, combination Race/Ethnicity;
    # discharges 2014 and later Race only"
    race_desc = if_else(
      condition = str_detect(
        string = filename,
        pattern = race_desc_only
      ),
      true = case_when(
        race_ethnicity == "1" ~ "American Indian or Alaska Native",
        race_ethnicity == "2" ~ "Asian",
        race_ethnicity == "3" ~ "Black or African American",
        race_ethnicity == "5" ~ "White",
        race_ethnicity == "6" ~ "Native Hawaiian or other Pacific Islander",
        race_ethnicity == "9" ~ "Refused"
      ),
      false = "Other-combined race and ethnicity"
    ),
    # marital status
    marital_status_desc = case_when(
      marital_status == "I" ~ "Single",
      marital_status == "M" ~ "Married",
      marital_status == "S" ~ "Separated",
      marital_status == "D" ~ "Divorced",
      marital_status == "W" ~ "Widowed",
      marital_status == "K" ~ "Unknown",
      marital_status == "C" ~ "Not Applicable (minors too young to legally marry)"
    ),
    # priority of visit
    priority_of_visit_desc = case_when(
      priority_of_visit == "1" ~ "Emergency",
      priority_of_visit == "2" ~ "Urgent",
      priority_of_visit == "3" ~ "Elective",
      priority_of_visit == "4" ~ "Newborn",
      priority_of_visit == "5" ~ "Trauma",
      priority_of_visit == "9" ~ "information not available"
    ),
    # source of visit
    source_of_visit_desc = if_else(
      condition = priority_of_visit == "4",
      true = case_when(
        source_of_visit == "1" ~ "Non-Health Care Facility Point of Origin",
        source_of_visit == "2" ~ "Clinic or Physician’s Office",
        source_of_visit == "4" ~ "Transfer from a Hospital (different facility)",
        source_of_visit == "5" ~ "Born inside this Hospital",
        source_of_visit == "6" ~ "Born outside this Hospital",
        source_of_visit == "8" ~ "Court/Law Enforcement",
        source_of_visit == "9" ~ "Information not available",
        source_of_visit == "D" ~ "Transfer from one Distinct Unit to another Distinct Unit Resulting in a Separate Claim to Payer",
        source_of_visit == "E" ~ "Transfer from Ambulatory Surgery Center",
        source_of_visit == "F" ~ "Transfer from Hospice"
      ),
      false = case_when(
        source_of_visit == "1" ~ "Non-Health Care Facility Point of Origin",
        source_of_visit == "2" ~ "Clinic or Physician’s Office",
        source_of_visit == "4" ~ "Transfer from a Hospital (different facility)",
        source_of_visit == "5" ~ "Transfer from a Skilled Nursing Facility",
        source_of_visit == "6" ~ "Transfer from another Health Care Facility",
        source_of_visit == "8" ~ "Court/Law Enforcement",
        source_of_visit == "9" ~ "Information not available",
        source_of_visit == "D" ~ "Transfer from one Distinct Unit to another Distinct Unit Resulting in a Separate Claim to Payer",
        source_of_visit == "E" ~ "Transfer from Ambulatory Surgery Center",
        source_of_visit == "F" ~ "Transfer from Hospice"
      )
    ),
    # discharge status
    discharge_status_desc = case_when(
      discharge_status == "01" ~ "Discharged to home or self care (routine discharge)",
      discharge_status == "02" ~ "Discharged/transferred to a Short-Term General Hospital for Inpatient care",
      discharge_status == "03" ~ "Discharged/transferred to a Skilled Nursing Facility",
      discharge_status == "04" ~ "Discharged/transferred to an Intermediate Care Facility (Assisted Living Facility)",
      discharge_status == "05" ~ "Discharged/transferred to a Designated Cancer Center or Children’s Hospital",
      discharge_status == "06" ~ "Discharged/transferred to home under care of Organized Home Health Service Organization",
      discharge_status == "07" ~ "Left against medical advice or discontinued care",
      discharge_status == "09" ~ "Admitted as an Inpatient to this Hospital (for state reporting, this code must be used, and is valid, only on ED records when the patient was admitted as an inpatient from the ED and the ED & IP portions of the visit are billed separately)",
      discharge_status == "20" ~ "Expired",
      discharge_status == "21" ~ "Discharged/Transferred to Court/Law Enforcement",
      discharge_status == "41" ~ "Expired in a Medical Facility (hospice patients only)",
      discharge_status == "43" ~ "Discharged/transferred to a Federal Health Care Facility",
      discharge_status == "50" ~ "Discharged home with Hospice",
      discharge_status == "51" ~ "Discharged/transferred to Hospice in a Medical Facility",
      discharge_status == "61" ~ "Discharged/transferred to a Swing Bed",
      discharge_status == "62" ~ "Discharged/transferred to an Inpatient Rehabilitation Facility (IRF)",
      discharge_status == "63" ~ "Discharged/transferred to a Long Term Care Hospital",
      discharge_status == "65" ~ "Discharged/transferred to a Psychiatric Hospital",
      discharge_status == "66" ~ "Discharged/transferred to a Critical Access Hospital",
      discharge_status == "69" ~ "Discharged/transferred to a Designated Disaster Alternative Care Site",
      discharge_status == "70" ~ "Discharged/transferred to another Type of Health Care Institution not Defined Elsewhere in this Code List",
      discharge_status == "81" ~ "Discharged to Home or Self Care (routine discharge) with a planned acute care hospital inpatient readmission",
      discharge_status == "82" ~ "Discharged/transferred to a Short-Term General Hospital for Inpatient care with a planned acute care hospital inpatient readmission",
      discharge_status == "83" ~ "Discharged/transferred to a Skilled Nursing Facility with a planned acute care hospital inpatient readmission",
      discharge_status == "84" ~ "Discharged/transferred to an Intermediate Care Facility (Assisted Living Facility) with a planned acute care hospital inpatient readmission",
      discharge_status == "85" ~ "Discharged/transferred to a Designated Cancer Center or Children’s Hospital with a planned acute care hospital inpatient readmission",
      discharge_status == "86" ~ "Discharged/transferred to home under care of Organized Home Health Service Organization with a planned acute care hospital inpatient readmission",
      discharge_status == "87" ~ "Discharged/Transferred to Court/Law Enforcement with a planned acute care hospital inpatient readmission",
      discharge_status == "88" ~ "Discharged/transferred to a Federal Health Care Facility with a planned acute care hospital inpatient readmission",
      discharge_status == "89" ~ "Discharged/transferred to a Swing Bed with a planned acute care hospital inpatient readmission",
      discharge_status == "90" ~ "Discharged/transferred to an Inpatient Rehabilitation Facility (IRF) with a planned acute care hospital inpatient readmission",
      discharge_status == "91" ~ "Discharged/transferred to a Long Term Care Hospital with a planned acute care hospital inpatient readmission",
      discharge_status == "93" ~ "Discharged/transferred to a Psychiatric Hospital with a planned acute care hospital inpatient readmission",
      discharge_status == "94" ~ "Discharged/transferred to a Critical Access Hospital with a planned acute care hospital inpatient readmission",
      discharge_status == "95" ~ "Discharged/transferred to another Type of Health Care Institution not Defined Elsewhere in this Code List with a planned acute care hospital inpatient readmission"
    ),
    # payer type code
    payer_type_desc = case_when(
      payer_type == "00" ~ "Self Pay",
      payer_type == "01" ~ "Commercial (Indemnity)",
      payer_type == "02" ~ "HMO",
      payer_type == "03" ~ "PPO",
      payer_type == "04" ~ "Discontinued/Reserved",
      payer_type == "05" ~ "Medicare",
      payer_type == "06" ~ "AHCCCS Medicaid (see APRDRG specs page C-62)",
      payer_type == "07" ~ "TRICARE",
      payer_type == "08" ~ "Children’s Rehab Services",
      payer_type == "09" ~ "Workers Compensation",
      payer_type == "10" ~ "Indian Health Services",
      payer_type == "11" ~ "Medicare Risk (Medicare Advantage Plans)",
      payer_type == "12" ~ "Charity",
      payer_type == "13" ~ "Foreign National",
      payer_type == "14" ~ "Other"
    ),
    # tribal affiliation
    tribal = if_else(
      condition = is.na(reservation),
      true = FALSE,
      false = TRUE
    )
  )

# checks
hdd_df_tidy %>%
  tabyl(race_ethnicity_desc, race_ethnicity)

hdd_df %>%
  mutate(
    race_ethnicity_desc = if_else(
      condition = str_detect(
        string = filename,
        pattern = race_desc_eth
      ),
      true = "Other-combined race and ethnicity",
      false = case_when(
        race_ethnicity == "1" ~ "American Indian or Alaska Native",
        race_ethnicity == "2" ~ "Asian",
        race_ethnicity == "3" ~ "Black or African American",
        race_ethnicity == "5" ~ "White",
        race_ethnicity == "6" ~ "Native Hawaiian or other Pacific Islander",
        race_ethnicity == "9" ~ "Refused"
      )
    )
  ) %>%
  tabyl(race_ethnicity_desc, race_ethnicity)


# pin meta information
# pin name
p_name <- str_c(
  "hdd-tidy-",
  min(year(ymd(hdd_df$admission_date))),
  "-",
  max(year(ymd(hdd_df$admission_date)))
)

# pin title
p_title <- str_c(
  "AZDHS Hospital Discharge Data, tidy (",
  min(year(ymd(hdd_df$admission_date))),
  "-",
  max(year(ymd(hdd_df$admission_date))),
  ")"
)

# pin description
p_description <- p_title

# save to pin board
pin_write(
  board = hdd_pb,
  x = hdd_df_tidy,
  name = p_name,
  type = "rds",
  title = p_title,
  description = p_description,
  metadata = list(
    owner = "Coconino HHS",
    department = "Epidemiology",
    user = "rherrera"
  )
)

hdd_df_tidy %>%
  filter(admission_year == 2021) %>%
  filter(n_county == "COCONINO COUNTY") %>%
  filter(n_lat > 32) %>%
  filter(n_lon < 90) %>%
  mutate(
    n_lon = as.numeric(n_lon),
    n_lat = as.numeric(n_lat)
  ) %>%
  ggplot(
    mapping = aes(
      x = n_lon,
      y = n_lat,
      group = payer_type
    )
  ) +
  geom_jitter(mapping = aes(color = sex), alpha = 1 / 6) +
  facet_wrap(~payer_type)
# scale_y_continuous(limits = c(30,40)) +
