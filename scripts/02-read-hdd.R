# package libraries
library(here) # project oriented workflow
library(tidyverse) # data reading, wrangling, and tidying
library(lubridate) # dates
# library(haven) # SAS; import
library(janitor) # clean
library(pins) # data access
library(visdat)

# source script for pin board
source(
  file = "scripts/01-setup-pin-board.R",
  echo = TRUE
)

# read finalized mortality data ####
# collect file names for function
filenames <- list.files(
  path = "S:/HIPAA Compliance/Hospital Discharge Data/HDD_Updated-05042021/",
  recursive = TRUE,
  pattern = "coconino.*?\\.csv",
  full.names = TRUE
)

# collect file names to name environment objects
filenames_short <- list.files(
  path = "S:/HIPAA Compliance/Hospital Discharge Data/HDD_Updated-05042021/",
  recursive = TRUE,
  pattern = "coconino.*?\\.csv",
  full.names = FALSE
)

# write function
func_read <- function(x, y) { # x = filename for sas; y = object name
  tmp <- read_csv(x) %>%
    clean_names() %>%
    mutate(across(.cols = everything(), as.character)) %>%
    mutate(filename = as.character(x)) # create new variable of file name
  assign(y, tmp, envir = .GlobalEnv) # create data objects
}

# iteratively call function for each desired file
map2(
  .x = filenames,
  .y = filenames_short,
  .f = ~ func_read(.x, .y)
)

# inspect; which data objects were created?
ls(pattern = "coconino_")

# combine to one data set
hdd_df <- mget(ls(pattern = "coconino_")) %>%
  bind_rows()

# inspect
glimpse(hdd_df)

# take a sample and check for missing data
hdd_df %>%
  sample_n(size = 1000) %>%
  vis_dat()

# dates for pin meta data
# pin name
p_name <- str_c(
  "hdd-raw-",
  min(year(ymd(hdd_df$admission_date))),
  "-",
  max(year(ymd(hdd_df$admission_date)))
)

# pin title
p_title <- str_c(
  "AZDHS Hospital Discharge Data, raw (",
  min(year(ymd(hdd_df$admission_date))),
  "-",
  max(year(ymd(hdd_df$admission_date))),
  ")"
)

# pin description
p_description <- p_title

# write mortality data to pin board ####
hdd_pb %>%
  pin_write(
    x = hdd_df,
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

# check work
hdd_pb %>%
  pin_meta(p_name)

# view list of pins saved to folder
hdd_pb %>%
  pin_list()

