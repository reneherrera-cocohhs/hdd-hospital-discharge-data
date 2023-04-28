# Prepare the pin board for use

# Setup ####
# load package libraries ####
library(here) # project oriented workflow
library(pins) # data access
library(tidyverse)

# Hospital discharge data ####
hdd_pb <- board_folder(
  path = "S:/HIPAA Compliance/Hospital Discharge Data/r-pin-board-rds-files/"
)

hdd_pb %>%
  pin_list()




# # set pin board folder
# hdd_data_folder <- board_folder(
#   path = "S:/HIPAA Compliance/Hospital Discharge Data/r-pin-board-rds-files"
# )
#
# # view pins on board
# pin_list(
#   board = hdd_data_folder
# )
#
#
# hdd_data <- pin_read(
#   board = hdd_data_folder,
#   name = "hdd_data"
# )
#
# glimpse(hdd_data)
#
#
# azdhs_med_list <- read_rds(
#   "../data/data-tidy/azdhs-medical-facility-listing.rds"
# )
#
# glimpse(azdhs_med_list)
#
#
# hdd_data %>%
#   filter(
#     birth_date == "",
#     sex == "",
#     str_detect(
#       pt_name, ""
#     )
#   ) %>%
#   left_join(
#     x = .,
#     y = azdhs_med_list,
#     by = c("az_fac_id" = "facid")
#   ) %>%
#   glimpse()
