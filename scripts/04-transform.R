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
