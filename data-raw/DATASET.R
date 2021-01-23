## code to prepare `DATASET` dataset goes here
library(tidyverse)


transducers <-
  structure(list(serial_no = c("440478", "440965", "440966"),
                 device_type = c("Rugged BaroTROLL",
                                 "Rugged TROLL 200", "Rugged TROLL 200"),
                 device_site = c("Dry Creek Rancheria",
                                 "Dry Creek Rancheria", "Dry Creek Rancheria"),
                 device_name = c("DCR Baro",
                                 "DCR ED1", "DCR VR1"), firmware_version = c("1.01", "1.02", "1.02"
                                 ), reports = c("A", "TS", "TS")),
            row.names = c(NA, -3L), class = c("tbl_df",
                                              "tbl", "data.frame"))

usethis::use_data(transducers, overwrite = TRUE)

transducer_subsets_to_keep <- tibble(
  datetime = seq(as_datetime("2016-01-01 00:00:00"),
                 as_datetime("2022-12-31 23:45:00"), by = "2 hours")
)

usethis::use_data(transducer_subsets_to_keep, overwrite = TRUE)

# MEASUREMENT UNITS -----------------------------------------------------------------------
# measurement unit conversions from lab data to wqx
# the process for creating this dataset is manual by looking
# the reported units and finding the appropriate entry on the excel file
# from the wqx.
measuremnt_units_lookup <- read_csv("data-raw/domain-values/measurement-units-lookup.csv")
usethis::use_data(measuremnt_units_lookup, overwrite = TRUE)


# CHARACTERISTIC NAMES -------------------------------------------------
characteristic_names_lookup <- read_csv("data-raw/domain-values/analyte-names-lookup.csv") %>%
  filter(use_in_db)

usethis::use_data(characteristic_names_lookup, overwrite = TRUE)

# CHARACTERISTIC ATTRIBUTES ----------------------------------------------
characteristic_attributes <- read_csv("data-raw/domain-values/characteristic-attributes.csv")

usethis::use_data(characteristic_attributes, overwrite = TRUE)


# METHOD ID AND CONTEXT
method_id_and_context <- read_csv("data-raw/domain-values/dcr-method-codes-lookup.csv")

usethis::use_data(method_id_and_context, overwrite = TRUE)






