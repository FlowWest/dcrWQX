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
