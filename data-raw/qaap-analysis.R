# check how many different units are being reported for
# each of the analytes

library(readxl)
library(tidyverse)

oct_31_2008 <- read_excel("../raw-data/qapp/08_10_31.xls")
x <- read_excel("../raw-data/qapp/11_10_13.xls")

qapp_book <- list.files("../raw-data/qapp/",
                        pattern = "^[0-9]",
                        full.names = TRUE)

raw_data <- map(qapp_book, ~read_excel(path = .))

units_in_all_datasets <- raw_data %>%
  imap_dfr(function(x, y) {
    x %>%
      distinct(ANALYTE, UNITS) %>%
      mutate(source = y)
  })

# What are all the method codes used?
all_files <- list.files("data-raw/qapp/", pattern = ".xl", full.names = TRUE)

all_codes <- all_files %>%
  map_df(function(f) {
    read_excel(f) %>%
      distinct(METHODNAME)
  }) %>%
  distinct(METHODNAME)

analytes_multiple_units <- units_in_all_datasets %>%
  group_by(ANALYTE) %>%
  summarise(
    total_units_reported = n_distinct(UNITS)
  ) %>%
  filter(total_units_reported > 1) %>%
  pull(ANALYTE)

# ML just needs to be placed in the correct part of the string
units_in_all_datasets %>%
  filter(ANALYTE %in% analytes_multiple_units[1])

units_in_all_datasets %>%
  filter(ANALYTE %in% analytes_multiple_units[2])

units_in_all_datasets %>%
  filter(ANALYTE %in% analytes_multiple_units[3])


# EXISTING DATA
prelim_data <- read_xlsx("data-raw/existing-preliminary-data.xlsx")

# what are all the analytes that are reported
prelim_data %>%
  distinct(Characteristic)

# what are all the sites that are reported?
prelim_data %>%
  distinct(`Monitoring Location ID`)

# what are the date ranges
prelim_data %>%
  group_by(`Monitoring Location ID`) %>%
  summarise(
    start_date = min(`Activity Start Date`),
    end_date = max(`Activity Start Date`)
  )
