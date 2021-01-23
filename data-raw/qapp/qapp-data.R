library(tidyverse)
library(readxl)
library(lubridate)

# Value encoding:
# Non-detects: -9999

# what are all the units that exist on the files?
excel_files <- list.files("data-raw/qapp/", pattern = ".xls", full.names = TRUE)

excel_files %>%
  map_df(function(f) {
    read_excel(path = f) %>%
      distinct(UNITS)
  }) %>%
  distinct(UNITS)


excel_files %>%
  map_df(function(f) {
    read_excel(path = f, sheet = "SAMPDATA") %>%
      distinct(ANALYTE)
  }) %>%
  distinct(ANALYTE)


# 2008 ------------
may_2008 <- read_excel("data-raw/qapp/08_05_01(1).xls") %>%
  distinct(UNITS)
