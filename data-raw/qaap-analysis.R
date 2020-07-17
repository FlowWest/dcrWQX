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

raw_data %>%
  imap_df(function(x, y) {
    x %>%
      distinct(ANALYTE, UNITS) %>%
      mutate(source = y)
  })
