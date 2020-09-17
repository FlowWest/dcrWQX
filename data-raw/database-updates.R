# this scripts reads raw data and updates the database
library(dcrWQX)
library(tidyverse)
# read in the new data
baro1 <- dcrWQX::read_transducer("../raw-data/2019-year-end/DCR Baro 15 min 2016_2019-12-20_16-02-24-219.csv")$data
vy1 <- dcrWQX::read_transducer("../raw-data/2019-year-end/DCR Vineyard 1 15 min 2016_2019-12-20_16-46-19-723-BaroMerge.csv")$data
epo1 <- read_transducer("../raw-data/2019-year-end/DCR ED 1 15 min 2016_Append_2019-12-20_16-04-29-152-BaroMerge.csv")$data


# plots
vy1 %>% ggplot(aes(`Analysis Start Date`, `Result Value`)) + geom_point()
epo1 %>% ggplot(aes(`Analysis Start Date`, `Result Value`)) + geom_point()
epo1 %>% View()


# update database
con <- dcrWQX::connect()


dcrWQX::db_append_data(db_connection = con,
                       table_name = "results",
                       data = baro1)

dcrWQX::db_append_data(db_connection = con,
                       table_name = "results",
                       data = vy1)

dcrWQX::db_append_data(db_connection = con,
                       table_name = "results",
                       data = epo1)

# check the results
con %>%
  tbl("results") %>%
  head() %>%
  collect() %>%
  View()
