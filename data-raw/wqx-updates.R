# scripts updates wqx data from database
library(tidyverse)
library(dcrWQX)
library(lubridate)
library(gt)
# update database
con <- dcrWQX::connect()

# BARO ---------------------------------------------------------
# baro only reports air temperature
# 440478

baro_air <- wqx_csv_results(con,
                     location = "Baro1",
                     characteristic = "Temperature, air",
                     file_path = "../DATA/physical-results/baro1-air-temp_results.csv")

# validate the data a bit
baro_air %>%
  select(`Activity Start Date`, `Activity Start Time`, `Result Value`)

baro_air %>%
  group_by(
    location_id = `Monitoring Location ID`,
    chars_name = `Characteristic Name`) %>%
  summarise(
    total = n(),
    start_date = min(as_date(`Activity Start Date`)),
    end_date = max(as_date(`Activity Start Date`))
  ) %>% ungroup()

baro_air %>%
  ggplot(aes(as_date(`Activity Start Date`), `Result Value`)) +
  geom_point(alpha = 0.1)


# EPO1 ------------------------------------------------------------

# 440965
epo_stream <- wqx_csv_results(con,
                            location = "EPO1",
                            characteristic = "Stream Stage",
                            file_path = "../DATA/physical-results/epo1-stream-stage_results.csv")


epo_stream %>%
  ggplot(aes(as_date(`Activity Start Date`), `Result Value`)) +
  geom_point(alpha=0.1)


# VY1 -----------------------------------------------------------

# 440966









