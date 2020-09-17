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
  select(
    `Activity Start Date`,
    `Activity Start Time`,
    `Analysis Start Time`,
    `Analysis Start Time Zone`,
    `Result Value`
  ) %>%
  glimpse()

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
# 440965 TS

# water temperature
epo_water <- wqx_csv_results(con,
                             location = "EPO1",
                             characteristic = "Temperature, water",
                             file_path = "../DATA/physical-results/epo1-water-temp_results.csv")

epo_water %>%
  select(
    `Activity Start Date`,
    `Activity Start Time`,
    `Analysis Start Time`,
    `Analysis Start Time Zone`,
    `Result Value`,
    `Result Unit`
  ) %>%
  glimpse()

epo_water %>%
  group_by(
    location_id = `Monitoring Location ID`,
    chars_name = `Characteristic Name`) %>%
  summarise(
    total = n(),
    start_date = min(as_date(`Activity Start Date`)),
    end_date = max(as_date(`Activity Start Date`))
  ) %>% ungroup()

epo_water %>%
  ggplot(aes(as_date(`Activity Start Date`), `Result Value`)) +
  geom_point(alpha = 0.1)


# VY1 -----------------------------------------------------------

# water temp
vy1_water <- wqx_csv_results(con,
                             location = "VY1",
                             characteristic = "Temperature, water",
                             file_path = "../DATA/physical-results/vy1-water-temp_results.csv")

vy1_water %>%
  select(`Activity Start Date`, `Activity Start Time`, `Result Value`)

vy1_water %>%
  group_by(
    location_id = `Monitoring Location ID`,
    chars_name = `Characteristic Name`) %>%
  summarise(
    total = n(),
    start_date = min(as_date(`Activity Start Date`)),
    end_date = max(as_date(`Activity Start Date`))
  ) %>% ungroup()

vy1_water %>%
  ggplot(aes(x = as_date(`Activity Start Date`), y = `Result Value`)) +
  geom_point(alpha = 0.1)

