baro <- read_transducer("data-raw/2020 Pressure Trans Data/raw/DCR Baro 15 min 2016_Append_2020-07-09_16-29-49-081.csv")$data
ed1 <- read_transducer("data-raw/2020 Pressure Trans Data/raw/DCR ED 1 15 min 2016_Append_2020-07-09_16-37-17-473-BaroMerge.csv")$data
vineyard <- read_transducer("data-raw/2020 Pressure Trans Data/raw/DCR Vineyard 1 15 min 2016_Append_2020-07-09_16-13-38-308-BaroMerge.csv")$data


all_data <- bind_rows(
  baro,
  ed1,
  vineyard
)


data_2020 <- all_data %>% filter(`Activity Start Date` > "2019-12-20")
data_2020 %>% pull(`Activity Start Date`) %>% range()

write_csv(data_2020, "data-raw/2020 Pressure Trans Data/raw/2020-transducer.csv", na = "")

data_2020 %>% distinct(`Monitoring Location ID`)
