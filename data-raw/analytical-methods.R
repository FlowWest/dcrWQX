library(tidyverse)
library(readxl)

dcr_methods <- read_csv("data-raw/domain-values/all-dcr-methods.csv", trim_ws = TRUE)
wqx_method_context <- read_excel("data-raw/domain-values/ANALYTICAL_METHOD_CONTEXT.xlsx", trim_ws = TRUE)
wqx_method_ids <- read_excel("data-raw/domain-values/ANALYTICAL_METHOD.xlsx", trim_ws = TRUE)


wqx_methods <- wqx_method_ids %>%
  left_join(wqx_method_context, by=c("Context Code" = "Code")) %>%
  filter(!is.na(UID.y)) %>%
  arrange(ID) %>%
  select(
    id = ID,
    context_code = `Context Code`,
    description = Description,
    id_name = Name.x,
    context_name = Name.y
  ) %>%
  filter(context_code != "WQXTEST")

write_csv(wqx_methods, "data-raw/domain-values/wqx-methods.csv")


wqx_methods %>% filter(id == "8260B")
wqx_methods %>% filter(id == "200.7")
wqx_methods %>% filter(id == "2540C")
wqx_methods %>% filter(id == "200.7")
