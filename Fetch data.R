pacman::p_load(tidyverse, rio, data.table)

Deaths = 
  # "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv" %>%
"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" %>%
  import() %>%
  pivot_longer(
    names_to = "Date",
    values_to = "Number",
    values_drop_na = T,
    cols = contains(match = "/20")
  ) %>%
  # filter(Number>0) %>%  
  mutate_at(.vars = "Date", .funs = lubridate::mdy) 

Deaths_country = 
  Deaths %>% 
  group_by(`Country/Region`, Date) %>% 
  summarise(Number = sum(Number)) %>% ungroup

source(file = "World Bank data.R")

TEST = 
  inner_join(x = Population,
          y = Deaths_country,
          by = c("country" = "Country/Region"))
