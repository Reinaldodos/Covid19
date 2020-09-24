pacman::p_load(tidyverse, rio, data.table, 
               tidycovid19, 
               flexdashboard, shinyWidgets)

source(file = "Fonctions.R", encoding = 'UTF-8')

Panel_data <- download_merged_data(cached = TRUE)

Long_data =
  Panel_data %>%
  mutate(date = lubridate::ymd(date),
         infected = confirmed-deaths-recovered) %>% 
  select(iso3c, country, date, confirmed, deaths, recovered, infected) %>%
  pivot_longer(
    cols = c("confirmed", "deaths", "recovered", "infected"),
    names_to = "Type",
    values_to = "Number"
  ) 

Daily_data = 
  Long_data %>%
  group_by(country, Type) %>%
  mutate(Daily_cases = Number - lag(Number)) %>% ungroup %>%
  drop_na(Daily_cases) %>% 
  filter(Daily_cases>0)

Countries = 
  Panel_data %>%
  group_by(country) %>% filter(confirmed==max(confirmed, na.rm = T)) %>% 
  ungroup %>% arrange(iso3c) %>% pull(iso3c) %>% unique()

