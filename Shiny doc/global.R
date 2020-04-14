pacman::p_load(tidyverse, rio, data.table, 
               tidycovid19, 
               flexdashboard, shinyWidgets)

source(file = "Fonctions.R", encoding = 'UTF-8')

Panel_data <- download_merged_data(cached = TRUE)

Long_data =
  Panel_data %>%
  mutate(date = lubridate::ymd(date)) %>%
  select(iso3c, country, date, confirmed, deaths, recovered) %>%
  pivot_longer(
    cols = c("confirmed", "deaths", "recovered"),
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
  group_by(country) %>% filter(confirmed==max(confirmed)) %>% 
  ungroup %>% arrange(-confirmed) %>% pull(iso3c) %>% unique()

pacman::p_load(EpiEstim)
Incidences =
  Daily_data %>%
  drop_na(Daily_cases) %>%
  filter(Daily_cases > 0) %>% 
  count(country) %>% filter(n >= 7) %>%
  semi_join(x = Daily_data, 
            by = "country") %>%
  split(f = .$iso3c) %>%
  map(.f = Calcul_incidence)

estimateR =
  Incidences %>%
  map(.f = EpiEstim::estimate_R,
      method = "parametric_si",
      config = make_config(list(mean_si = 5,
                                std_si = 3.4)))