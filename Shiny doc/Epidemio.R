pacman::p_load(EpiEstim, magrittr)
Incidences =
  Daily_data %>%
  split(f = .$iso3c) %>% 
  map(.f = Deuxieme_vague) %>%
  map(.f = Calcul_incidence)

estimateR =
  Incidences %>%
  map(.f = EpiEstim::estimate_R,
      method = "parametric_si",
      config = make_config(list(mean_si = 5,
                                std_si = 3.4)))

Dates=
estimateR$FRA$dates %>% lubridate::as_date() %>%  
  data.frame(Date=.) %>% 
  rowid_to_column()

estimateR$FRA$R %>% 
  gather(key = start_end, value = rowid, starts_with("t_")) %>% 
  left_join(y=Dates, by = "rowid") %>% 
  select(-rowid) %>% spread(key = start_end, value = Date) %>% 
  select(starts_with("t_"), everything()) %>% 
  view()