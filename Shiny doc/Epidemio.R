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
