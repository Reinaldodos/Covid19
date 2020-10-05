pacman::p_load(EpiEstim, magrittr)
Incidences =
  Daily_data %>%
  split(f = .$iso3c) %>% 
  map(.f = Calcul_incidence)

estimateR =
  Incidences %>%
  map(.f = EpiEstim::estimate_R,
      method = "parametric_si",
      config = make_config(list(mean_si = 5.71,
                                std_si = 3.89)))

Incidences_current_wave =
  Daily_data %>%
  split(f = .$iso3c) %>% 
  map(.f = Deuxieme_vague) %>%
  map(.f = Calcul_incidence)
