pacman::p_load(EpiEstim, magrittr)
Incidences =
  Daily_data %>%
  split(f = .$iso3c) %>% 
  map(.f = Calcul_incidence)

get_estimate = ~EpiEstim::estimate_R(incid = .,
                                     method = "parametric_si",
                                     config = make_config(list(mean_si = 3.96,
                                                               std_si = 4.75)))

estimateR =
  Incidences %>%
  map(.f = safely(get_estimate))

estimateR = estimateR %>% purrr::transpose() %$% result %>% compact()%>%
  map(.f = Get_weekly_R0) %>%
  bind_rows(.id = "iso3c") %>%
  left_join(y = Daily_data %>% distinct(iso3c, country),
            by = "iso3c")

PLOTT =
  Panel_data %>% 
  filter(iso3c=="FRA") %>% distinct(region, income) %>% 
  semi_join(x= Panel_data) %>% distinct(iso3c) %>% 
  semi_join(x=estimateR)  %>% 
  ggplot(mapping = aes(x = Date, y = `Median(R)`)) +
  geom_line(mapping = aes(colour = country)) +
  geom_hline(yintercept = 1, colour = "red")

plotly::ggplotly(dynamicTicks = T, p = PLOTT)
