TEST =
  TEST %>%
  group_by(country) %>%
  mutate(Daily_cases = Number - lag(Number)) %>% ungroup %>%
  drop_na(Daily_cases) %>%
  filter(Daily_cases > 0)


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

Incidences %>%
  map(R0) %>%
  bind_rows(.id = "country") %>%
  # filter(country %in% Countries) %>%
  ggplot(mapping = aes(x = reorder(country, desc(R0)))) +
  geom_linerange(mapping = aes(ymin = `2.5 %`,
                               ymax = `97.5 %`)) +
  ylim(0, NA) +
  coord_flip()

estimateR =
  Incidences %>%
  map(.f = EpiEstim::estimate_R,
      method = "parametric_si",
      config = make_config(list(mean_si = 5,
                                std_si = 3.4)))

estimateR$ITA %>% plot


estimate_R0 =
  estimateR %>%
  map(.f = Get_weekly_R0) %>%
  bind_rows(.id = "country") %>%
  mutate(Date = ymd(Date))

PLOT =
  estimate_R0 %>%
  filter(`Std(R)` < 0.05) %>%
  distinct(country, Date) %>%
  semi_join(x = estimate_R0) %>%
  ggplot(mapping = aes(x = Date, colour = country)) +
  geom_line(mapping = aes(y = `Mean(R)`)) +
  geom_errorbar(mapping = aes(ymin = `Quantile.0.025(R)`,
                              ymax = `Quantile.0.975(R)`)) +
  theme(legend.position = "none")

plotly::ggplotly(PLOT)

PlotIncidence(incidence = Incidences$US)

estimate_R0 %>%
  filter(country %in% c("France",
                        "Portugal",
                        "Korea, South",
                        "United Kingdom")) %>%
  ggplot(mapping = aes(x = Date, colour = country)) +
  geom_line(mapping = aes(y = `Mean(R)`)) +
  # geom_errorbar(mapping = aes(ymin = `Quantile.0.025(R)`,
  #                             ymax = `Quantile.0.975(R)`))+
  geom_hline(yintercept = 1,
             linetype = "dashed",
             colour = "red") +
  scale_y_log10() +
  theme(legend.position = "bottom")

PLOT =
  estimate_R0 %>%
  group_by(country) %>%
  filter(`Mean(R)` == max(`Mean(R)`)) %>% ungroup %>%
  filter(Date <= "2020-02-28") %>%
  distinct(country) %>%
  semi_join(x = estimate_R0) %>%
  ggplot(mapping = aes(x = Date, colour = country)) +
  geom_smooth(mapping = aes(y = `Mean(R)`), se = F) +
  # geom_errorbar(mapping = aes(ymin = `Quantile.0.025(R)`,
  #                             ymax = `Quantile.0.975(R)`))+
  geom_hline(yintercept = 1,
             linetype = "dashed",
             colour = "red") +
  scale_y_log10() +
  theme(legend.position = "bottom")
plotly::ggplotly(PLOT)
