TEST =
  TEST %>%
  group_by(country) %>%
  mutate(Daily_cases = Number - lag(Number)) %>% ungroup %>%
  drop_na(Daily_cases) %>% 
  filter(Daily_cases>0)

TEST  %>%
  filter(Daily_cases> 200) %>%
  count(country) %>%
  top_n(n = 12, wt = n) %>%
  semi_join(x = TEST, by = "country") %>%
  # filter(str_detect(string = country, pattern = "China", negate = T)) %>% 
  # mutate_at(.vars = vars(Number, Daily_cases), .funs = ~./Population) %>% 
  ggplot(mapping = aes(
    x = Number,
    y = Daily_cases,
    colour = country,
    group = country
  )) +
  geom_smooth(se = F) +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = "bottom") +
  xlab(label = "Total confirmed cases") +
  ylab(label = "New confirmed cases")+
  facet_wrap(facets = ~region)
