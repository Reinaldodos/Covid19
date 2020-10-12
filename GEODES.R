pacman::p_load(rio, tidyverse, data.table, lubridate, zoo)
PLOTT = function(output, VAR) {
  graphe =
    output %>%
    ggplot(mapping = aes(x = jour, y = {
      {
        VAR
      }
    }, colour = Libellé))
  
  return(graphe)
}
SEUILS <- function(graphe) {
  graphe =
    graphe +
    geom_hline(yintercept = .4,
               colour = "green",
               linetype = "dashed") +
    geom_hline(yintercept = .6,
               colour = "red",
               linetype = "dashed")
  return(graphe)
}
HISTO_GRAPHE <- function(graphe) {
  graphe =
    graphe +
    geom_bar(stat = "identity") +
    theme(legend.position = "none")
  return(graphe)
}



Capacites = "./Capacités hospitalières.xlsx" %>% rio::import()

input =
  "https://www.data.gouv.fr/en/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7" %>%
  rio::import(format = "csv") %>%
  group_by(dep, jour) %>%
  summarise(Rea = sum(rea, na.rm = T), .groups = "drop") 

output =
  inner_join(x = input,
             y = Capacites,
             by = "dep") %>%
  mutate_at(.vars = vars(dep, Libellé),
            .funs = as.factor) %>%
  mutate(Occupation = Mobile / Lits)

# gérer les merdouilles sur les dates
date_ymd = 
  output %>% 
  mutate_at(.vars = "jour", .funs = lubridate::ymd) %>% 
  drop_na(jour)

date_dmy = 
  output %>% 
  filter(str_detect(string = jour, pattern = "/")) %>% 
  mutate_at(.vars = "jour", .funs = lubridate::dmy) %>% 
  drop_na(jour)

output = bind_rows(date_ymd, date_dmy)%>%
  mutate(Mobile = zoo::rollmean(
    x = Rea,
    k = 7,
    na.pad = T,
    align = "right"
  ))



Departements = 
  output %>%
  ggplot(mapping = aes(x = jour, y = Mobile)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ Libellé, scales = "free_y")

Vague =
  output %>%
  PLOTT(VAR = Occupation) %>% SEUILS() +
  geom_smooth(se = F)

BdR =
  output %>%
  filter(dep == "13",
         jour == max(jour, na.rm = T)) %>% pull(Lits)

Paris =
  output %>%
  filter(dep == "75",
         jour == max(jour, na.rm = T)) %>% pull(Occupation)

Big_Wave =
  output %>%
  filter(Lits >= BdR / 2,
         jour == max(jour, na.rm = T)) %>% 
  distinct(dep) %>% semi_join(x = output, by = "dep")

Big_Wave %>%
  PLOTT(VAR = Occupation) %>%
  SEUILS() %>%
  HISTO_GRAPHE() +
  facet_wrap( ~ Libellé)+
  xlim(dmy(01072020, NA))

Big_Wave %>%
  PLOTT(VAR = Mobile) +
  geom_line() +
  scale_y_log10() 

plotly::ggplotly(dynamicTicks = T, p = Vague)
