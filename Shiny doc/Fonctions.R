Deuxieme_vague <- function(DATA) {
  PEAK =
    DATA %>% 
    filter(Type=="deaths") %>% 
    mutate(Mobile = zoo::rollmean(x = Daily_cases,
                                  k = 14, na.pad = T)) %>%
    filter(Mobile == max(Mobile, na.rm = T)) %>%
    pull(date)
  
  if (length(PEAK)==0) {
    PEAK = lubridate::today()
  }
  
  Origine =
    DATA %>%
    filter(Type=="confirmed") %>% 
    mutate(Mobile = zoo::rollmean(x = Daily_cases,
                                  k = 14, na.pad = T)) %>%
    filter(date > PEAK) %>%
    filter(Mobile == min(Mobile, na.rm = T)) %>%
    pull(date)
  
  if (length(Origine) == 0) {
    DATA = DATA
  } else {
    DATA = DATA %>% filter(date>=Origine)
    }
  
  DATA %>%
    filter(Type=="confirmed") %>% 
    return()
}

Calcul_incidence <- function(FRANCE) {
  require(lubridate)
  require(incidence)
  Dates = FRANCE %>% pull(date)
  REPS = FRANCE %>% pull(Daily_cases)
  France_list =
    pmap(.f = base::rep,
         .l = list(x = Dates,
                   times = REPS)) %>%
    map(as.character) %>% flatten_chr() %>% lubridate::ymd()
  France_incidence =  incidence(France_list)
  return(France_incidence)
}

is_SPLIT <- function(incidence) {
  SPLIT = find_peak(x = incidence)
  if(as.duration(SPLIT %--% lubridate::today())>ddays(2))
    return(SPLIT) else
      return(NULL)
}

PlotIncidence = function(incidence) {
  plot(x = incidence,
       fit=fit(x = incidence, 
               split = is_SPLIT(incidence = incidence)))
}

R0 <- function(incidence) {
  FIT = fit(incidence)
  cbind.data.frame(R0 = FIT$info$r, FIT$info$r.conf) %>%
    return()
}

Get_weekly_R0 <- function(estimateR) {
  list(
    estimateR$R %>%
      mutate(Moyenne = (t_start + t_end) / 2),
    estimateR$dates %>% data.frame(Date = .) %>%
      rowid_to_column(var = "Moyenne")
  ) %>%
    reduce(.f = inner_join, by = "Moyenne") %>%
    return()
}

PLOT_rel_abs = function(plot){
  plot %>%
    mutate(Morts_relatif = Number / population) %>%
    rename(Morts_absolu = Number) %>%
    pivot_longer(names_to = "Relatif_absolu",
                 values_to = "Valeur",
                 cols = starts_with("Morts")) %>%
    drop_na(Valeur) %>%
    ggplot(mapping = aes(x = date,
                         y = Valeur,
                         colour = country)) +
    geom_line() +
    facet_grid(facets = Relatif_absolu ~ ., scales = "free") +
    theme(legend.position = "bottom")
}

