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
