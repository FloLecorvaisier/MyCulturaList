gg_n_season <- function(df_all, font_plot, filters) {
  table_season <- as.data.frame(with(df_all, table(type, year, season)))
  table_season$year <- as.numeric(as.character(table_season$year))
  table_season$season <- factor(table_season$season, levels = c("winter", "spring", "summer", "fall"))
  tb_season_filtered <- table_season[table_season$type %in% filters$Type
                                     & table_season$year %in% filters$minYear:filters$maxYear, ] |> 
    group_by(season, year) |>
    summarise(Freq = sum(Freq))
  gg <- ggplot(tb_season_filtered) +
    geom_col_interactive(aes(x = year, y = Freq, fill = season, tooltip = Freq, data_id = paste0(season, year))) +
    scale_fill_manual(values = c("lightblue1", "olivedrab2", "orangered1", "brown"),
                      labels = c("Winter", "Spring", "Summer", "Fall")) +
    scale_x_continuous(breaks = seq(1910, ceiling(year(today()) / 10) * 10, 10)) +
    # scale_y_continuous(limits = c(0, max(tapply(table_season$Freq, table_season$year, sum))), breaks = seq(0, 50, 5)) +
    labs(x = element_blank(), y = element_blank(), fill = element_blank()) +
    ggtitle("Number of animes watched per season of first diffusion") +
    theme_minimal(base_family = font_plot, base_size = 10) +
    theme(legend.position = "bottom")
  gir <- girafe(ggobj = gg, options = list(opts_sizing(rescale = TRUE)))
  return(gir)
}




