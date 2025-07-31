gg_n_score <- function(df_all, font_plot, filters, switch) {
  table_score <- as.data.frame(with(df_all, table(type, year, score)))
  table_score$score <- as.numeric(as.character(table_score$score))
  tb_score_filtered <- table_score[table_score$type %in% filters$Type
                                   & table_score$year %in% filters$minYear:filters$maxYear, ] |> 
    group_by(score) |>
    summarise(Freq = sum(Freq))
  tb_score_filtered2 <- rbind(tb_score_filtered, data.frame(score = 0, Freq = 0)) ## For polar coord version
  if (!switch) {
    gg <- ggplot(tb_score_filtered) +
      geom_col_interactive(aes(x = score, y = Freq, tooltip = Freq, data_id = score), fill = "darkolivegreen4") +
      scale_x_continuous(breaks = 1:10, limits = c(.5, 10.5)) +
      labs(x = element_blank(), y = element_blank()) +
      ggtitle("Number of animes watched per personal score") +
      theme_minimal(base_family = font_plot, base_size = 12)
  } else {
    gg <- ggplot(tb_score_filtered2) +
      geom_hline(yintercept = seq(0, 5 * ceiling((max(tb_score_filtered$Freq) + 1) / 5), length.out = 6), color = "gray80") +
      annotate("segment", x = 0:10, y = 0, yend = 5 * ceiling((max(tb_score_filtered$Freq) + 1) / 5), color = "gray80") +
      geom_col_interactive(aes(y = Freq , x = score, tooltip = Freq, data_id = score), fill = "darkolivegreen4") +
      annotate("label", x = 0, y = seq(0, (5 * ceiling((max(tb_score_filtered$Freq) + 1) / 5)), length.out = 6), 
               label = seq(0, (5 * ceiling((max(tb_score_filtered$Freq)) / 5)), length.out = 6), 
               color = "gray30", label.size = 0) +
      scale_x_continuous(breaks = 1:10) +
      labs(x = element_blank(), y = element_blank()) +
      ggtitle("Number of animes watched per personal score") +
      ylim(- 5 * ceiling((max(tb_score_filtered$Freq) + 1) / 5 ** 2), 5 * ceiling((max(tb_score_filtered$Freq) + 1) / 5)) +
      theme_minimal(base_family = font_plot, base_size = 12) +
      coord_polar(start = - pi / 11) +
      theme(axis.text.y = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", color = "white"),
            panel.grid.major.y = element_blank())
  }
  gir <- girafe(ggobj = gg, options = list(opts_selection(type = "single"), opts_sizing(rescale = TRUE)))
}
