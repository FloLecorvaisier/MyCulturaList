gg_n_studio <- function(df_all, font_plot, filters, switch) {
  df_studio <- data.frame("studio" = unlist(strsplit(paste(df_all$studio, collapse = "|"), "\\|")))
  df_studio$title <- rep(df_all$title, times = lengths(regmatches(df_all$studio, gregexpr("\\|", df_all$studio))) + 1) ## To add the title for each
  
  ## Needed for filtering
  df_studio$type <- rep(df_all$type, times = lengths(regmatches(df_all$studio, gregexpr("\\|", df_all$studio))) + 1) ## To add the type for each
  df_studio$year <- rep(df_all$year, times = lengths(regmatches(df_all$studio, gregexpr("\\|", df_all$studio))) + 1) ## To add the year for each
  df_studio$score <- rep(df_all$score, times = lengths(regmatches(df_all$studio, gregexpr("\\|", df_all$studio))) + 1) ## To add the year for each
  
  table_studio <- as.data.frame(with(df_studio, table(type, year, studio)))
  
  studios_to_print <- names(sort(with(table_studio[table_studio$type %in% filters$Type
                                                   & table_studio$year %in% filters$minYear:filters$maxYear, ],
                                      tapply(Freq, studio, sum)), decreasing = T))[1:min(10, length(unique(df_studio$studio)))]
  tb_studio_filtered <- table_studio[table_studio$type %in% filters$Type
                                     & table_studio$year %in% filters$minYear:filters$maxYear
                                     & table_studio$studio %in% studios_to_print, ] |> 
    group_by(studio) |>
    summarise(Freq = sum(Freq))
  tb_studio_filtered2 <- rbind(tb_studio_filtered, data.frame(studio = "", Freq = 0)) ## For polar coord version
  if (!switch) {
    gg <- ggplot(tb_studio_filtered) +
      geom_col_interactive(aes(x = Freq , y = factor(studio, levels = rev(studios_to_print)), tooltip = Freq, data_id = studio), fill = "darkolivegreen4") +
      labs(x = element_blank(), y = element_blank()) +
      ggtitle("Number of animes watched per studio") +
      theme_minimal(base_family = font_plot, base_size = 12) +
      theme()
  } else {
    gg <- ggplot(tb_studio_filtered2) +
      geom_hline(yintercept = seq(0, 5 * ceiling((max(tb_studio_filtered$Freq) + 1) / 5), length.out = 6), color = "gray80") +
      geom_segment(x = 1:nrow(tb_studio_filtered2), y = 0, yend = 5 * ceiling((max(tb_studio_filtered$Freq) + 1) / 5), color = "gray80") +
      geom_col_interactive(aes(y = Freq , x = factor(studio, levels = rev(c("", studios_to_print))), tooltip = Freq, data_id = studio), 
                           fill = "darkolivegreen4") +
      annotate("label",
               x = 11,
               y = seq(0,5 * ceiling((max(tb_studio_filtered$Freq) + 1) / 5), length.out = 6),
               label = seq(0,5 * ceiling((max(tb_studio_filtered$Freq) + 1) / 5), length.out = 6),
               color = "gray30", label.size = 0) +
      labs(x = element_blank(), y = element_blank()) +
      ggtitle("Number of animes watched per studio") +
      ylim(-5 * ceiling((max(tb_studio_filtered$Freq) + 1) / 5 ** 2), 5 * ceiling((max(tb_studio_filtered$Freq) + 1) / 5)) +
      theme_minimal(base_family = font_plot, base_size = 12) +
      coord_polar(start = pi / nrow(tb_studio_filtered2)) +
      theme(axis.text.y = element_blank(),
            axis.text.x = element_text(hjust = 1, 
                                       angle = seq(360 - 360 / nrow(tb_studio_filtered2), 0, -360 / nrow(tb_studio_filtered2))),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white", color = "white"),
            panel.grid.major.y = element_blank())
  }
  gir <- girafe(ggobj = gg, options = list(opts_sizing(rescale = TRUE)))
  return(gir)
}