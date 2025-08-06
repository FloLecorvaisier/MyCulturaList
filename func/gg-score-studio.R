gg_score_studio <- function(df_all, font_plot, filters, switch) {
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
  mean_score_studio <- tapply(df_studio$score[df_studio$type %in% filters$Type
                                              & df_studio$year %in% filters$minYear:filters$maxYear
                                              & df_studio$studio %in% studios_to_print], 
                              df_studio$studio[df_studio$type %in% filters$Type
                                               & df_studio$year %in% filters$minYear:filters$maxYear
                                               & df_studio$studio %in% studios_to_print], 
                              mean)
  mean_score_studio <- round(mean_score_studio, 2)
  
  df_studio_filtered <- df_studio[df_studio$type %in% filters$Type
                                  & df_studio$year %in% filters$minYear:filters$maxYear
                                  & df_studio$studio %in% studios_to_print, ]
  
  gg <- ggplot(df_studio_filtered) +
    geom_boxplot_interactive(aes(x = score, y = factor(studio, levels = rev(studios_to_print))), fill = "darkolivegreen4") +
    annotate("label", 
             x = mean_score_studio, 
             y = factor(names(mean_score_studio), levels = rev(studios_to_print)),
             label = mean_score_studio, 
             fill = "darkolivegreen2") +
    labs(x = element_blank(), y = element_blank()) +
    ggtitle("Score per studio") +
    scale_x_continuous(breaks = 1:10) +
    coord_cartesian(xlim = c(1, 10)) +
    theme_minimal(base_family = font_plot, base_size = 12)
  gir <- girafe(ggobj = gg, options = list(opts_selection(type = "single"), opts_sizing(rescale = TRUE)))
  return(gir)
}