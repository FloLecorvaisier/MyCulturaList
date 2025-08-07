gg_grid_popularity <- function(df_all, font_plot, switch) {
  
  ## Used in checkup
  # dfx <- df_all
  # df_all <- dfx 
  
  nsup10K <- sum(df_all$popularity > 1e4)
  
  ## Limit to 10K
  df_all <- df_all[df_all$popularity <= 1e4, ]
  
  df_all$xpop = (df_all$popularity - 1) %% 100
  df_all$ypop = - df_all$popularity %/% 100 + 1
  
  df_pop <- data.frame(x = 0:99,
                       y = rep(0:-99, each = 100),
                       val = 0)
  df_pop$val <- paste(df_pop$x, df_pop$y) %in% paste(df_all$xpop, df_all$ypop)
  df_pop$title[df_pop$val == T] <- df_all$title[order(df_all$popularity)][df_all$popularity <= 10000]
  df_pop$title[is.na(df_pop$title)] <- "" ## To prevent that hovering prints "NA"
  
  ## replacing "'" else data_id does not work
  df_pop$title2 <- gsub("'", "XXX", df_pop$title)
  
  df_pop$popularity[df_pop$val == T] <- df_all$popularity[order(df_all$popularity)]
  df_pop$popularity[is.na(df_pop$popularity)] <- "" ## To prevent that hovering prints "NA"
  
  ## Cutting the popularity scores into intervals
  inter = cut(df_all$popularity, seq(0, 1e4, 1e3))
  
  ## Data frame with proportions by intervals
  df_prop = data.frame(inter = paste0(1000 * 0:9 + 1, " - ", 1000 * 1:10),
                       prop_tot = as.numeric(table(inter) / 1000),
                       prop_rel = as.numeric(table(inter) / nrow(df_all)))
  
  ## y positions of the points/segments/labels in rel plot
  yval = cumsum(rev(df_prop$prop_rel)) - rev(df_prop$prop_rel) / 2
  
  ## Good ol' trigonometry for hjust of labels
  hjust = - sin(yval * 2 * pi) / 2 + .5
  
  if (switch == "grid") {
    gg <- ggplot(df_pop) +
      geom_tile_interactive(aes(x = x, y = y, fill = val, 
                                tooltip = paste0(title, ifelse(val == T, "\nRank: ", ""), popularity), 
                                data_id = title2), 
                            show.legend = F, color = "grey") +
      geom_hline(yintercept = seq(0, -100, -10) + .5) +
      annotate("text", x = -25, y = seq(-5, -95, -10), 
               label = paste0(1000 * 0:9 + 1, " - ", 1000 * 1:10),
               hjust = 0, vjust = 0, family = font_plot) +
      scale_fill_manual(values = c("transparent", "darkolivegreen4")) +
      # scale_fill_manual(values = c("darkolivegreen4")) +
      ggtitle("Completion grid of most popular animes",
              subtitle = paste0("Number of animes ranked > 10000: ", nsup10K)) +
      theme() +
      theme_void(base_family = font_plot)
  } else if (switch == "prop_tot") {
    gg <- ggplot(df_prop) +
      geom_col(aes(x = inter, y = prop_tot * 100), fill = "darkolivegreen4") +
      geom_hline(yintercept = 100, linetype = "dashed") +
      annotate("text", x = 1:10, y = df_prop$prop_tot * 100 + 3, 
               label = paste(format(df_prop$prop_tot * 100, digits = 1), "%"), family = font_plot) + 
      coord_cartesian(ylim = c(0, 100)) +
      labs(x = element_blank(), y = "Completion (%)") +
      ggtitle("Percentage of completion of popular animes") +
      theme_minimal(base_family = font_plot) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else if (switch == "prop_rel") {
    gg <- ggplot(df_prop) +
      geom_col(aes(x = 1, y = prop_rel, fill = inter), show.legend = T) +
      annotate("point", x = 1.2, y = yval[rev(df_prop$prop_rel > thrshld)]) +
      annotate("segment", x = 1.2, xend = 1.6, y = yval[rev(df_prop$prop_rel > thrshld)]) +
      annotate("text", x = 1.8, y = yval[rev(df_prop$prop_rel > thrshld)], 
               label = paste(format(rev(df_prop$prop_rel[df_prop$prop_rel > thrshld]) * 100, digits = 3), "%"),
               hjust = hjust[rev(df_prop$prop_rel) > thrshld],
               family = font_plot) +
      scale_fill_viridis_d(option = "E") +
      coord_polar(theta = "y") +
      xlim(c(-1, 1.8)) +
      labs(fill = element_blank()) +
      ggtitle("Relative distribution of watched animes per popularity rank") +
      theme_void(base_family = font_plot, base_size = 12) +
      theme()
  }
  gir <- girafe(ggobj = gg, options = list(opts_sizing(rescale = TRUE)))
  return(gir)
}
