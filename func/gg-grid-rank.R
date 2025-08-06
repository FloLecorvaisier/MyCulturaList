gg_grid_rank <- function(df_all, font_plot) {
  
  # df_all <- dfx ## Used in checkup
  
  nsup10K <- sum(df_all$rank[!is.na(df_all$rank)] > 1e4)
  
  ## Because NSFW animes are excluded + limit to 10K
  df_all <- df_all[!is.na(df_all$rank) & df_all$rank <= 1e4, ]
  
  df_all$xrank = (df_all$rank - 1) %% 100
  df_all$yrank = - df_all$rank %/% 100 + 1
  
  df_rank <- data.frame(x = 0:99,
                        y = rep(0:-99, each = 100),
                        val = 0)
  df_rank$val <- paste(df_rank$x, df_rank$y) %in% paste(df_all$xrank, df_all$yrank)
  df_rank$title[df_rank$val == T] <- df_all$title[order(df_all$rank)][df_all$rank <= 10000]
  df_rank$title[is.na(df_rank$title)] <- "" ## To prevent that hovering prints "NA"
  
  ## replacing "'" else data_id does not work
  df_rank$title2 <- gsub("'", "XXX", df_rank$title)
  
  df_rank$rank[df_rank$val == T] <- df_all$rank[order(df_all$rank)]
  df_rank$rank[is.na(df_rank$rank)] <- "" ## To prevent that hovering prints "NA"
  
  gg <- ggplot(df_rank) +
    geom_tile_interactive(aes(x = x, y = y, fill = val, 
                              tooltip = paste0(title, ifelse(val == T, "\nRank: ", ""), rank), 
                              data_id = title2), 
                          show.legend = F, color = "grey") +
    geom_hline(yintercept = seq(0, -100, -10) + .5) +
    annotate("text", x = -25, y = seq(-5, -95, -10), 
             label = paste0(1000 * 0:9 + 1, " - ", 1000 * 1:10),
             hjust = 0, vjust = 0, family = font_plot) +
    scale_fill_manual(values = c("transparent", "darkolivegreen4")) +
    # scale_fill_manual(values = c("darkolivegreen4")) +
    ggtitle("Completion grid of top ranked animes", 
            subtitle = paste0("Number of animes ranked > 10000: ", nsup10K)) +
    theme() +
    theme_void(base_family = font_plot)
  
  gir <- girafe(ggobj = gg, options = list(opts_sizing(rescale = TRUE)))
  return(gir)
}
