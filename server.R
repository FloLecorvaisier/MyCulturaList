output = list(); input = list(); input$user_mal = "ScienceLeaf"

css_infobox <- "font-size: 18px; font-family: Ubuntu"


function(input, output, session) {
  
  #### Loading data using the API ####
  
  observeEvent(input$load_user, {
    GET(paste0("https://api.myanimelist.net/v2/users/",
               input$user_mal,
               "/animelist?fields=list_status,start_season,studios,media_type,mean,genres,average_episode_duration,num_times_rewatched&limit=1000&nsfw=true"),
        add_headers("X-MAL-CLIENT-ID" = "e715766d230d70906370ecc2e581af17")) -> x

    xx <- fromJSON(content(x, "text"), simplifyVector = FALSE)
    if (object.size(xx) > 1024) { ## To check that the user name exists
      list_of_animes <- xx$data
      
      #### Infoboxes ####
      
      infobox_animes <- data.frame(completed = numeric(5),
                                   watching = numeric(5),
                                   plan_to_watch = numeric(5),
                                   dropped = numeric(5),
                                   on_hold = numeric(5),
                                   eps = numeric(5),
                                   time = numeric(5),
                                   row.names = c("movie", "ona", "ova", "tv", "special"))
      
      
      for (anime in list_of_animes) {
        infobox_animes[anime$node$media_type, anime$list_status$status] <- infobox_animes[anime$node$media_type, anime$list_status$status] + 1
        
        infobox_animes$eps[rownames(infobox_animes) == anime$node$media_type] <-
          infobox_animes$eps[rownames(infobox_animes) == anime$node$media_type] + 
          anime$list_status$num_episodes_watched
        
        infobox_animes$time[rownames(infobox_animes) == anime$node$media_type] <- 
          infobox_animes$time[rownames(infobox_animes) == anime$node$media_type] + 
          anime$node$average_episode_duration * anime$list_status$num_episodes_watched
      }
      infobox_animes <- infobox_animes[1:5, ] # Temporary, just because MAL added "tv_special" and "cm", which are not represented...
      
      infobox_animes$type = c("Movies", "ONAs", "OVAs", "TVs", "Specials")
      
      output$all_cmpl <- renderUI(tags$p(sum(infobox_animes$completed, na.rm = T), style = css_infobox))
      output$all_cw <- renderUI(tags$p(sum(infobox_animes$watching, na.rm = T), style = css_infobox))
      output$all_ptw <- renderUI(tags$p(sum(infobox_animes$plan_to_watch, na.rm = T), style = css_infobox))
      output$all_hld <- renderUI(tags$p(sum(infobox_animes$on_hold, na.rm = T), style = css_infobox))
      output$all_eps <- renderUI(tags$p(sum(infobox_animes$eps, na.rm = T), style = css_infobox))
      output$all_time <- renderUI(tags$p(gsub("(.*)M.*", "\\1M", seconds_to_period(sum(infobox_animes$time, na.rm = T))), style = css_infobox))
      
      output$movie_cmpl <- renderUI(tags$p(infobox_animes$completed[rownames(infobox_animes) == "movie"], style = css_infobox))
      output$movie_cw <- renderUI(tags$p(infobox_animes$watching[rownames(infobox_animes) == "movie"], style = css_infobox))
      output$movie_ptw <- renderUI(tags$p(sum(infobox_animes$plan_to_watch[rownames(infobox_animes) == "movie"], na.rm = T), style = css_infobox))
      output$movie_hld <- renderUI(tags$p(sum(infobox_animes$on_hold[rownames(infobox_animes) == "movie"], na.rm = T), style = css_infobox))
      output$movie_eps <- renderUI(tags$p(infobox_animes$eps[rownames(infobox_animes) == "movie"], style = css_infobox))
      output$movie_time <- renderUI(tags$p(gsub("(.*)M.*", "\\1M", seconds_to_period(infobox_animes$time[rownames(infobox_animes) == 'movie'])), style = css_infobox))
      
      output$tv_cmpl <- renderUI(tags$p(infobox_animes$completed[rownames(infobox_animes) == "tv"], style = css_infobox))
      output$tv_cw <- renderUI(tags$p(infobox_animes$watching[rownames(infobox_animes) == "tv"], style = css_infobox))
      output$tv_ptw <- renderUI(tags$p(sum(infobox_animes$plan_to_watch[rownames(infobox_animes) == "tv"], na.rm = T), style = css_infobox))
      output$tv_hld <- renderUI(tags$p(sum(infobox_animes$on_hold[rownames(infobox_animes) == "tv"], na.rm = T), style = css_infobox))
      output$tv_eps <- renderUI(tags$p(infobox_animes$eps[rownames(infobox_animes) == "tv"], style = css_infobox))
      output$tv_time <- renderUI(tags$p(gsub("(.*)M.*", "\\1M", seconds_to_period(infobox_animes$time[rownames(infobox_animes) == 'tv'])), style = css_infobox))
      
      output$ova_cmpl <- renderUI(tags$p(infobox_animes$completed[rownames(infobox_animes) == "ova"], style = css_infobox))
      output$ova_cw <- renderUI(tags$p(infobox_animes$watching[rownames(infobox_animes) == "ova"], style = css_infobox))
      output$ova_ptw <- renderUI(tags$p(sum(infobox_animes$plan_to_watch[rownames(infobox_animes) == "ova"], na.rm = T), style = css_infobox))
      output$ova_hld <- renderUI(tags$p(sum(infobox_animes$on_hold[rownames(infobox_animes) == "ova"], na.rm = T), style = css_infobox))
      output$ova_eps <- renderUI(tags$p(infobox_animes$eps[rownames(infobox_animes) == "ova"], style = css_infobox))
      output$ova_time <- renderUI(tags$p(gsub("(.*)M.*", "\\1M", seconds_to_period(infobox_animes$time[rownames(infobox_animes) == 'ova'])), style = css_infobox))
      
      output$ona_cmpl <- renderUI(tags$p(infobox_animes$completed[rownames(infobox_animes) == "ona"], style = css_infobox))
      output$ona_cw <- renderUI(tags$p(infobox_animes$watching[rownames(infobox_animes) == "ona"], style = css_infobox))
      output$ona_ptw <- renderUI(tags$p(sum(infobox_animes$plan_to_watch[rownames(infobox_animes) == "ona"], na.rm = T), style = css_infobox))
      output$ona_hld <- renderUI(tags$p(sum(infobox_animes$on_hold[rownames(infobox_animes) == "ona"], na.rm = T), style = css_infobox))
      output$ona_eps <- renderUI(tags$p(infobox_animes$eps[rownames(infobox_animes) == "ona"], style = css_infobox))
      output$ona_time <- renderUI(tags$p(gsub("(.*)M.*", "\\1M", seconds_to_period(infobox_animes$time[rownames(infobox_animes) == 'ona'])), style = css_infobox))
      
      output$special_cmpl <- renderUI(tags$p(infobox_animes$completed[rownames(infobox_animes) == "special"], style = css_infobox))
      output$special_cw <- renderUI(tags$p(infobox_animes$watching[rownames(infobox_animes) == "special"], style = css_infobox))
      output$special_ptw <- renderUI(tags$p(sum(infobox_animes$plan_to_watch[rownames(infobox_animes) == "special"], na.rm = T), style = css_infobox))
      output$special_hld <- renderUI(tags$p(sum(infobox_animes$on_hold[rownames(infobox_animes) == "special"], na.rm = T), style = css_infobox))
      output$special_eps <- renderUI(tags$p(infobox_animes$eps[rownames(infobox_animes) == "special"], style = css_infobox))
      output$special_time <- renderUI(tags$p(gsub("(.*)M.*", "\\1M", seconds_to_period(infobox_animes$time[rownames(infobox_animes) == 'special'])), style = css_infobox))
      
      #### Number of animes per season ####
      
      df_season <- data.frame(title = character(length(list_of_animes)),
                              type = character(length(list_of_animes)),
                              year = numeric(length(list_of_animes)),
                              season = numeric(length(list_of_animes)),
                              score = numeric(length(list_of_animes)))
      
      for (i in 1:length(list_of_animes)) {
        if (list_of_animes[[i]]$list_status$status == "completed") {
          df_season$title[i] <- list_of_animes[[i]]$node$title
          df_season$type[i] <- list_of_animes[[i]]$node$media_type
          df_season$year[i] <- list_of_animes[[i]]$node$start_season$year
          df_season$season[i] <- list_of_animes[[i]]$node$start_season$season
          df_season$score[i] <- list_of_animes[[i]]$list_status$score
        }
      }
      df_season <- df_season[df_season$season != 0, ]
      
      table_season <- as.data.frame(with(df_season, table(type, year, season)))
      table_season$year <- as.numeric(as.character(table_season$year))
      table_season$season <- factor(table_season$season, levels = c("winter", "spring", "summer", "fall"))
      
      output$seasonplot <- renderPlot({
        ggplot(table_season) +
          geom_col(aes(x = year, y = Freq, fill = season)) +
          scale_fill_manual(values = c("lightblue1", "olivedrab2", "orangered1", "brown"),
                            labels = c("Winter", "Spring", "Summer", "Fall")) +
          scale_x_continuous(limits = c(min(df_season$year) - 1, max(df_season$year) + 1)) +
          scale_y_continuous(limits = c(0, max(tapply(table_season$Freq, table_season$year, sum)))) +
          labs(x = element_blank(), y = element_blank(), fill = element_blank()) +
          ggtitle("Number of animes watched per season of first diffusion") +
          theme_minimal(base_family = "Avenir Next LT Pro", base_size = 12)
      }, res = 96)
      
      
      #### Number of animes per personal score ####
      
      table_score <- as.data.frame(with(df_season, table(score)))
      table_score$score <- as.numeric(as.character(table_score$score))
      
      output$scoreplot <- renderPlot({
        ggplot(table_score) +
          geom_col(aes(x = score, y = Freq), fill = "darkolivegreen4") +
          scale_x_continuous(breaks = 1:10, limits = c(.5, 10.5)) +
          labs(x = element_blank(), y = element_blank()) +
          ggtitle("Number of animes watched per personal score") +
          theme_minimal(base_family = "Avenir Next LT Pro", base_size = 12)
      }, res = 96)
    } else {
      showNotification("User not found!", type = "error")
    }
  })
}
