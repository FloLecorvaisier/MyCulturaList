output = list(); input = list(); input$user_mal = "ScienceLeaf"

function(input, output, session) {
  
  observeEvent(input$load_user, {
    GET(paste0("https://api.myanimelist.net/v2/users/",
               input$user_mal,
               "/animelist?fields=list_status,start_season,studios,media_type,mean,genres,average_episode_duration,num_times_rewatched&limit=1000&nsfw=true"),
        add_headers("X-MAL-CLIENT-ID" = "e715766d230d70906370ecc2e581af17")) -> x

    xx <- fromJSON(content(x, "text"), simplifyVector = FALSE)

    list_of_animes <- xx$data


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
    
    
    #### Number of animes per 
    
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
  })
}
