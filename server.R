output = list(); input = list(); input$user_mal = "ScienceLeaf"
# input$listType =  c("movie", "ona", "ova", "tv", "special")
# input$listYear = c(1917, 2025)
# input$selectList = "Satoshi Kon full length movies"

css_infobox <- "font-size: 16px; font-family: Aleo"

## Will contain multiple reactive values too complex for reactiveVal()
reacVals <- reactiveValues()

function(input, output, session) {
  
  ## Reactive value for clicked element
  
  KEY <- readLines("MAL-KEY", warn = F)
  
  font_plot <- "Aleo"
  
  #### Loading data using the API ####
  
  observeEvent(input$load_user, {
    GET(paste0("https://api.myanimelist.net/v2/users/",
               input$user_mal,
               "/animelist?fields=list_status,start_season,studios,media_type,mean,genres,average_episode_duration,num_times_rewatched&limit=1000&nsfw=true"),
        add_headers("X-MAL-CLIENT-ID" = KEY)) -> x

    xx <- fromJSON(content(x, "text"), simplifyVector = FALSE)
    if (object.size(xx$data) > 1024) { ## To check that the user name exists
      list_of_animes <- xx$data

      #### Infoboxes ####

      infobox_animes <- data.frame(completed = numeric(10),
                                   watching = numeric(10),
                                   plan_to_watch = numeric(10),
                                   dropped = numeric(10),
                                   on_hold = numeric(10),
                                   eps = numeric(10),
                                   time = numeric(10),
                                   row.names = c("movie", "ona", "ova", "tv", "special", "tv_special", "cm", "pv", "music", "unknown"))


      for (anime in list_of_animes) {
        infobox_animes[anime$node$media_type, anime$list_status$status] <- infobox_animes[anime$node$media_type, anime$list_status$status] + 1

        infobox_animes$eps[rownames(infobox_animes) == anime$node$media_type] <-
          infobox_animes$eps[rownames(infobox_animes) == anime$node$media_type] +
          anime$list_status$num_episodes_watched

        infobox_animes$time[rownames(infobox_animes) == anime$node$media_type] <-
          infobox_animes$time[rownames(infobox_animes) == anime$node$media_type] +
          anime$node$average_episode_duration * anime$list_status$num_episodes_watched
      }
      # infobox_animes <- infobox_animes[1:5, ] # Temporary, just because MAL added "tv_special" and "cm", which are not represented...
      infobox_animes <- rbind(infobox_animes, apply(infobox_animes, 2, sum))
      rownames(infobox_animes)[11] <- "general"
      infobox_animes$time <- seconds_to_period(infobox_animes$time)
      infobox_animes$type <- c("Movies", "ONAs", "OVAs", "TVs", "Specials", "TV specials", "CMs", "PVs", "Musics", "Unknown", "General")

      ## Rendering the values in the different infoboxes
      ll <- levels(interaction(colnames(infobox_animes)[c(1:3, 5:7)], rownames(infobox_animes), sep = "|")) # Warning but working...
      lapply(ll, function(k) {
        output[[k]] <- renderUI(tags$p(infobox_animes[rownames(infobox_animes) == gsub(".*\\|(.*)", "\\1", k), 
                                                      colnames(infobox_animes) == gsub("(.*)\\|.*", "\\1", k)], 
                                style = css_infobox))
      })
      
      
      #### Summary of the data ####

      df_all <- data.frame(title = character(length(list_of_animes)),
                           type = character(length(list_of_animes)),
                           year = numeric(length(list_of_animes)),
                           season = numeric(length(list_of_animes)),
                           score = numeric(length(list_of_animes)),
                           studio = character(length(list_of_animes)),
                           genre = character(length(list_of_animes)))

      for (i in 1:length(list_of_animes)) {
        if (list_of_animes[[i]]$list_status$status == "completed") {
          df_all$title[i] <- list_of_animes[[i]]$node$title
          df_all$type[i] <- list_of_animes[[i]]$node$media_type
          df_all$year[i] <- list_of_animes[[i]]$node$start_season$year
          df_all$season[i] <- list_of_animes[[i]]$node$start_season$season
          df_all$score[i] <- list_of_animes[[i]]$list_status$score
          studio <- character()
          for (j in 1:length(list_of_animes[[i]]$node$studios)) {
            studio <- c(studio, list_of_animes[[i]]$node$studios[[j]]$name)
          }
          df_all$studio[i] <- paste(studio, collapse = "|")
          genre <- character()
          for (j in 1:length(list_of_animes[[i]]$node$genres)) {
            genre <- c(genre, list_of_animes[[i]]$node$genres[[j]]$name)
          }
          df_all$genre[i] <- paste(genre, collapse = "|")
        }
      }
      df_all <- df_all[df_all$season != 0, ] ## Dirty method to remove unfinished animes
      # df_all$type[df_all$type == "tv_special"] <- "special"

      
      #### Number of animes per season ####
      
      table_season <- as.data.frame(with(df_all, table(type, year, season)))
      table_season$year <- as.numeric(as.character(table_season$year))
      table_season$season <- factor(table_season$season, levels = c("winter", "spring", "summer", "fall"))

      output$season_plot <- renderGirafe({
        tb_season_filtered <- table_season[table_season$type %in% input$listType
                                           &table_season$year %in% min(input$listYear):max(input$listYear), ] |> 
          group_by(season, year) |>
          summarise(Freq = sum(Freq))
        gg <- ggplot(tb_season_filtered) +
          geom_col_interactive(aes(x = year, y = Freq, fill = season, tooltip = Freq, data_id = paste0(season, year))) +
          scale_fill_manual(values = c("lightblue1", "olivedrab2", "orangered1", "brown"),
                            labels = c("Winter", "Spring", "Summer", "Fall")) +
          scale_x_continuous(limits = c(min(df_all$year) - 1, max(df_all$year) + 1), breaks = seq(1960, 2020, 10)) +
          scale_y_continuous(limits = c(0, max(tapply(table_season$Freq, table_season$year, sum))), breaks = seq(0, 50, 5)) +
          labs(x = element_blank(), y = element_blank(), fill = element_blank()) +
          ggtitle("Number of animes watched per season of first diffusion") +
          theme_minimal(base_family = font_plot, base_size = 10) +
          theme(legend.position = "bottom")
        gir <- girafe(ggobj = gg, options = list(opts_selection(type = "single"), opts_sizing(rescale = TRUE)))
      })
      
      
      #### List of animes in selection ####
      
      ## To adapt to whatever plot was clicked and unselect other clicked plots.
      observeEvent(input$season_plot_selected, {
        reacVals$clicked = "season"
        session$sendCustomMessage(type = 'genres_plot_set', message = character(0))
        session$sendCustomMessage(type = 'score_plot_set', message = character(0))
        session$sendCustomMessage(type = 'studio_plot_set', message = character(0))
        })
      observeEvent(input$score_plot_selected, {
        reacVals$clicked = "score"
        session$sendCustomMessage(type = 'genres_plot_set', message = character(0))
        session$sendCustomMessage(type = 'season_plot_set', message = character(0))
        session$sendCustomMessage(type = 'studio_plot_set', message = character(0))
        })
      observeEvent(input$studio_plot_selected, {
        reacVals$clicked = "studio"
        session$sendCustomMessage(type = 'genres_plot_set', message = character(0))
        session$sendCustomMessage(type = 'score_plot_set', message = character(0))
        session$sendCustomMessage(type = 'season_plot_set', message = character(0))
      })
      observeEvent(input$genres_plot_selected, {
        reacVals$clicked = "genres"
        session$sendCustomMessage(type = 'score_plot_set', message = character(0))
        session$sendCustomMessage(type = 'season_plot_set', message = character(0))
        session$sendCustomMessage(type = 'studio_plot_set', message = character(0))
      })
      
      ## Prepares the titles that will be rendered.
      selection <- reactive({
        out <- NULL
        if (is.null(reacVals$clicked)) {
          return()
        }
        if (reacVals$clicked == "season") {
          out <- df_all$title[paste0(df_all$season, df_all$year) %in% input$season_plot_selected
                              & df_all$type %in% input$listType
                              & df_all$year %in% min(input$listYear):max(input$listYear)]
        }
        if (reacVals$clicked == "score") {
          out <- df_all$title[df_all$score %in% input$score_plot_selected
                              & df_all$type %in% input$listType
                              & df_all$year %in% min(input$listYear):max(input$listYear)]
        }
        if (reacVals$clicked == "studio") {
          out <- df_studio$title[df_studio$studio %in% input$studio_plot_selected
                                 & df_studio$type %in% input$listType
                                 & df_studio$year %in% min(input$listYear):max(input$listYear)]
        }
        if (reacVals$clicked == "genres") {
          out <- df_genre$title[df_genre$genre  %in% input$genres_plot_selected
                                & df_genre$type %in% input$listType
                                & df_genre$year %in% min(input$listYear):max(input$listYear)]
        }
        out
      })
      
      ## Output of the list of titles
      output$list_of_animes <- renderUI({
        HTML(paste0(selection(), collapse = "<br/>"))
      })
      

      #### Number of animes per personal score ####

      table_score <- as.data.frame(with(df_all, table(type, year, score)))
      table_score$score <- as.numeric(as.character(table_score$score))

      output$score_plot <- renderGirafe({
        tb_score_filtered <- table_score[table_score$type %in% input$listType
                                         & table_score$year %in% min(input$listYear):max(input$listYear), ] |> 
          group_by(score) |>
          summarise(Freq = sum(Freq))
        gg <- ggplot(tb_score_filtered) +
          geom_col_interactive(aes(x = score, y = Freq, tooltip = Freq, data_id = score), fill = "darkolivegreen4") +
          scale_x_continuous(breaks = 1:10, limits = c(.5, 10.5)) +
          labs(x = element_blank(), y = element_blank()) +
          ggtitle("Number of animes watched per personal score") +
          theme_minimal(base_family = font_plot, base_size = 12)
        gir <- girafe(ggobj = gg, options = list(opts_selection(type = "single"), opts_sizing(rescale = TRUE)))
      })
      
      #### Number of animes per studio ####
        
      df_studio <- data.frame("studio" = unlist(strsplit(paste(df_all$studio, collapse = "|"), "\\|")))
      df_studio$title <- rep(df_all$title, times = lengths(regmatches(df_all$studio, gregexpr("\\|", df_all$studio))) + 1) ## To add the title for each
      df_studio$type <- rep(df_all$type, times = lengths(regmatches(df_all$studio, gregexpr("\\|", df_all$studio))) + 1) ## To add the type for each
      df_studio$year <- rep(df_all$year, times = lengths(regmatches(df_all$studio, gregexpr("\\|", df_all$studio))) + 1) ## To add the year for each
      df_studio$score <- rep(df_all$score, times = lengths(regmatches(df_all$studio, gregexpr("\\|", df_all$studio))) + 1) ## To add the year for each
      
      table_studio <- as.data.frame(with(df_studio, table(type, year, studio)))
      
      ## For now changing studios to print does not work because "observeEvent"is dependent on the previous one, i.e. it only updates when new user is added
      studios_to_print <- names(sort(with(table_studio[table_studio$type %in% input$listType
                                                       & table_studio$year %in% min(input$listYear):max(input$listYear), ],
                                          tapply(Freq, studio, sum)), decreasing = T))[1:min(10, length(unique(df_studio$studio)))]
      
      output$studio_plot <- renderGirafe({
        tb_studio_filtered <- table_studio[table_studio$type %in% input$listType
                                           & table_studio$year %in% min(input$listYear):max(input$listYear)
                                           & table_studio$studio %in% studios_to_print, ] |> 
          group_by(studio) |>
          summarise(Freq = sum(Freq))
        tb_studio_filtered2 <- rbind(tb_studio_filtered, data.frame(studio = "", Freq = 0)) ## For polar coord version.
        if (!input$switch_studio) {
          gg <- ggplot(tb_studio_filtered) +
            geom_col_interactive(aes(x = Freq , y = factor(studio, levels = rev(studios_to_print)), tooltip = Freq, data_id = studio), fill = "darkolivegreen4") +
            labs(x = element_blank(), y = element_blank()) +
            ggtitle("Number of animes watched per studio") +
            theme_minimal(base_family = font_plot, base_size = 12) +
            theme()
        } else {
          gg <- ggplot(tb_studio_filtered2) +
            geom_hline(yintercept = seq(0, max(tb_studio_filtered$Freq), length.out = 5), color = "gray80") +
            geom_segment(x = 1:nrow(tb_studio_filtered2), y = 0, yend = max(tb_studio_filtered$Freq), color = "gray80") +
            geom_col_interactive(aes(y = Freq , x = factor(studio, levels = rev(c("", studios_to_print))), tooltip = Freq, data_id = studio), 
                                 fill = "darkolivegreen4") +
            annotate("label",
                     x = 11,
                     y = seq(0,max(tb_studio_filtered$Freq), length.out = 5),
                     label = seq(0,max(tb_studio_filtered$Freq), length.out = 5),
                     color = "gray30", label.size = 0) +
            labs(x = element_blank(), y = element_blank()) +
            ggtitle("Number of animes watched per studio") +
            ylim(-5, max(tb_studio_filtered$Freq)) +
            theme_minimal(base_family = font_plot, base_size = 12) +
            coord_polar(start = pi / nrow(tb_studio_filtered2)) +
            theme(axis.text.y = element_blank(),
                  axis.text.x = element_text(hjust = 1, 
                                             angle = seq(360 - 360 / nrow(tb_studio_filtered2), 0, -360 / nrow(tb_studio_filtered2))),
                  panel.grid = element_blank(),
                  panel.background = element_rect(fill = "white", color = "white"),
                  panel.grid.major.y = element_blank())
        }
        
        gir <- girafe(ggobj = gg, options = list(opts_selection(type = "single"), opts_sizing(rescale = TRUE)))
      })
      
      #### Number of animes per genre ####
      
      df_genre <- data.frame("genre" = unlist(strsplit(paste(df_all$genre, collapse = "|"), "\\|")))
      df_genre$title <- rep(df_all$title, times = lengths(regmatches(df_all$genre, gregexpr("\\|", df_all$genre))) + 1) ## To add the title for each
      df_genre$type <- rep(df_all$type, times = lengths(regmatches(df_all$genre, gregexpr("\\|", df_all$genre))) + 1) ## To add the type for each
      df_genre$year <- rep(df_all$year, times = lengths(regmatches(df_all$genre, gregexpr("\\|", df_all$genre))) + 1) ## To add the year for each
      df_genre$score <- rep(df_all$score, times = lengths(regmatches(df_all$genre, gregexpr("\\|", df_all$genre))) + 1) ## To add the score for each

      ## Only keeping the "Genres" tags from MAL
      df_genre_genres <- df_genre[df_genre$genre %in% c("Action", "Adventure", "Avant Garde", "Award Winning", "Boys Love",
                                                        "Comedy", "Drama", "Fantasy", "Girls Love", "Gourmet", 
                                                        "Horror", "Mystery", "Romance", "Sci-Fi", "Slice of Life",
                                                        "Sports", "Supernatural", "Suspense"), ]
      
      table_genres <- as.data.frame(with(df_genre_genres, table(type, year, genre)))
      
      genres_to_print <- names(sort(with(table_genres[table_genres$type %in% input$listType
                                                      & table_genres$year %in% min(input$listYear):max(input$listYear), ],
                                         tapply(Freq, genre, sum)), decreasing = T))[1:min(10, length(unique(df_genre_genres$genre)))]
      
      output$genres_plot <- renderGirafe({
        tb_genres_filtered <- table_genres[table_genres$type %in% input$listType
                                           & table_genres$year %in% min(input$listYear):max(input$listYear)
                                           & table_genres$genre %in% genres_to_print, ] |> 
          group_by(genre) |>
          summarise(Freq = sum(Freq))
        gg <- ggplot(tb_genres_filtered) +
          geom_col_interactive(aes(x = Freq , y = factor(genre, levels = rev(genres_to_print)), tooltip = Freq, data_id = genre), fill = "darkolivegreen4") +
          labs(x = element_blank(), y = element_blank()) +
          ggtitle("Number of animes watched per genre") +
          theme_minimal(base_family = font_plot, base_size = 12) +
          theme()
        gir <- girafe(ggobj = gg, options = list(opts_selection(type = "single"), opts_sizing(rescale = TRUE)))
      })
      
      #### Score per studio ####
      
      output$score_studio_plot <- renderGirafe({
        mean_score_studio <- tapply(df_studio$score[df_studio$type %in% input$listType
                                                    & df_studio$year %in% min(input$listYear):max(input$listYear)
                                                    & df_studio$studio %in% studios_to_print], 
                                    df_studio$studio[df_studio$type %in% input$listType
                                                     & df_studio$year %in% min(input$listYear):max(input$listYear)
                                                     & df_studio$studio %in% studios_to_print], 
                                    mean)
        mean_score_studio <- round(mean_score_studio, 2)
        gg <- ggplot(data = df_studio[df_studio$type %in% input$listType
                                      & df_studio$year %in% min(input$listYear):max(input$listYear)
                                      & df_studio$studio %in% studios_to_print, ]) +
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
      })
      
      #### Score per genre ####
      
      output$score_genres_plot <- renderGirafe({
        mean_score_genres <- tapply(df_genre$score[df_genre$type %in% input$listType
                                                   & df_genre$year %in% min(input$listYear):max(input$listYear)
                                                   & df_genre$genre %in% genres_to_print], 
                                    df_genre$genre[df_genre$type %in% input$listType
                                                   & df_genre$year %in% min(input$listYear):max(input$listYear)
                                                   & df_genre$genre %in% genres_to_print], 
                                    mean)
        mean_score_genres <- round(mean_score_genres, 2)
        gg <- ggplot(df_genre[df_genre$type %in% input$listType
                              & df_genre$year %in% min(input$listYear):max(input$listYear)
                              & df_genre$genre %in% genres_to_print, ]) +
          geom_boxplot_interactive(aes(x = score, y = factor(genre, levels = rev(genres_to_print))), fill = "darkolivegreen4") +
          annotate("label", 
                   x = mean_score_genres, 
                   y = factor(names(mean_score_genres), levels = rev(genres_to_print)),
                   label = mean_score_genres, 
                   fill = "darkolivegreen2") +
          labs(x = element_blank(), y = element_blank()) +
          ggtitle("Score per genre") +
          scale_x_continuous(breaks = 1:10) +
          coord_cartesian(xlim = c(1, 10)) +
          theme_minimal(base_family = font_plot, base_size = 12) +
          theme()
        gir <- girafe(ggobj = gg, options = list(opts_selection(type = "single"), opts_sizing(rescale = TRUE)))
      })
      
      #### Lists to complete ####
      
      ## The file containing the lists that can be finished
      reacVals$lists <- read.table("lists.txt", sep = "\t")
      colnames(reacVals$lists) <- c("list", "id")
      reacVals$lists$title <- reacVals$lists$picture <- reacVals$lists$watched <- reacVals$lists$link <- NA
      
      observeEvent(input$selectList, {
        ## GETting the data for the elements of the lists.txt file
        for (id in unique(reacVals$lists$id[reacVals$lists$list == input$selectList])) {
          if (is.na(reacVals$lists$title[reacVals$lists$id == id])[1]) {
            getid <- GET(paste0("https://api.myanimelist.net/v2/anime/", id, "?fields=title,main_picture"),
                         add_headers("X-MAL-CLIENT-ID" = KEY))
            dataid <- fromJSON(content(getid, "text"), simplifyVector = FALSE)
            reacVals$lists$title[reacVals$lists$id == id] <- dataid$title
            reacVals$lists$picture[reacVals$lists$id == id] <- dataid$main_picture$medium
            reacVals$lists$watched[reacVals$lists$id == id] <- ifelse(dataid$title %in% df_all$title, T, F)
            reacVals$lists$link[reacVals$lists$id == id] <- paste0("https://myanimelist.net/anime/", id)
          }
        }
        
        ## Reactive values that will contain the images URLs and links to MAL
        list_watched <- reactiveVal(data.frame(img = NULL, link = NULL))
        list_not_watched <- reactiveVal(data.frame(img = NULL, link = NULL))
        
        updateProgressBar(
          id = "pBlists",
          value = nrow(reacVals$lists[reacVals$lists$title %in% df_all$title & 
                                        reacVals$lists$list == input$selectList, ]) /
            nrow(reacVals$lists[reacVals$lists$list == input$selectList, ]) * 100
        )
        
        lists_select <- reacVals$lists[reacVals$lists$list == input$selectList, ][order(reacVals$lists$title[reacVals$lists$list == input$selectList]), ]
        updt_watched <- data.frame(img = lists_select$picture[lists_select$watched == T],
                                   link = lists_select$link[lists_select$watched == T])
        updt_not_watched <- data.frame(img = lists_select$picture[lists_select$watched == F],
                                       link = lists_select$link[lists_select$watched == F])
        
        list_watched(updt_watched)
        list_not_watched(updt_not_watched)
        
        output$image_gallery_watched <- renderUI({
          div(class = "image-container",
              lapply(1:nrow(list_watched()), function(i) {
                tags$a(
                  href = list_watched()$link[i], # URL du lien
                  target = "_blank",
                  tags$img(src = list_watched()$img[i], alt = "Image dynamique", class = "image-item")
                )
              })
          )
        })
        output$image_gallery_not_watched <- renderUI({
          div(class = "image-container",
              lapply(1:nrow(list_not_watched()), function(i) {
                tags$a(
                  href = list_not_watched()$link[i], # URL du lien
                  target = "_blank",
                  tags$img(src = list_not_watched()$img[i], alt = "Image dynamique", class = "image-item")
                )
              })
          )
        })
      })
    } else if (xx$error == "bad_request") {
      showNotification("Invalid cliend ID!", type = "error")
    } else if (xx$error == "not_found") {
      showNotification("User not found!", type = "error")
    }
  }) ## End of the main observeEvent
}
