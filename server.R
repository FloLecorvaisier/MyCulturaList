output = list(); input = list(); input$user_mal = "ScienceLeaf"
# input$listType =  c("movie", "ona", "ova", "tv", "special", "tv_special")
# input$listYear = c(1917, 2025)
# input$selectList = "Satoshi Kon full length movies"

css_infobox <- "font-size: 16px; font-family: Aleo"

## Will contain multiple reactive values too complex for reactiveVal()
reacVals <- reactiveValues()

function(input, output, session) {
  
  ## Loading external files
  for (file in list.files("func")) {
    source(paste0("func/", file))
  }
  
  ## Reactive value for clicked element
  
  KEY <- readLines("MAL-KEY", warn = F)
  
  font_plot <- "Aleo"
  
  #### Loading data using the API ####

  observeEvent(input$load_user, {
    resp <- GET(paste0("https://api.myanimelist.net/v2/users/",
               input$user_mal,
               "/animelist?fields=list_status,start_season,studios,media_type,mean,popularity,rank,genres,average_episode_duration,num_list_users,num_times_rewatched&limit=1000&nsfw=true"),
        add_headers("X-MAL-CLIENT-ID" = KEY))

    list_all <- fromJSON(content(resp, "text"), simplifyVector = FALSE)
    if (object.size(list_all$data) > 1024) { ## To check that the user name exists
      list_of_animes <- list_all$data

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
                           status = character(length(list_of_animes)),
                           type = character(length(list_of_animes)),
                           year = numeric(length(list_of_animes)),
                           season = numeric(length(list_of_animes)),
                           score = numeric(length(list_of_animes)),
                           mean = numeric(length(list_of_animes)),
                           popularity = numeric(length(list_of_animes)),
                           num_list_users = numeric(length(list_of_animes)),
                           rank = numeric(length(list_of_animes)),
                           studio = character(length(list_of_animes)),
                           genre = character(length(list_of_animes)))

      for (i in 1:length(list_of_animes)) {
        df_all$title[i] <- list_of_animes[[i]]$node$title
        df_all$status[i] <- list_of_animes[[i]]$list_status$status
        df_all$type[i] <- list_of_animes[[i]]$node$media_type
        
        ## ifelse() needed because some animes do not have a release date yet.
        df_all$year[i] <- ifelse(!is.null(list_of_animes[[i]]$node$start_season$year), list_of_animes[[i]]$node$start_season$year, NA)
        df_all$season[i] <- ifelse(!is.null(list_of_animes[[i]]$node$start_season$season), list_of_animes[[i]]$node$start_season$season, NA)
        df_all$score[i] <- list_of_animes[[i]]$list_status$score
        df_all$mean[i] <- ifelse(!is.null(list_of_animes[[i]]$node$mean), list_of_animes[[i]]$node$mean, NA)
        df_all$popularity[i] <- ifelse(!is.null(list_of_animes[[i]]$node$popularity), list_of_animes[[i]]$node$popularity, NA)
        df_all$rank[i] <- ifelse(!is.null(list_of_animes[[i]]$node$rank), list_of_animes[[i]]$node$rank, NA)
        df_all$num_list_users[i] <- list_of_animes[[i]]$node$num_list_users
        studio <- character(length(list_of_animes[[i]]$node$studios))
        if (length(list_of_animes[[i]]$node$studios) > 0) {
          for (j in 1:length(list_of_animes[[i]]$node$studios)) {
            studio[j] <- list_of_animes[[i]]$node$studios[[j]]$name
          }
          df_all$studio[i] <- paste(studio, collapse = "|")
        }
        genre <- character()
        for (j in 1:length(list_of_animes[[i]]$node$genres)) {
          genre <- c(genre, list_of_animes[[i]]$node$genres[[j]]$name)
        }
        df_all$genre[i] <- paste(genre, collapse = "|")
      }
      df_all <- df_all[df_all$status == "completed", ]
      
      ## Cutting popularity by intervals
      df_all$inter_pop <- cut(df_all$popularity, seq(0, 1e4, 1e3))
      
      ## replacing "'" else data_id does not work and many problems arise.
      df_all$title2 <- gsub("'", "XXX", df_all$title)
      
      #### List of animes in selection ####
      
      ## To adapt to whatever plot was clicked and unselect other clicked plots.
      observeEvent(input$grid_rank_plot_selected, {
        reacVals$clicked = "grid_rank"
        # session$sendCustomMessage(type = 'grid_rank_plot_set', message = character(0))
        session$sendCustomMessage(type = 'grid_popularity_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_genres_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_season_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_score_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_studio_plot_set', message = character(0))
        session$sendCustomMessage(type = 'scoremean_plot_set', message = character(0))
      })
      observeEvent(input$grid_popularity_plot_selected, {
        reacVals$clicked = "grid_popularity"
        session$sendCustomMessage(type = 'grid_rank_plot_set', message = character(0))
        # session$sendCustomMessage(type = 'grid_popularity_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_genres_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_season_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_score_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_studio_plot_set', message = character(0))
        session$sendCustomMessage(type = 'scoremean_plot_set', message = character(0))
      })
      observeEvent(input$n_genres_plot_selected, {
        reacVals$clicked = "genres"
        session$sendCustomMessage(type = 'grid_rank_plot_set', message = character(0))
        session$sendCustomMessage(type = 'grid_popularity_plot_set', message = character(0))
        # session$sendCustomMessage(type = 'n_genres_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_season_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_score_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_studio_plot_set', message = character(0))
        session$sendCustomMessage(type = 'scoremean_plot_set', message = character(0))
      })
      observeEvent(input$n_season_plot_selected, {
        reacVals$clicked = "season"
        session$sendCustomMessage(type = 'grid_rank_plot_set', message = character(0))
        session$sendCustomMessage(type = 'grid_popularity_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_genres_plot_set', message = character(0))
        # session$sendCustomMessage(type = 'n_season_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_score_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_studio_plot_set', message = character(0))
        session$sendCustomMessage(type = 'scoremean_plot_set', message = character(0))
      })
      observeEvent(input$n_score_plot_selected, {
        reacVals$clicked = "score"
        session$sendCustomMessage(type = 'grid_rank_plot_set', message = character(0))
        session$sendCustomMessage(type = 'grid_popularity_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_genres_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_season_plot_set', message = character(0))
        # session$sendCustomMessage(type = 'n_score_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_studio_plot_set', message = character(0))
        session$sendCustomMessage(type = 'scoremean_plot_set', message = character(0))
      })
      observeEvent(input$n_studio_plot_selected, {
        reacVals$clicked = "studio"
        session$sendCustomMessage(type = 'grid_rank_plot_set', message = character(0))
        session$sendCustomMessage(type = 'grid_popularity_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_genres_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_season_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_score_plot_set', message = character(0))
        # session$sendCustomMessage(type = 'n_studio_plot_set', message = character(0))
        session$sendCustomMessage(type = 'scoremean_plot_set', message = character(0))
      })
      observeEvent(input$scoremean_plot_selected, {
        reacVals$clicked = "scoremean"
        session$sendCustomMessage(type = 'grid_rank_plot_set', message = character(0))
        session$sendCustomMessage(type = 'grid_popularity_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_genres_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_season_plot_set', message = character(0))
        session$sendCustomMessage(type = 'score_plot_set', message = character(0))
        session$sendCustomMessage(type = 'n_studio_plot_set', message = character(0))
        # session$sendCustomMessage(type = 'scoremean_plot_set', message = character(0))
      })
      
      ## Prepares the titles that will be rendered.
      selection <- reactive({
        out <- NULL
        if (is.null(reacVals$clicked)) {
          return()
        }
        if (reacVals$clicked == "grid_rank") {
          out <- df_all$title[df_all$title2 %in% input$grid_rank_plot_selected
                              & df_all$type %in% input$listType
                              & df_all$year %in% min(input$listYear):max(input$listYear)]
        }
        if (reacVals$clicked == "grid_popularity") {
          if (input$switch_grid_popularity == "grid") {
            out <- df_all$title[df_all$title2 %in% input$grid_popularity_plot_selected
                                & df_all$type %in% input$listType
                                & df_all$year %in% min(input$listYear):max(input$listYear)]
          } else {
            out <- df_all$title[df_all$inter_pop %in% input$grid_popularity_plot_selected
                                & df_all$type %in% input$listType
                                & df_all$year %in% min(input$listYear):max(input$listYear)]
          }
          
        }
        if (reacVals$clicked == "season") {
          out <- df_all$title[paste0(df_all$season, df_all$year) %in% input$n_season_plot_selected
                              & df_all$type %in% input$listType
                              & df_all$year %in% min(input$listYear):max(input$listYear)]
        }
        if (reacVals$clicked == "score") {
          out <- df_all$title[df_all$score %in% input$n_score_plot_selected
                              & df_all$type %in% input$listType
                              & df_all$year %in% min(input$listYear):max(input$listYear)]
        }
        if (reacVals$clicked == "scoremean") {
          out <- df_all$title[df_all$title2 %in% input$scoremean_plot_selected
                              & df_all$type %in% input$listType
                              & df_all$year %in% min(input$listYear):max(input$listYear)]
        }
        if (reacVals$clicked == "studio") {
          ## Supplementary loop needed else sapply() returns an error when the plot is unselected.
          if (!is.null(input$n_studio_plot_selected)) {  
            cond_studio <- sapply(strsplit(df_all$studio, "\\|"), function(x) input$n_studio_plot_selected %in% x)
          } else cond_studio <- FALSE
          out <- df_all$title[cond_studio
                              & df_all$type %in% input$listType
                              & df_all$year %in% min(input$listYear):max(input$listYear)]
        }
        if (reacVals$clicked == "genres") {
          out <- df_genre$title[df_genre$genre  %in% input$n_genres_plot_selected
                                & df_genre$type %in% input$listType
                                & df_genre$year %in% min(input$listYear):max(input$listYear)]
        }
        out
      })
      
      ## Output of the list of titles
      output$list_of_animes <- renderUI({
        HTML(paste0(selection(), collapse = "<br/>"))
      })
      
      ## Listing the filters
      filters <- reactive({
        list(Type = input$listType,
             minYear = min(input$listYear),
             maxYear = max(input$listYear))
      })
      
      #### Number of animes per... ####
      
      ##### ...season ####
      ## func/gg-n-season.R
      
      output$n_season_plot <- renderGirafe({
        gg_n_season(df_all, font_plot, filters())
      })
        
      ##### ...personal score ####
      ## func/gg-n-score.R

      output$n_score_plot <- renderGirafe({
        gg_n_score(df_all, font_plot, filters(), switch = input$switch_score)
      })
      
      ##### ...studio ####
        
      output$n_studio_plot <- renderGirafe({
        gg_n_studio(df_all, font_plot, filters(), switch = input$switch_studio)
      })
      
      ##### ...genre ####
      
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
      
      output$n_genres_plot <- renderGirafe({
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
        gir <- girafe(ggobj = gg, options = list(opts_sizing(rescale = TRUE)))
      })
      
      #### Score per... ####
      
      ##### ...studio ####
      
      output$score_studio_plot <- renderGirafe({
        gg_score_studio(df_all, font_plot, filters(), switch = input$switch_studio)
      })
      
      ##### ...genre ####
      
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
        gir <- girafe(ggobj = gg, options = list(opts_sizing(rescale = TRUE)))
      })
      
      #### Correlation score/mean ####
      
      ## Rounding to first decimal for greater readibility
      df_all$xmean <- round(df_all$mean, 1)
      df_all$xmean <- floor(df_all$mean * 10) / 10
      
      ## Coloring according to score VS mean
      df_all$sup <- df_all$score > df_all$mean
      df_all <- df_all[order(df_all$sup), ]
      
      ## Counting the number of repetitions per group of xmean and score
      df_all <- df_all %>%
        group_by(xmean, score) %>%
        mutate(n = row_number(),
               count = n()) %>%
        ungroup()
      
      ## Finding the highest number of duplicates (needed for next step and adaptability of different df)
      max_dup <- max(df_all$count)
      
      ## Calculating the offset
      df_all <- df_all %>%
        group_by(xmean, score) %>%
        mutate(n = row_number(),
               offset = (n - mean(n)) * 1 / max_dup,
               xscore = score + offset)
      
      output$scoremean_plot <- renderGirafe({
        gg <- ggplot(df_all[df_all$type %in% input$listType
                            & df_all$year %in% min(input$listYear):max(input$listYear), ]) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
          geom_point_interactive(aes(x = xscore, y = xmean, color = sup, data_id = title2,
                                     tooltip = paste0(title, "\nUser score: ", score, "\nMAL score: ", mean)), 
                                 show.legend = F, size = 1) +
          scale_x_continuous(breaks = 1:10) +
          scale_y_continuous(breaks = 1:10) +
          scale_color_manual(values = c("darkolivegreen3", "darkolivegreen4")) +
          coord_cartesian(xlim = c(1, 10), ylim = c(1, 10)) +
          labs(x = "Personal score", y = "Mean MAL users score") +
          theme_minimal(base_family = font_plot, base_size = 12) +
          theme(panel.grid.minor.x = element_blank())
        gir <- girafe(ggobj = gg, options = list(opts_sizing(rescale = TRUE),
                                                 opts_hover(css = "r:2pt;")))
      })
      
      #### Grid of... ####
      
      ##### ...popularity ####
      ## func/gg-grid-popularity.R
      
      output$grid_popularity_plot <- renderGirafe({
        gg_grid_popularity(df_all, font_plot, switch = input$switch_grid_popularity)
      })
      
      ##### ...rank ####
      ## func/gg-grid-rank.R
      
      output$grid_rank_plot <- renderGirafe({
        gg_grid_rank(df_all, font_plot)
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
    } else if (list_all$error == "bad_request") {
      showNotification("Invalid cliend ID!", type = "error")
    } else if (list_all$error == "not_found") {
      showNotification("User not found!", type = "error")
    }
  }) ## End of the main observeEvent
}
