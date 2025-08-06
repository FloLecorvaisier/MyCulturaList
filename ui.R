library(shiny)
library(shinydashboard)
library(httr)
library(shiny)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(extrafont)
library(shinyWidgets)
library(ggiraph)
library(dplyr)

loadfonts(quiet = T)

infobox_meta <- data.frame(val = c("completed", "watching", "plan_to_watch", "on_hold", "time", "eps"),
                           title = c("Complete", "Currently watching", "Plan to watch", "On hold", "Time", "Episodes"),
                           icon = c("flag-checkered", "eye", "clipboard-list", "circle-pause", "clock", "display"))
infobox_meta2 <- data.frame(title = c("General", "Movies", "TVs", "OVAs", "ONAs", "Specials", "TV Specials", "CMs", "PVs", "Musics", "Unknown"),
                            type = c("general", "movie", "tv", "ova", "ona", "special", "tv_special", "cm", "pv", "music", "unknown"),
                            color = c("aqua", "lime", "maroon", "orange", "navy", "olive", "purple", "teal", "green", "fuchsia", "red"))

## The file containing the lists that can be finished
lists <- read.table("lists.txt", sep = "\t")
colnames(lists) <- c("list", "id")

image_urls <- NULL
# image_urls <- c("https://cdn.myanimelist.net/images/anime/1136/100516.jpg", "https://cdn.myanimelist.net/images/anime/1791/100508.jpg")

opts_sizing(rescale = T)

ui <- dashboardPage(
  dashboardHeader(title = "MyCulturaList"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("MyAnimeList", 
               tabName = "mal", 
               icon = icon("users"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "mal",
              fluidRow(
                box(width = 3, textInput("user_mal",
                                         label = "User",
                                         value = "ScienceLeaf"),
                    actionButton("load_user", "Load user")),
                box(width = 3,
                  checkboxGroupInput("listType", "Type(s) to show",
                                     c("Movies" = "movie", "TVs" = "tv", "ONAs" = "ona", "OVAs" = "ova", "Specials" = "special",
                                       "TV Specials" = "tv_special", "CMs" = "cm", "PVs" = "pv", "Musics" = "music", "Unknown" = "unknown"), 
                                     selected = c("tv", "movie", "ona", "ova", "special", "tv_special")),
                ),
                box(width = 3, title = "Quantitative filters",
                    sliderInput("listYear", "Year(s) to show", 1917, year(today()), value = c(1917, year(today())), dragRange = T, sep = ""),
                    sliderInput("listScore", "User score(s) to show (WIP)", 1, 10, value = c(1, 10), dragRange = T, sep = ""),
                    sliderInput("listMean", "MAL mean(s) to show (WIP)", 1, 10, .2, value = c(1, 10), dragRange = T, sep = "")
                )
              ),
              fluidRow(
                do.call(tabBox, c(id = "valueBox", height = "310px", width = 12, lapply(infobox_meta2$type, function(i) {
                  tabPanel(
                    infobox_meta2$title[infobox_meta2$type == i],
                    lapply(infobox_meta$val, function(x) {
                      infoBox(
                        title = infobox_meta$title[infobox_meta$val == x],
                        value = htmlOutput(paste(x, i, sep = "|")),
                        icon = icon(infobox_meta$icon[infobox_meta$val == x]),
                        color = infobox_meta2$color[infobox_meta2$type == i],
                        fill = TRUE)
                    })
                  )
                })))
              ),
              fluidRow(
                box(girafeOutput("n_season_plot", height = "100%")),
                box(h3("List of animes in the selection"),
                    style = "max-height: 610px; overflow-y: auto;", 
                    htmlOutput("list_of_animes"))
              ),
              fluidRow(
                box(width = 4, checkboxInput("switch_studio", "Polar coordinates"), girafeOutput("n_studio_plot", height = "100%")),
                box(width = 4, checkboxInput("switch_score", "Polar coordinates"), girafeOutput("n_score_plot", height = "100%")),
                box(width = 4, girafeOutput("n_genres_plot", height = "100%"))
              ),
              fluidRow(
                box(width = 4, girafeOutput("score_studio_plot", height = "100%")),
                box(width = 4, girafeOutput("score_genres_plot", height = "100%"))
              ),
              fluidRow(
                box(width = 4, girafeOutput("scoremean_plot", height = "100%"))
              ),
              fluidRow(
                box(width = 6, girafeOutput("grid_popularity_plot", height = "100%")),
                box(width = 6, girafeOutput("grid_rank_plot", height = "100%"))
              ),
              
              ### Lists to achieve ####
              
              h1("Lists of animes to complete", style = "font-family: Aleo"),
              selectInput("selectList", "Select a list", unique(lists$list), width = "600px"), 
              progressBar("pBlists", value = 0),
              fluidPage(
                # Ajout de styles pour aligner les images côte à côte
                tags$style(
                  HTML("
                    .image-container {
                      display: flex;
                      flex-wrap: wrap;
                    }
                    .image-container img {
                      width: 180px;
                      height: 255px;
                    }
                  ")
                ),
                h2("Watched", style = "font-family: Aleo"),
                uiOutput("image_gallery_watched"),
                h2("Not watched", style = "font-family: Aleo"),
                uiOutput("image_gallery_not_watched")
              )
      ) ## End of the tabItem()
    ) ## End of the tabItems()
  ) ## End of the dashboardBody()
)

