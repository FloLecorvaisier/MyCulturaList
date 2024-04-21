library(shiny)
library(shinydashboard)
library(httr)
library(shiny)
library(jsonlite)
library(ggplot2)
library(lubridate)

infobox_meta <- data.frame(val = c("cmpl", "cw", "ptw", "hld", "time", "eps"),
                           title = c("Complete", "Currently watching", "Plan to watch", "On hold", "Time", "Episodes"),
                           icon = c("flag-checkered", "eye", "clipboard-list", "circle-pause", "clock", "display"))

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
                box(textInput("user_mal", 
                              label = "User", 
                              value = "ScienceLeaf"),
                    actionButton("load_user", "Load user")),
                tabBox(
                  id = "valueBox", height = "280px",
                  tabPanel(
                    "General",
                    lapply(c("cmpl", "cw", "ptw", "hld", "time", "eps"), function(x) {
                      infoBox(
                        title = infobox_meta$title[infobox_meta$val == x],
                        value = htmlOutput(paste0("all_", x)),
                        icon = icon(infobox_meta$icon[infobox_meta$val == x]),
                        color = "aqua",
                        fill = TRUE)
                    })
                  ),
                  tabPanel(
                    "Movies",
                    lapply(c("cmpl", "cw", "ptw", "hld", "time", "eps"), function(x) {
                      infoBox(
                        title = infobox_meta$title[infobox_meta$val == x],
                        value = htmlOutput(paste0("movie_", x)),
                        icon = icon(infobox_meta$icon[infobox_meta$val == x]),
                        color = "lime",
                        fill = TRUE)
                    })
                  ),
                  tabPanel(
                    "TVs",
                    lapply(c("cmpl", "cw", "ptw", "hld", "time", "eps"), function(x) {
                      infoBox(
                        title = infobox_meta$title[infobox_meta$val == x],
                        value = htmlOutput(paste0("tv_", x)),
                        icon = icon(infobox_meta$icon[infobox_meta$val == x]),
                        color = "maroon",
                        fill = TRUE)
                    })
                  ),
                  tabPanel(
                    "OVAs",
                    lapply(c("cmpl", "cw", "ptw", "hld", "time", "eps"), function(x) {
                      infoBox(
                        title = infobox_meta$title[infobox_meta$val == x],
                        value = htmlOutput(paste0("ova_", x)),
                        icon = icon(infobox_meta$icon[infobox_meta$val == x]),
                        color = "orange",
                        fill = TRUE)
                    })
                  ),
                  tabPanel(
                    "ONAs",
                    lapply(c("cmpl", "cw", "ptw", "hld", "time", "eps"), function(x) {
                      infoBox(
                        title = infobox_meta$title[infobox_meta$val == x],
                        value = htmlOutput(paste0("ona_", x)),
                        icon = icon(infobox_meta$icon[infobox_meta$val == x]),
                        color = "navy",
                        fill = TRUE)
                    })
                  ),
                  tabPanel(
                    "Specials",
                    lapply(c("cmpl", "cw", "ptw", "hld", "time", "eps"), function(x) {
                      infoBox(
                        title = infobox_meta$title[infobox_meta$val == x],
                        value = htmlOutput(paste0("special_", x)),
                        icon = icon(infobox_meta$icon[infobox_meta$val == x]),
                        color = "olive",
                        fill = TRUE)
                    })
                  )
                )
              ),
              fluidRow(
                box(plotOutput("seasonplot")),
                box(plotOutput("scoreplot"))
              )
      )
    )
  )
)

