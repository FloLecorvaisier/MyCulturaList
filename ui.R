library(shiny)
library(shinydashboard)
library(httr)
library(shiny)
library(jsonlite)
library(ggplot2)
library(lubridate)
library(extrafont)
loadfonts(quiet = T)

infobox_meta <- data.frame(val = c("completed", "watching", "plan_to_watch", "on_hold", "time", "eps"),
                           title = c("Complete", "Currently watching", "Plan to watch", "On hold", "Time", "Episodes"),
                           icon = c("flag-checkered", "eye", "clipboard-list", "circle-pause", "clock", "display"))
infobox_meta2 <- data.frame(title = c("General", "Movies", "TVs", "OVAs", "ONAs", "Specials"),
                            type = c("general", "movie", "tv", "ova", "ona", "special"),
                            color = c("aqua", "lime", "maroon", "orange", "navy", "olive"))


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
                do.call(tabBox, c(id = "valueBox", height = "280px", lapply(infobox_meta2$type, function(i) {
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
                }))),
              ),
              fluidRow(
                box(plotOutput("seasonplot")),
                box(plotOutput("scoreplot"))
              ),
              fluidRow(
                box(plotOutput("studioplot"))
              )
      )
    )
  )
)

