library(shiny)
library(shinydashboard)
library(httr)
library(shiny)
library(jsonlite)
library(ggplot2)

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
              ),
              fluidRow(
                box(plotOutput("seasonplot"))
              )
      )
    )
  )
)
