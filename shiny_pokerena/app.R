# remotes::install_github("Appsilon/shiny.semantic@develop")
# devtools::install_github("trestletech/shinyAce")
# pacman::p_load(tidyverse, purrr, shiny, shiny.semantic, semantic.dashboard, shinyAce)

library(tidyverse)
library(purrr)
library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(shinyAce)
library(Poker)
library(R6)
library(lubridate)
library(nanotime)

dir("mods", full.names = T) %>% walk(source)
dir("../R", full.names = T) %>% walk(source)

if(!dir.exists("data")) dir.create("data")

ui <- dashboardPage(
  dashboardHeader(color = "inverted", shinyjs::useShinyjs()),
  dashboardSidebar(
    side = "left", size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "table", "Table")
      #menuItem(tabName = "explore", "Explorer"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "table", poker_ui("table"))
      #tabItem(tabName = "explore", explore_ui("exp"))
    )
  )
)


server <- function(input, output, session) {

  callModule(poker_server, "table")

}

shinyApp(ui, server)
