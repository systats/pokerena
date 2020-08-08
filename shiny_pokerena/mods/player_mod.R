init <- 'rbind(
      # tibble(name = "aaaaa", fun = list(player_fish)),
      # tibble(name = "yyyyy", fun = list(player_random)),
      # tibble(name = "meeeee", fun = list(player_app))
        tibble(name = "potman", fun = list(player_api)),
        tibble(name = "me", fun = list(player_app))
    ) %>% mutate(credit = 100, bb = 2)'


player_ui <- function(id){
  ns <- NS(id)
  tagList(
    aceEditor(ns("code"), mode = "r", value = init),
    br(),
    verbatimTextOutput(ns("dev"))
  )
}

player_server <- function(input, output, session){

  players <- reactive({
    req(input$code)
    eval(parse(text = isolate(input$code)))
  })

  output$dev <- renderPrint({
    players() %>% glimpse
  })

  return(players)
}
