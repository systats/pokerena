poker_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(class = "ten wide column",
      tabset(
        tabs = list(
          list(menu = "Table", content = poker_table_ui(ns("table"))),
          list(menu = "Events", content = DT::DTOutput(ns("events"))),
          list(menu = "Session", content = DT::DTOutput(ns("session"))),
          list(menu = "Result", content = DT::DTOutput(ns("result")))
        )
        # active = "second_tab",
        # id = "exampletabset"
      )
      # div(class = "ui header", "Feed"),
      # event_feed_ui(ns("feed"))
    ),
    div(class = "six wide column",
        tabset(
          tabs = list(
            list(menu = "Input", content = tagList(
                actionButton(ns("reload"), "reload", class = "ui button"),
                actionButton(ns("nex"), "next", class = "ui button"),
                br(),
                br(),

                div(class = "ui buttons",
                    actionButton(ns("fold"), "Fold", class = "ui blue button"),
                    actionButton(ns("call"), "Call", class = "ui green button"),
                    actionButton(ns("raise"), "Raise", class = "ui orange button"),
                    actionButton(ns("allin"), "Allin", class = "ui red button")
                ),
                br(),
                br(),

                sliderInput(ns("chips"), label = "", min = 0, max = 100, valu = 0),

                verbatimTextOutput(ns("dev"))
              )
            ),
            list(menu = "Config", content = player_ui(ns("pl")))
          )
        )
    ),
    verbatimTextOutput(ns("results"))
  )
}


layout_seatid <- function(df){

  n_player <- df$n_player[1]

  if(n_player == 2){
    out <- df %>% mutate(seat_id = case_when(seat_id == 1 ~ 2, seat_id == 2 ~ 5))
  }
  if(n_player == 3){
    out <- df %>% mutate(seat_id = case_when(seat_id == 1 ~ 2, seat_id == 2 ~ 4, seat_id == 3 ~ 6))
  }
  if(n_player == 4){
    out <- df %>% mutate(seat_id = case_when(seat_id == 1 ~ 1, seat_id == 2 ~ 3, seat_id == 3 ~ 4, seat_id == 4 ~ 6))
  }
  if(n_player == 5){
    out <- df %>% mutate(seat_id = case_when(seat_id == 5 ~ 6, T ~ seat_id))
  }
  if(n_player == 6){
    out <- df
  }
  return(out)
}

poker_server <- function(input, output, session){


  players <- callModule(player_server, "pl")

  observe({
    game$players <- players()
  })

  game <- reactiveValues(state = NULL, players = NULL)

  observe({
    input$reload
    players()

    game$state <- poker_game$new(game$players, delay = 0)
    game$state$step()
  })

  observeEvent(input$nex, {
    game$players <- game$state$players %>% mutate(bb = 2)
  })

  observeEvent(input$fold, {
    game$state$set_input <- dplyr::tibble(chips = input$chips, action = "fold")
    game$state$step()
  })

  observeEvent(input$call, {

    game$state$set_input <- dplyr::tibble(chips = input$chips, action = "call")
    game$state$step()
  })

  observeEvent(input$raise, {

    game$state$set_input <- dplyr::tibble(chips = input$chips, action = "raise")
    game$state$step()
  })

  observeEvent(input$allin, {

    game$state$set_input <- dplyr::tibble(chips = input$chips, action = "raise")
    game$state$step()
  })

  output$dev <- renderPrint({
    shiny::invalidateLater(2000)
    dplyr::glimpse(game$state$get_state("me"))
  })

  events <- reactive({
    shiny::invalidateLater(2000)
    readr::read_rds("data/events.rds") %>%
      dplyr::select(name, action, to_call, chips, t_stake, pot, credit, hand, board, state)
  })

  fun1 <- function(){
    readr::read_rds("data/session.rds")
  }
  fun1_pos <- purrr::possibly(fun1, NULL)
  sess <- reactive({
    req(events())

    shiny::invalidateLater(3000)

    tar <- events() %>%
      group_by(name) %>%
      filter(state == max(state)) %>%
      slice(n()) %>%
      ungroup %>%
      transmute(name, last_action = action, last_chips = chips)


    d <- fun1_pos()
    if(!is.null(game$state$result)){
      d$show <- 1
    } else {
      d$show <- 0
    }
    d %>% left_join(tar, by = "name")
  })

  output$session <- DT::renderDT({
    req(sess())
    sess()  %>%
      dplyr::transmute(name, tc = to_call, ch  = chips, ts = t_stake, po = pot, cr = credit, hand, board, st = state)
  })

  output$events <- DT::renderDT({
    events() %>%
      dplyr::transmute(name, a = action, tc = to_call, ch  = chips, ts = t_stake, po = pot, cr = credit, hand, board, st = state) %>%
      DT::datatable(options = list(pageLength = 100))
  })

  output$result <- DT::renderDT({
    shiny::invalidateLater(3000)
    game$state$result
  })

  callModule(poker_table_server, "table", sess, players)

  shiny::outputOptions(output, "events", suspendWhenHidden = F)
  shiny::outputOptions(output, "session", suspendWhenHidden = F)

  observe({
    shiny::invalidateLater(2000)

    if(!is.null(game$state$result)){
      d <- game$state$result %>%
        dplyr::mutate(n_player = dplyr::n(), seat_id = 1:dplyr::n()) %>%
        layout_seatid() %>%
        dplyr::filter(net > 0) %>%
        dplyr::distinct(seat_id, name, net)
      shinyjs::runjs(glue::glue("$('#table-table-seat_{d$seat_id}-dimmer').dimmer('show');"))
      Sys.sleep(1)
      shinyjs::runjs(glue::glue("$('#table-table-seat_{d$seat_id}-dimmer').dimmer('hide');"))
    }
  })
}
