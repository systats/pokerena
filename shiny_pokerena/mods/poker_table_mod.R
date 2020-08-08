poker_table_ui <- function(id){

  ns <- NS(id)

  tagList(
    div(class = "ui basic segment", style="background-image: url('images/poker_table.svg') !important; background-size: 100% 100%;background-repeat: no-repeat;background-color: #6a0000!important;",
    div(class = "ui three column grid",
        div(class = "column",
            seat_ui(ns("seat_1"))
        ),
        div(class = "column",
            seat_ui(ns("seat_2"))
        ),
        div(class = "column",
            seat_ui(ns("seat_3"))
        )
    ),
    div(class = "ui inverted header", textOutput(ns("pot")), align = "center", style = "margin:0;font-size:25px;"),
    uiOutput(ns("board")),
    div(class = "ui three column grid",
        div(class = "column",
            seat_ui(ns("seat_6"))
        ),
        div(class = "column",
            seat_ui(ns("seat_5"))
        ),
        div(class = "column",
            seat_ui(ns("seat_4"))
        )
    )
  ))
}


poker_table_server <- function(input, output, session, sess, players){


  output$board <- renderUI({

    b <- sess()$board[1]
    if(b != ""){
      board <- stringr::str_split(b, " ") %>% unlist %>%
        str_replace_all("10", "T") %>%
        stringr::str_replace_all("C", "c") %>%
        stringr::str_replace_all("S", "s") %>%
        stringr::str_replace_all("H", "h") %>%
        stringr::str_replace_all("D", "d")

      board_len <- length(board)
      if(board_len < 5) board <- c(board, rep("back", 5-board_len))
      board <- glue::glue("images/PokerCards/images/[104x146]/{board}@2x.png")

      div(class = "ui centered images", style = "padding-left:23%;",
        img(src=board[1], style = "width:60px;"),
        img(src=board[2], style = "width:60px;"),
        img(src=board[3], style = "width:60px;"),
        img(src=board[4], style = "width:60px;"),
        img(src=board[5], style = "width:60px;")
      )
    } else {
      tagList(
        br(), br(), br(), br(), br()
      )
    }
  })

  output$pot <- renderText({
    glue::glue("Pot: {sess()$pot[1]}")
  })

  # observe({
  #   players()
  #   1:6 %>%
  #     purrr::walk(~{
  #       shinyjs::reset(glue::glue("seat_{.x}"))
  #     })
  # })
  observe({
    req(players())
    1:6 %>%
      walk(~{
        callModule(seat_server, glue::glue("seat_{.x}"), df = reactive({ NULL }))
      })
  })

  observe({
    req(sess())
    sess() %>%
      # glimpse %>%
      layout_seatid() %>%
      # glimpse %>%
      split(.$seat_id) %>%
      iwalk(~{
        callModule(seat_server, glue::glue("seat_{.y}"), df = reactive({.x}))
      })
  })
}
