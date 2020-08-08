seat_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("card"))
  )
}


seat_server <- function(input, output, session, df){

  output$card <- renderUI({
    req(df())
    cards <- df()$hand %>%
      stringr::str_split(" ") %>% unlist %>%
      stringr::str_replace_all("10", "T") %>%
      stringr::str_replace_all("C", "c") %>%
      stringr::str_replace_all("S", "s") %>%
      stringr::str_replace_all("H", "h") %>%
      stringr::str_replace_all("D", "d")

    w <- "top"
    col <- df() %>% dplyr::mutate(col = dplyr::case_when(last_action == "fold" ~ "blue", last_action %in% c("check", "call") ~ "green", last_action == "raise" ~ "red", T ~ "grey")) %>% pull(col)

    card_ui <- function(card){
      div(class="ui fade reveal",
        div(class="visible content",
          img(src=glue::glue("images/PokerCards/images/[104x146]/back@2x.png"), style = "width:70px;")
        ),
        div(class="hidden content",
          img(src=glue::glue("images/PokerCards/images/[104x146]/{card}@2x.png"), style = "width:70px;")
        )
      )
    }

    where <- ifelse(df()$seat_id > 3, "top", "bottom")
    stakes <- div(class = glue::glue("ui {col} inverted header"), df()$last_action, df()$last_chips, glue::glue("({df()$s_stake}/{df()$t_stake})"), style = "font-size:25px;")

    tagList(
      if(where == "top") stakes,
      div(class = "ui card", style = "background-color:transparent",
          div(class = "ui basic segment", style = "margin-bottom:-10px;",
              div(class = "ui two column grid",
                  div(class = "column",
                      #if(df()$last_action != "fold")
                      if(df()$name == "me" | df()$show == 1){
                        img(src=glue::glue("images/PokerCards/images/[104x146]/{cards[1]}@2x.png"), style = "width:70px;")
                      } else{
                        card_ui(cards[1])
                      }
                  ),
                  div(class = "column",
                      if(df()$name == "me" | df()$show == 1){
                        img(src=glue::glue("images/PokerCards/images/[104x146]/{cards[2]}@2x.png"), style = "width:70px;")
                      } else{
                        card_ui(cards[2])
                      }
                  )
              ),
              div(class="ui inverted winner_animation dimmer", id = session$ns("dimmer"),
                  img(src = "trophy2.png", class = "ui image", id = session$ns("winner"))
              )
          ),
          div(class = "extra content",
              div(class = "ui header", glue::glue("{df()$name} ({df()$credit})"), style ="color:white;")
          )
      ),
      if(where == "bottom") stakes
    )

  })
}




