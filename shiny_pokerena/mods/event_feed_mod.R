event_feed_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("feed"))
  )
}

event_feed_server <- function(input, output, session, events){
  output$feed <- renderUI({
    req(events())
    div(class="ui feed",
        events() %>%
          split(1:nrow(.)) %>%
          purrr::map(~{
            div(class="event",
                # div(class = "label",
                #     icon("user")
                # ),
                div(class="content",
                    div(class="summary",
                        .x$name, .x$action, "(", .x$chips, ")"
                    )
                )
            )
          })
    )
  })
}
