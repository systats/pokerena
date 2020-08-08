#' poker_admin
#' @export
poker_admin <- R6::R6Class("pokeradmin",
  public = list(

    log = NULL,

    history = NULL,
    tasks = NULL,
    open = NULL,

    initialize = function(tasks = NULL){
      #if(is.null(tasks)){
      self$log <- dplyr::tibble(state = 0, task = "Logging", status = 1)
        tasks <- rbind(
          dplyr::tibble(state = 0, task = "init"),
          dplyr::tibble(state = 1, task = "preflop_deal"),
          dplyr::tibble(state = 1, task = "preflop_action"),
          dplyr::tibble(state = 2, task = "flop_deal"),
          dplyr::tibble(state = 2, task = "flop_action"),
          dplyr::tibble(state = 3, task = "turn_deal"),
          dplyr::tibble(state = 3, task = "turn_action"),
          dplyr::tibble(state = 4, task = "river_deal"),
          dplyr::tibble(state = 4, task = "river_action"),
          dplyr::tibble(state = 5, task = "showdown")
        ) %>% dplyr::mutate(id = 1:n())
      #}
      self$tasks <- self$open <- tasks
    },

    this_task = function(){
      if(nrow(self$open) == 0) return(self$open)
      return(head(self$open, 1))
    },

    next_task = function(){
      # self$ok(task = glimpse(self$this_task()$task))

      if(nrow(self$open) == 0) return(self$open)
      self$history <- rbind(self$history, self$open[1, ])
      self$open <- self$open[-1, ]
    },

    finalize = function(){
      self$history <- rbind(self$open[-nrow(self$open), ], self$open[1, ])
      self$open <- self$open[nrow(self$open), ]
    },

    ok = function(task, status = NULL){
      if(is.null(status)) status <- 1
      new <- dplyr::tibble(
        state = self$session$state[1],
        task = task,
        status = status
      )

      self$log <- rbind(self$log, new)
    },
    error = function(task, status = NULL){
      if(is.null(status)) status <- 0
      new <- dplyr::tibble(
        state = self$session$state[1],
        task = task,
        status = status
      )
      self$log <- rbind(self$log, new)
    }
  )
)
