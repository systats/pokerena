#' poker game
#' @export
game <- R6::R6Class("poker_game",
  inherit = poker_session,
  active = list(
    set_input = function(x){
       self$input <- x
    }
  ),
  public = list(

    admin = NULL,
    input = NULL,

    players = NULL,
    session = NULL,
    events = NULL,
    event = NULL,
    result = NULL,

    deck = NULL,
    query = NULL,
    delay = 0,
    verbose = T,

    initialize = function(players, delay = 0){

       self$players <- players
       self$delay <- delay
       self$admin <- poker_admin$new()
       self$init_session()

    },
    
    vali_actions = function(state){
      actions <- c("fold", ifelse(state$to_call == 0, "check", "call"))
      if(state$credit > state$to_call & length(unique(self$query)) > 1) actions <- c(actions, "raise", "allin")
      return(actions)
    },

    validate_action = function(){
      a_ <- self$event

      ### 1: if previous raise -> check -> fold
      if(a_$to_call > 0 & a_$action == "check") self$event$action <- "fold"

      ### 2: if previous check -> call -> check
      if(a_$to_call == 0 & a_$action == "call") self$event$action <- "check"

      ### 3: if previous check -> raise > bb -> check
      if(a_$to_call == 0 & a_$action == "raise" & a_$chips < a_$bb) self$event$action <- "check"

      ### 4: if previous raise -> raise > bb -> fold
      if(a_$to_call > 0 & a_$action == "raise" & a_$chips < (a_$to_call + a_$bb)) self$event$action <- "fold"

      ### correct chips
      if(a_$action == "call") self$event$chips <- a_$to_call
      if(a_$action == "call" & a_$to_call > a_$credit) self$event$chips <- a_$credit
      if(a_$action %in% c("check", "fold")) self$event$chips <- 0
    },


    bind_action = function(.name){

      # if(self$delay > 0) Sys.sleep(runif(1, 1, self$delay))

      if(self$verbose){
        # cli::cli_alert_info("{paste0(.name, rep())} \t p{self$event$position} {self$event$action} {self$event$chips} {self$event$to_call} {self$event$pot} {self$event$hand} {self$event$board}")
        ph <- .name
        over <- round(10 - stringr::str_length(.name))
        side <- paste(rep(".", over/2), collapse = "")
        if(over > 0) ph <- paste0(side, .name, side, collapse = ".")
        if(over < 0) ph <- stringr::str_sub(ph, 1, 10)
        # if(length(ph) < 10) ph <- paste(ph, paste0(rep(".", (10-length(ph))), collapse = ""), collapse = "")
        action_color <- switch (self$event$action,
          fold = crayon::blue,
          call = crayon::green,
          check = crayon::green,
          raise = crayon::red,
          allin = crayon::red
        )
        cat(ph %+% "\t" %+% action_color(self$event$action), glue::glue("{self$event$chips} {self$event$to_call} {self$event$pot} \t {self$event$hand} {self$event$board}") %+% "\n")
      }

      self$events <- dplyr::bind_rows(self$events, self$event) %>%
        dplyr::mutate(
          chips = ifelse(credit <= chips, credit, chips),
          allin = ifelse(credit <= chips, 1, 0),
          folded = ifelse(action == "fold", 1, 0),
          stake = chips,
          t_stake_ = t_stake + stake,
          s_stake_ = s_stake + stake,
          credit_ = credit - stake,
          pot_ = pot + stake
        )

      latest <- tail(self$events, 1)
      self$set_name_value(.name, "chips", latest$chips)
      self$set_name_value(.name, "allin", latest$allin)
      self$set_name_value(.name, "folded", latest$folded)

      self$add_name_value(.name, "t_stake", latest$chips)
      self$add_name_value(.name, "s_stake", latest$chips)
      self$sub_name_value(.name, "credit", latest$chips)

      self$session$pot <- self$session$pot[1] + latest$chips
      self$session$to_call <- (max(self$session$t_stake) - (self$session$t_stake))
      self$session$to_call <- ifelse(self$session$allin == 1, 0, self$session$to_call)
      
      if(latest$allin == 1) self$set_name_value(.name, "to_call", 0)
      if(latest$folded == 1) self$set_name_value(.name, "to_call", 0)
      
      self$session$event_id <- self$session$event_id + 1
      self$add_name_value(.name, "action_id", 1)
      
      self$next_player()
      self$input <- NULL

      if(latest$action == "fold"){
        self$session$n_in <- self$session$n_in - 1
        self$query <- self$query %>% purrr::discard(~.x == .name)
      }

      if(latest$allin == 1){
        self$query <- self$query %>% purrr::discard(~.x == .name)
      }

      #readr::write_rds(self$session, path = "data/session.rds")
      #readr::write_rds(self$events, path = "data/events.rds")
    },

    get_action = function() {

      .name <- self$get_name()
      if(is.na(.name)){
        self$admin$next_task()
        return(NULL)
      }
      s_ <- self$get_state(.name)
      f_ <- self$get_fun(.name)

      self$event <- cbind(s_, f_(s_, input = self$input))
      self$validate_action()

      self$bind_action(.name)
    },

    step_action = function(step = NULL){
      if(is.null(step)){
        self$bet_actions()
      } else {
        self$get_action()
        stop("WAIT FOR NEXT")
      }
    },

    decide_next = function(stop){

      if(nrow(self$session) < 2){
        self$admin$finalize()
        return(T)
      }
      sess <- self$session %>% dplyr::filter(folded == 0) #%>% glimpse

      last_states <- self$events %>% filter(state == max(state))

      NO_BILLS <- sum(sess$to_call) == 0
      if(is.na(NO_BILLS)) NO_BILLS <- T

      PREFLOP <- sess$state[1] == 1
      ALL_ONCE <- all(sess$name %in% last_states$name)
      if(is.na(NO_BILLS)) ALL_ONCE <- T

      SHOWDOWN <- min(sess$credit, na.rm = T) == 0
      N_QUERY <- length(unique(self$query)) < 2

      BB_TWICE <- sum(self$events$position == 2) > 1
      PRE <- NO_BILLS & PREFLOP & BB_TWICE
      POST <- NO_BILLS & !PREFLOP & ALL_ONCE

      CLEAN_UP <- nrow(sess) == 1 # | N_QUERY
      DEAL_ROUNOUT <- NO_BILLS & ALL_ONCE & SHOWDOWN & N_QUERY
      # JUMP <- self$admin$this_

      if(DEAL_ROUNOUT){
        self$admin$next_task()
        self$deal_runout()
        return(T)
      }
      if(CLEAN_UP){
        self$admin$finalize()
        return(T)
      }
      if(PRE | POST){
        self$admin$next_task()
        return(T)
      }
      return(F)
    },

    bet_actions = function(){
      STOP <- F
      while(!STOP){
        self$get_action()
        STOP <- self$decide_next(STOP)
      }
    },

    eval_hands = function(){
      evals <- self$session %>%
        dplyr::filter(hand != "" & !is.na(hand)) %>%
        dplyr::distinct(name, hand, runout) %>%
        dplyr::mutate(out = stringr::str_split(glue::glue("{hand} {runout}"), " ") %>% purrr::map(get_parse_hand)) %>%
        tidyr::unnest(out) %>%
        get_hand_ranks() %>%
        dplyr::transmute(name, main, winner, rank)

      self$session <- self$session %>% dplyr::left_join(evals, by = "name")
    },

    set_winner = function(){

      self$session$runout <- self$session$board[1]
      self$events$runout <- self$session$board[1]

      active <- self$session %>%
        dplyr::filter(folded == 0) %>%
        dplyr::pull(name) %>%
        unique()

      FULL_BOARD <- length(stringr::str_split(self$session$runout[1], "\\s+")[[1]]) > 2

      if(FULL_BOARD){
        ### showdown hands
        self$eval_hands()
      }
      if(length(active) < 2 | length(self$session[["rank"]]) == 0) {
        self$session <- self$session %>%
          dplyr::mutate(
            winner = ifelse(name %in% active, 1, 0),
            rank = ifelse(name %in% active, 1, 2)
          )
      }
    },

    set_pots = function(){

      pots <- self$session %>%
        split_pots() %>%
        map_pots() %>%
        dplyr::group_by(name) %>%
        dplyr::summarise(ret = sum(value)) %>%
        dplyr::ungroup()

      if(length(self$session[["net"]]) > 0) return()

      self$session <- self$session %>%
        dplyr::left_join(pots, by = "name") %>%
        dplyr::mutate(
          ret = ifelse(is.na(ret), 0, ret),
          net = ret - t_stake
        )
    },

    finalize = function(){

      if(!is.null(self$events$winner)) return()
      
      ### Find Winners
      self$set_winner()
      
      ### Allocate Pots for multiple players at showdown
      self$set_pots()

      self$events <- self$events %>%
        dplyr::left_join(self$session %>% dplyr::select(name, winner, rank, ret, net), by = "name")

      # readr::write_rds(self$session, path = "data/session.rds")
      # readr::write_rds(self$events, path = "data/events.rds")

      winner <- self$events %>% dplyr::filter(winner == 1) %>% tail(1)
      if(nrow(winner) > 0) cli::cli_alert_success("winner is {winner$name} who collects {winner[['ret']]} chips ({winner$net} net return)")

      self$result <- self$players %>% dplyr::left_join(self$session %>% dplyr::select(name, winner, rank, ret, net), by = "name")
      self$players <- self$result %>% dplyr::transmute(name, fun, credit = credit + net)
      self$result <- self$result %>% dplyr::select(-fun)

    },

    run = function(verbose = F, step = NULL){

      self$verbose <- verbose

      if(self$admin$this_task()$state == 0){
        self$set_blinds()
        self$admin$next_task()
      }

      ### Preflop
      if(self$admin$this_task()$state == 1){
        if(self$admin$this_task()$task == "preflop_deal"){
          self$deal_preflop()
          # readr::write_rds(self$session, path = "data/session.rds")
          # readr::write_rds(self$events, path = "data/events.rds")
          self$admin$next_task()
        }
        if(self$admin$this_task()$task == "preflop_action"){
          self$step_action(step)
        }
      }


      ### Flop
      if(self$admin$this_task()$state == 2){
        if(self$admin$this_task()$task == "flop_deal"){
          self$deal_flop()
          self$admin$next_task()
        }
        if(self$admin$this_task()$task == "flop_action"){
          self$step_action(step)
        }
      }

      ### Turn
      if(self$admin$this_task()$state == 3){
        if(self$admin$this_task()$task == "turn_deal"){
          self$deal_turn()
          self$admin$next_task()
        }
        if(self$admin$this_task()$task == "turn_action"){
          self$step_action(step)
        }
      }
      ### River
      if(self$admin$this_task()$state == 4){
        if(self$admin$this_task()$task == "river_deal"){
          self$deal_river()
          self$admin$next_task()
        }
        if(self$admin$this_task()$task == "river_action"){
          self$step_action(step)
        }
      }

      ### Showdown
      if(self$admin$this_task()$state == 5){
        self$finalize()
      }
    },

    step = function(verbose = F, step = NULL){
      try(self$run(verbose, step), silent = T)
    },

    export = function(){
      return(
        dplyr::tibble(
          game_id = self$session$game_id[1],
          result = list(self$result),
          events = list(dplyr::as_tibble(self$events)),
          log = list(dplyr::as_tibble(self$log))
        )
      )
    }
  )
)


# self$ok(self$this_task())




