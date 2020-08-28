#' poker_session
#' @export
poker_session = R6::R6Class("session",
  public = list(

    init_session = function(){

      self$session <- dplyr::tibble(
        game_id = as.character(suppressWarnings(round(as.numeric(nanotime::nanotime(Sys.time()))))),
        time = as.character(Sys.time()),
        event_id = 0,
        action_id = 0
      ) %>%
      cbind(., self$players %>% dplyr::select(-fun)) %>%
      dplyr::mutate(
        state = 1,
        seat_id = 1:dplyr::n(),
        position = 1:dplyr::n(),
        chips = dplyr::case_when(position == 1 ~ bb/2, position == 2 ~ bb, T ~ 0),
        to_call = bb, s_stake = 0, t_stake = 0, pot = 0,
        t_stake_ = chips,
        s_stake_ = max(chips),
        credit_ = credit-chips,
        pot_ = cumsum(chips),
        allin = 0, folded = 0,
        n_player = dplyr::n(),
        n_in = dplyr::n(),
        hand = "", board = ""
      )
    },

    get_values = function(var){
      d <- self$session[[var]]
      ifelse(is.na(d), 0, d)
    },

    next_player = function(){
      self$query <- self$query[-1]
    },

    get_name = function(){
      return(self$query[1])
    },

    get_player = function(.name){
      self$players[self$players$name == .name, ]
    },

    get_fun = function(.name){
      self$players[self$players$name == .name, ]$fun[[1]]
    },

    get_state = function(.name){
      self$session %>% dplyr::select(-chips) %>% dplyr::filter(name == .name)
    },

    set_name_value = function(.name, var, value){
      self$session[[var]][self$session$name == .name] <- value
    },

    get_name_value = function(.name, var){
      d <- self$session[[var]][self$session$name == .name]
      ifelse(is.na(d), 0, d)
    },

    add_name_value = function(.name, var, value){
      self$session[[var]][self$session$name == .name] <- self$session[[var]][self$session$name == .name] + value
    },

    sub_name_value = function(.name, var, value){
      self$session[[var]][self$session$name == .name] <- self$session[[var]][self$session$name == .name] - value
    },

    deal_pocket = function(){
      hands <- sample(Poker:::full_deck, nrow(self$players)*2)
      self$deck <- setdiff(Poker:::full_deck, hands)
      self$session <- self$session %>%
        dplyr::mutate(hand = hands %>% split((1:length(.)-1) %/% 2) %>% purrr::map_chr(paste, collapse = " "), board = "")
    },

    deal_board = function(k = 3){
      cards <- sample(self$deck, k)
      self$deck <- setdiff(self$deck, cards)
      .board <- stringr::str_squish(paste(c(self$session$board[1], cards), collapse = " "))
      self$session <- self$session %>%
        dplyr::mutate(board = .board)
    },

    reset_query = function(){
      sess <- self$session %>% dplyr::filter(allin == 0 & folded == 0)
      if(nrow(sess) < 2) return()
      self$query <- c(sess$name, sess$name, sess$name, sess$name, sess$name)
    },

    init_query = function(){
      sess <- self$session
      self$query <- c(sess$name[-(1:2)], sess$name, sess$name, sess$name, sess$name)
    },

    set_blinds = function(){

      self$events <- self$session[1:2, ] %>%
        dplyr::mutate(action = c("sb", "bb"))

      self$add_name_value(self$session$name[1], "t_stake", self$session$bb[1]/2)
      self$add_name_value(self$session$name[2], "t_stake", self$session$bb[2])
      self$add_name_value(self$session$name[1], "s_stake", self$session$bb[1]/2)
      self$add_name_value(self$session$name[2], "s_stake", self$session$bb[2])
      self$sub_name_value(self$session$name[1], "credit", self$session$bb[1]/2)
      self$sub_name_value(self$session$name[2], "credit", self$session$bb[2])

      self$session$pot <- (self$session$bb[1]/2 + self$session$bb[1])
      self$session$to_call <- (max(self$session$t_stake_) - (self$session$t_stake))
      self$session$event_id <- 1
      self$session$action_id <- 1
      
    },

    deal_preflop = function(){
      self$deal_pocket()
      self$init_query()
    },

    deal_flop = function(){
      self$deal_board(3)
      self$session$state <- 2
      self$session$s_stake <- 0
      self$reset_query()
    },

    deal_turn = function(){
      self$deal_board(1)
      self$session$state <- 3
      self$session$s_stake <- 0
      self$reset_query()
    },

    deal_river = function(){
      self$deal_board(1)
      self$session$state <- 4
      self$session$s_stake <- 0
      self$reset_query()
    },

    deal_runout = function(){

      if(self$admin$this_task()$state == 2){
        self$deal_board(3)
        self$admin$next_task()
        self$admin$next_task()
      }
      if(self$admin$this_task()$state == 3){
        self$deal_board(1)
        self$admin$next_task()
        self$admin$next_task()
      }
      if(self$admin$this_task()$state == 4){
        self$deal_board(1)
        self$admin$next_task()
        self$admin$next_task()
      }
    }
  )
)
