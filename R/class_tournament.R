#' poker tournament
#' @export
tournament <- R6::R6Class("poker_tournament",

  public = list(

    players = NULL,
    config = NULL,

    BB = NULL,
    SBB = NULL,
    N_ROUND = 1,

    games = NULL,
    result = NULL,

    initialize = function(players, config){

      self$players <- players
      self$config <- config

      if(!is.null(config$bb)){
        self$players$bb <- self$BB <- config$bb
      } else {
        bb <- self$players[["bb"]][1]
        if(is.null(bb)) bb <- 2
        self$BB <- bb
      }
      if(!is.null(config$crdit)){
        self$players$credit <- config$credit
      }

      self$SBB <- self$BB
      if(!is.null(config$credit)) self$players$credit <- config$credit
      if(is.null(config$max_rounds)) self$config$max_rounds <- 50
    },

    increase_blinds = function(){

      type <- self$config$bb_slope

      if(type == "linear"){
        self$BB <- self$BB + self$SBB
      }
      if(type == "exp"){
        self$BB <- self$BB ^ 2
      }
    },

    rotate_dealer = function(){
      self$players <- rbind(self$players[-1, ], self$players[1, ])
    },

    count = function(){
      self$N_ROUND <- self$N_ROUND + 1
    },

    run = function(verbose = F){

      while(self$N_ROUND <= self$config$max_round){

        ### play one game
        p <- game$new(self$players, delay = 0)
        p$run(verbose)

        self$games <- rbind(self$games, p$export())

        ### who is still alive?
        self$players <- p$players %>% dplyr::filter(credit > 0)

        ### stop tournament if no active player remains
        if(nrow(self$players) < 2) self$N_ROUND <- self$config$max_round + 1

        ### increase  blind levels
        if(self$N_ROUND %% self$config$bb_round == 0) self$increase_blinds()
        self$players$bb <- self$BB

        self$rotate_dealer()
        self$count() # N_ROUND
      }
    }
  )
)
