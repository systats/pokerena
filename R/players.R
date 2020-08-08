#' player_fold
#' @export
player_fold <- function(state_, ...){
  return(dplyr::tibble(chips = 0, action = "fold"))
}

#' player_fish
#' @export
player_fish <- function(state_, ...){
  return(dplyr::tibble(chips = state_$to_call, action = "call"))
}

#' player_call
#' @export
player_call <- function(state_, ...){
  return(dplyr::tibble(chips = state_$to_call, action = "call"))
}

#' player_raise
#' @export
player_raise <- function(state_, ...){
  return(dplyr::tibble(chips = state_$to_call + 5, action = "raise"))
}

#' player_allin
#' @export
player_allin <- function(state_, ...){
  return(dplyr::tibble(chips = state_$credit, action = "raise"))
}

#' player_random
#' @export
player_random <- function(state_, ...){
  action <- sample(c("fold", "call", "raise"), 1)
  chips <- 0
  if(action == "raise") chips <- 10
  return(dplyr::tibble(chips, action))
}

#' player_promt
#' @export
player_promt <- function(state_, ...){

  dplyr::glimpse(state_)

  action <- menu(c("fold", "call", "raise"), title = "What action to perform?")

  if(action == 3){
    action <- "raise"
    chips <- as.numeric(readline("How much raise?"))
  } else if(action == 2){
    action <- "call"
    chips <- state_$to_call
  } else {
    action <- "fold"
    chips <- 0
  }

  return(dplyr::tibble(chips, action))
}

#' player_app
#' @export
player_app <- function(state_, ...){
  params <- list(...)
  inp <- params$input
  if(is.null(inp)) stop("wait for user input")
  return(inp)
}

#' player_api
#' @export
player_api <- function(state_, ...){

  req <- httr::POST(url = "http://213.152.100.65:3838/our_bots", body = list(name = "potman", data = state_), encode = "json")

  if(req$status_code == 500) stop("Bot action could not be retrieved")

  action <- jsonlite::fromJSON(rawToChar(req$content)) %>%
    dplyr::transmute(action = dplyr::case_when(action == 1 ~ "fold", action == 2 ~ "call", action == 3 ~ "raise"), chips)

  return(action)
}
