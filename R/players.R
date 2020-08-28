#' player_fold
#' @export
player_fold <- function(s, ...){
  return(dplyr::tibble(chips = 0, action = "fold"))
}

#' player_fish
#' @export
player_fish <- function(s, ...){
  return(dplyr::tibble(chips = s$to_call, action = "call"))
}

#' player_call
#' @export
player_call <- function(s, ...){
  return(dplyr::tibble(chips = s$to_call, action = "call"))
}

#' player_raise
#' @export
player_raise <- function(s, ...){
  return(dplyr::tibble(chips = s$to_call + 5, action = "raise"))
}

#' player_allin
#' @export
player_allin <- function(s, ...){
  return(dplyr::tibble(chips = s$credit, action = "raise"))
}

#' player_random
#' @export
player_random <- function(s, ...){
  action <- sample(c("fold", "call", "raise"), 1)
  chips <- 0
  if(action == "raise") chips <- 10
  if(s$to_call == 0) action <- "call"
  
  return(dplyr::tibble(chips, action))
}

#' player_promt
#' @export
player_promt <- function(s, ...){

  dplyr::glimpse(s)

  action <- menu(c("fold", "call", "raise"), title = "What action to perform?")

  if(action == 3){
    action <- "raise"
    chips <- as.numeric(readline("How much raise?"))
  } else if(action == 2){
    action <- "call"
    chips <- s$to_call
  } else {
    action <- "fold"
    chips <- 0
  }

  return(dplyr::tibble(chips, action))
}

#' player_app
#' @export
player_app <- function(s, ...){
  params <- list(...)
  inp <- params$input
  if(is.null(inp)) stop("wait for user input")
  return(inp)
}

#' player_api
#' @export
player_api <- function(s, ...){

  req <- httr::POST(url = "http://213.152.100.65:3838/our_bots", body = list(name = "potman", data = s), encode = "json")

  if(req$status_code == 500) stop("Bot action could not be retrieved")

  action <- jsonlite::fromJSON(rawToChar(req$content)) %>%
    dplyr::transmute(action = dplyr::case_when(action == 1 ~ "fold", action == 2 ~ "call", action == 3 ~ "raise"), chips)

  return(action)
}
