
#' get_parse_hand
#' @export
get_parse_hand <- function(cards){

  c1 <- jsonlite::toJSON(list(cards))
  h1 <- Poker::parse_cards(c1)
  t1 <- Poker::categorize_hand(h1)

  main <- t1$category$name
  outcome <- format(t1[["category"]])
  ca <- format(h1)
  target <- t1$category$cards %>% purrr::map(format) %>% unlist
  kicker <- t1$category$kickers %>% purrr::map(format) %>% unlist %>% paste(collapse = " ")

  if(!any(cards[1:2] %in% target)){
    dplyr::tibble(cards = paste(cards, collapse = " "), main = "Board Lead", target = paste(target, collapse = " "), kicker = "")
  } else {
    dplyr::tibble(cards = paste(cards, collapse = " "), main, target = paste(target, collapse = " "), kicker)
  }
}

#' get_hand_ranks
#' @export
get_hand_ranks <- function(.x){
  prep_rank_hand <- function(cards){
    c1 <- jsonlite::toJSON(list(cards))
    h1 <- Poker::parse_cards(c1)
    t1 <- Poker::categorize_hand(h1)
  }
  cards <- .x$cards %>% stringr::str_split(" ")
  rank_outcome <- cards %>% purrr::map(prep_rank_hand)
  o <- Poker::order_hands(rank_outcome)
  r <- Poker::rank_hands(rank_outcome)
  outcome <- cbind(.x)[o,]
  outcome$rank <- r
  outcome$winner <- ifelse(outcome$rank == 1, 1, 0)
  return(outcome)
}
