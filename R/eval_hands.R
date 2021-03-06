#' get_starting_hands
#' @export
get_starting_hands <- function(n_player){
  game_id <- deeplyr::silently(round(as.numeric(nanotime::nanotime(Sys.time())), 0))
  sample(Poker:::full_deck, size = n_player*2, replace = F) %>%
    split(0:(length(.)-1) %/% 2 + 1) %>%
    purrr::map(paste, collapse = " ") %>%
    purrr::imap_dfr(~{ tidyr::expand_grid(game_id, n_player, state = 1, hand = .x, board = "") })
}

#' prep_rank_hand
#' @export
prep_rank_hand <- function(cards){
  c1 <- jsonlite::toJSON(list(cards))
  h1 <- Poker::parse_cards(c1)
  t1 <- Poker::categorize_hand(h1)
}

#' get_pair_ranks
#' @export
get_pair_ranks <- function(.state){
  out<-.state %>%
    tidyr::separate(cards, sep = " ", into = c("card1", "card2"), remove = F) %>%
    dplyr::left_join(pokerena::pairranks, by = c("card1", "card2")) %>%
    dplyr::transmute(
      cards,
      # outcome = "",
      rank = as.numeric(as.factor(126 - score)),
      winner = as.numeric(rank == 1),
      main = ifelse(pair, "One Pair", "High Card")
      #main = ifelse(winner == 1 | main == "One Pair", main, "Board Lead")
    )
  .state %>% dplyr::left_join(out, by = "cards")
}

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
    dplyr::tibble(cards = paste(cards, collapse = " "), main = "Board Lead") #, target = paste(target, collapse = " "), kicker = ""
  } else {
    dplyr::tibble(cards = paste(cards, collapse = " "), main) #, target = paste(target, collapse = " "), kicker
  }
}

#' get_hand_ranks
#' @export
get_hand_ranks <- function(.x){
  
  #.x$cards <- .x$cards %>% str_replace_all("T", "10")
  
  if(.x$state[1] == 1)  return(get_pair_ranks(.x))
  
  cards <- .x$cards %>% stringr::str_split(" ")
  
  #if(length(cards[[1]]) < 3) return(get_pair_ranks(.x))
  
  outcome <- cards %>% purrr::map_dfr(get_parse_hand) %>% select(-cards)
  rank_outcome <- cards %>% purrr::map(prep_rank_hand)
  
  o <- Poker::order_hands(rank_outcome)
  r <- Poker::rank_hands(rank_outcome)
  outcome <- cbind(.x, outcome)[o,]# %>% dplyr::mutate(hand = hand %>% stringr::str_replace_all("T", "10"))
  outcome$rank <- r
  outcome$winner <- ifelse(outcome$rank == 1, 1, 0)
  return(outcome)
}

