#' pockets
#' @export
pockets <- function(cards = Poker:::full_deck){
  
  ordering <- c("2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A")
  cards <- cards %>% stringr::str_replace_all("10", "T")
  
  tidyr::expand_grid(
    card1 = cards,
    card2 = cards
  ) %>%
  dplyr::mutate(
    value1 = stringr::str_remove(card1, ".$"),
    value2 = stringr::str_remove(card2, ".$"),
    value1 = factor(value1, ordering),
    value2 = factor(value2, ordering),
    id1 = as.numeric(as.factor(value1)),
    id2 = as.numeric(as.factor(value2)),
    hand = paste(card1, card2),
    suited = ifelse(stringr::str_detect(hand, "S .{1,2}S|C .{1,2}C|H .{1,2}H|D .{1,2}D"), 1, 0),
    suit1 = stringr::str_extract(card1, ".$"),
    suit2 = stringr::str_extract(card2, ".$"),
    suit_id1 = as.numeric(as.factor(suit1)),
    suit_id2 = as.numeric(as.factor(suit2))
  ) %>%
  dplyr::mutate(
    value1 = forcats::fct_reorder(value1, -id1),
    value2 = forcats::fct_reorder(value2, id2)
  ) %>%
  dplyr::filter(card1 != card2) %>%
  dplyr::filter(id1 < id2 & suited | id1 > id2 & !suited | (id1 == id2 & suit_id1 > suit_id2)) %>%
  dplyr::arrange(-id1, id2) %>%
  dplyr::distinct(hand, .keep_all = T)
  
}

#' pockets_both
#' @export
pockets_both <- function(){
 
 grid <- pockets()
 
 grid %>%  
  dplyr::rename(x = card1, card1 = card2) %>% 
  dplyr::rename(card2 = x) %>% 
  dplyr::bind_rows(grid)
}


