#' valuedict
# valuedict <- dplyr::tibble(
#   value = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"),
#   id = 1:13
# )
# usethis::use_data(valuedict)
"valuedict"


#' pairranks
# pairranks <- tidyr::expand_grid(
#   card1 = Poker:::full_deck,
#   card2 = Poker:::full_deck
# ) %>%
#   dplyr::filter(card1 != card2) %>%
#   dplyr::mutate(value1 = stringr::str_remove(card1, ".$"), value2 = stringr::str_remove(card2, ".$")) %>%
#   dplyr::mutate(suit1 = stringr::str_extract(card1, ".$"), suit2 = stringr::str_extract(card2, ".$")) %>%
#   dplyr::mutate(pair = as.numeric(value1 == value2) * 100, suit = as.numeric(suit1 == suit2)) %>%
#   dplyr::left_join(valuedict %>% dplyr::rename_all(~paste0(.x, 1))) %>%
#   dplyr::left_join(valuedict %>% dplyr::rename_all(~paste0(.x, 2))) %>%
#   dplyr::mutate(id_max = ifelse(id1 >= id2, id1, id2)) %>%
#   dplyr::mutate(score = ifelse(pair, pair + id1 + id2, id_max)) %>%
#   dplyr::arrange(dplyr::desc(score))
# usethis::use_data(pairranks)
"pairranks"


