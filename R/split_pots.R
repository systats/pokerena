#' split_pots
#' @export
split_pots <- function(sess){

  dead_money <- sess %>%
    dplyr::filter(folded == 1) %>%
    dplyr::pull(t_stake) %>%
    sum

  pots <- sess %>%
    dplyr::filter(folded == 0) %>%
    dplyr::arrange(t_stake) %>%
    dplyr::mutate(
      pot_1st = min(t_stake),
      pot_2nd = t_stake - pot_1st,
      pot_3rd = t_stake - (pot_1st + pot_2nd),
      pot_4th = t_stake - (pot_1st + pot_2nd + pot_3rd)
    ) %>%
    dplyr::select(name, rank, pot_1st, pot_2nd, pot_3rd, pot_4th) %>%
    dplyr::mutate_at(-1:-2, ~ifelse(.x>0, sum(.x), 0)) %>%
    dplyr::mutate(pot_1st = pot_1st + dead_money)

  return(pots)
}

#' map_pots
#' @export
map_pots <- function(pots){
  pots %>%
    tidyr::gather(pot, value, -name, -rank) %>%
    dplyr::filter(value > 0) %>%
    split(.$pot) %>%
    purrr::map_dfr(~{
      prize <- .x %>%

        dplyr::filter(rank == min(rank, na.rm = T))
      if(nrow(prize) > 1){
        ### split pots
        prize <- prize %>% dplyr::mutate(value = value/nrow(prize))
      }
      return(prize)
    })
}

