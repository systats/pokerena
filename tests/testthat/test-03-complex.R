
test_that("3 player dynamic", {

  player_1 <- function(s, ...){
    actions <- c("raise", "raise", "call", "fold")
    chips <- c(20, 20, 0, 0)
    return(dplyr::tibble(chips = chips[s$action_id], action = actions[s$action_id]))
  }
  
  player_2 <- function(s, ...){
    actions <- c("call", "call", "raise")
    chips <- c(s$to_call, s$to_call, 20)
    return(dplyr::tibble(chips = chips[s$action_id], action = actions[s$action_id]))
  }
  
  player_3 <- function(s, ...){
    actions <- c("fold")
    chips <- c(0)
    return(dplyr::tibble(chips = chips[s$action_id], action = actions[s$action_id]))
  }
  
  players <- rbind(
    tibble(name = "AA", fun = list(player_1)),
    tibble(name = "BB", fun = list(player_2)),
    tibble(name = "CC", fun = list(player_3))
  ) %>% mutate(credit = 100, bb = 2)

  g <- game$new(players, delay = 0)
  g$run(verbose = F)

  expect_equal(nrow(g$events), 10)
  expect_equal(g$session$pot[1], 102)
})