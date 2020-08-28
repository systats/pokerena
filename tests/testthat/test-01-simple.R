
test_that("2 player folding only", {

  players <- rbind(
    tibble(name = "fish2", fun = list(player_fold)),
    tibble(name = "fish", fun = list(player_fish))
  ) %>% mutate(credit = 100, bb = 2)

  g <- game$new(players, delay = 0)
  g$run(verbose = F)

  #g$events %>% View

  expect_equal(nrow(g$events), 3)
  expect_equal(g$session$pot[1], 3)
})


test_that("2 player checking only", {

  players <- rbind(
    tibble(name = "AA", fun = list(player_fish)),
    tibble(name = "BB", fun = list(player_fish))
  ) %>% mutate(credit = 100, bb = 2)

  g <- game$new(players, delay = 0)
  g$run(verbose = F)
  # g$result
  # g$session
  # g$events %>% View

  expect_equal(nrow(g$events), 10)
})

test_that("3 player checking only", {

  players <- rbind(
    tibble(name = "fish", fun = list(player_call)),
    tibble(name = "fish2", fun = list(player_call)),
    tibble(name = "fish3", fun = list(player_call))
  ) %>% mutate(credit = 100, bb = 2)

  g <- game$new(players, delay = 0)
  g$run(verbose = F)

  #g$events %>% View
  expect_equal(nrow(g$events), 14)
  expect_equal(max(g$events$pot), 6)
})


test_that("3 player raise fold fold", {
  
  players <- rbind(
    tibble(name = "AA", fun = list(player_fold)),
    tibble(name = "BB", fun = list(player_fold)),
    tibble(name = "CC", fun = list(player_raise))
  ) %>% mutate(credit = 100, bb = 2)
  
  g <- game$new(players, delay = 0)
  g$run(verbose = F)
  
  #g$events %>% View
  expect_equal(nrow(g$events), 5)
  expect_equal(max(g$events$pot), 10)
})


test_that("2 player allin", {

  players <- rbind(
    tibble(name = "caller", fun = list(player_call)),
    tibble(name = "aggr", fun = list(player_allin))
  ) %>% mutate(credit = 100, bb = 2)

  g <- game$new(players, delay = 0)
  g$run(verbose = F)

  expect_equal(nrow(g$events), 5)
  expect_equal(g$session$pot[1], 200)
})


test_that("2 player allin differnt stack sizes", {

  players <- rbind(
    tibble(name = "caller", fun = list(player_call)),
    tibble(name = "aggr", fun = list(player_allin))
  ) %>% mutate(credit = c(50, 100), bb = 2)

  g <- game$new(players, delay = 0)
  g$step(verbose = F)

  #g$events %>% View
  expect_equal(nrow(g$events), 5)
})



test_that("2 player raise (allin) differnt stack sizes", {

  players <- rbind(
    tibble(name = "aggr", fun = list(player_allin)),
    tibble(name = "caller", fun = list(player_call))
  ) %>% mutate(credit = c(100, 5), bb = 2)

  g <- game$new(players, delay = 0)
  g$run()

  expect_equal(nrow(g$events), 4)
  expect_equal(g$session$pot[1], 105)
})


