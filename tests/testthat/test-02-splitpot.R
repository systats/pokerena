context("Split pots")

test_that("allin equal split pot", {

  sess <- tibble(
    name = c("a", "b", "c"),
    t_stake = c(10, 60, 60),
    folded = c(1, 0, 0),
    allin = c(0, 0, 1),
    rank = c(3, 2, 1)
  )

  pots <- sess %>%
    split_pots() %>%
    map_pots()

  testthat::expect_equal(pots$value, 130)
  testthat::expect_equal(pots$name[1], "c")
})

test_that("allin winner split pot", {

  sess <- tibble(
    name = c("a", "b", "c"),
    t_stake = c(10, 40, 60),
    folded = c(1, 0, 0),
    allin = c(0, 1, 0),
    rank = c(3, 1, 2)
  )

  pots <- sess %>%
    split_pots() %>%
    map_pots()

  testthat::expect_equal(max(pots$value), 90)
  testthat::expect_equal(pots$name[1], "b")
})

test_that("allin looser split pot", {

  sess <- tibble(
    name = c("a", "b", "c"),
    t_stake = c(10, 60, 40),
    folded = c(1, 0, 0),
    allin = c(0, 0, 1),
    rank = c(3, 1, 2)
  )

  pots <- sess %>%
    split_pots() %>%
    map_pots()

  testthat::expect_equal(max(pots$value), 90)
  testthat::expect_equal(pots$name[2], "b")
})

test_that("allin real split pot", {

  sess <- tibble(
    name = c("a", "b", "c"),
    t_stake = c(10, 60, 60),
    folded = c(1, 0, 0),
    allin = c(0, 0, 0),
    rank = c(2, 1, 1)
  )

  pots <- sess %>%
    split_pots() %>%
    map_pots()

  testthat::expect_equal(pots$value[1], 65)
  testthat::expect_equal(sum(pots$value), 130)
})
