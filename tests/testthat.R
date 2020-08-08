library(testthat)
pacman::p_load(tidyverse, purrr, tidyr)

library(pokerena)
devtools::load_all()

test_check("pokerena")
