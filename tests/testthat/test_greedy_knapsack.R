context("knapsack_greedy")

suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(gk <- knapsack_greedy(cx = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements", "time_elapsed"))
})

test_that("functions rejects errounous input.", {
  expect_error(knapsack_greedy("hej", 3500))
  expect_error(knapsack_greedy(cx = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- knapsack_greedy(cx = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 15428)
  #expect_equal(round(gk$elements[1]), 7)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- knapsack_greedy(cx = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- knapsack_greedy(cx = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- knapsack_greedy(cx = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  st <- system.time(gk <- knapsack_greedy(cx = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)
  
  gk <- knapsack_greedy(cx = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 201114)
  
  gk <- knapsack_greedy(cx = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 277582)
})