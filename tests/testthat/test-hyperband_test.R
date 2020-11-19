test_that("Parameter sampling works", {
  expect_length(param_sample("ranger", "mtry", columns = 11), 1)
})
