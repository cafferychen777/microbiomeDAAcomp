test_that("power_analysis produces expected output format", {
  # Setup test parameters
  effect_sizes <- c(0.5, 1.0)
  sample_sizes <- c(30, 50)
  
  # Run analysis
  results <- power_analysis(
    effect_sizes = effect_sizes,
    sample_sizes = sample_sizes
  )
  
  # Test output structure
  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), length(effect_sizes) * length(sample_sizes))
  expect_equal(ncol(results), 3)
  expect_named(results, c("effect_size", "sample_size", "power"))
})

test_that("power values are within valid range", {
  results <- power_analysis(
    effect_sizes = c(0.5),
    sample_sizes = c(100),
    n_simulations = 500
  )
  
  expect_true(all(results$power >= 0 & results$power <= 1))
})

test_that("larger effect sizes lead to higher power", {
  results <- power_analysis(
    effect_sizes = c(0.2, 1.0),
    sample_sizes = c(100),
    n_simulations = 500
  )
  
  expect_true(results$power[2] > results$power[1])
})

test_that("larger sample sizes lead to higher power", {
  results <- power_analysis(
    effect_sizes = c(0.5),
    sample_sizes = c(30, 200),
    n_simulations = 500
  )
  
  expect_true(results$power[2] > results$power[1])
})

test_that("function handles invalid inputs appropriately", {
  # Test negative effect size
  expect_error(
    power_analysis(effect_sizes = -1, sample_sizes = 30),
    "effect_sizes must be positive"
  )
  
  # Test invalid sample size
  expect_error(
    power_analysis(effect_sizes = 0.5, sample_sizes = 0),
    "sample_sizes must be positive integers"
  )
  
  # Test invalid alpha
  expect_error(
    power_analysis(effect_sizes = 0.5, sample_sizes = 30, alpha = 1.5),
    "alpha must be between 0 and 1"
  )
})

test_that("results are reproducible with set.seed", {
  set.seed(123)
  result1 <- power_analysis(
    effect_sizes = 0.5,
    sample_sizes = 50,
    n_simulations = 100
  )
  
  set.seed(123)
  result2 <- power_analysis(
    effect_sizes = 0.5,
    sample_sizes = 50,
    n_simulations = 100
  )
  
  expect_equal(result1$power, result2$power)
}) 