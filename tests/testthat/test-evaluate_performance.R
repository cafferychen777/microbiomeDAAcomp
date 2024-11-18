test_that("evaluate_performance basic functionality works", {
  # Setup test data
  set.seed(123)
  n_samples <- 100
  
  # Create test results
  true_status <- sample(c(TRUE, FALSE), n_samples, replace = TRUE)
  test_results <- list(
    method1 = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.7, 0.3)),
    method2 = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.6, 0.4))
  )
  
  # Test basic functionality
  result <- evaluate_performance(test_results, true_status)
  
  # Check structure
  expect_s3_class(result, "daa_performance")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)  # Two methods
  expect_true(all(c("method", "sensitivity", "specificity", "precision") %in% names(result)))
  
  # Check metric values are between 0 and 1
  numeric_cols <- sapply(result, is.numeric)
  expect_true(all(result[, numeric_cols] >= 0 & result[, numeric_cols] <= 1))
})

test_that("evaluate_performance handles different metrics correctly", {
  set.seed(123)
  n_samples <- 100
  true_status <- sample(c(TRUE, FALSE), n_samples, replace = TRUE)
  test_results <- list(
    method1 = sample(c(TRUE, FALSE), n_samples, replace = TRUE)
  )
  
  # Test different metric combinations
  metrics1 <- c("sensitivity", "specificity")
  result1 <- evaluate_performance(test_results, true_status, metrics = metrics1)
  expect_equal(sum(names(result1) %in% metrics1), length(metrics1))
  
  metrics2 <- c("accuracy", "f1_score", "mcc")
  result2 <- evaluate_performance(test_results, true_status, metrics = metrics2)
  expect_equal(sum(names(result2) %in% metrics2), length(metrics2))
  
  # Test invalid metrics
  expect_error(
    evaluate_performance(test_results, true_status, metrics = "invalid_metric"),
    "should be one of"
  )
})

test_that("evaluate_performance confidence intervals work correctly", {
  set.seed(123)
  n_samples <- 100
  true_status <- sample(c(TRUE, FALSE), n_samples, replace = TRUE)
  test_results <- list(
    method1 = sample(c(TRUE, FALSE), n_samples, replace = TRUE)
  )
  
  result <- evaluate_performance(test_results, true_status)
  
  # Check CI columns exist
  ci_cols <- c("sensitivity_ci_lower", "sensitivity_ci_upper",
               "specificity_ci_lower", "specificity_ci_upper",
               "precision_ci_lower", "precision_ci_upper")
  expect_true(all(ci_cols %in% names(result)))
  
  # Check CI values are between 0 and 1
  for (col in ci_cols) {
    expect_true(all(result[[col]] >= 0 & result[[col]] <= 1))
  }
  
  # Check lower bound is less than upper bound
  expect_true(all(result$sensitivity_ci_lower <= result$sensitivity_ci_upper))
  expect_true(all(result$specificity_ci_lower <= result$specificity_ci_upper))
  expect_true(all(result$precision_ci_lower <= result$precision_ci_upper))
})

test_that("evaluate_performance handles edge cases", {
  # Test empty results
  expect_error(
    evaluate_performance(list(), logical(0)),
    "results must be a list of method results"
  )
  
  # Test mismatched lengths
  test_results <- list(method1 = c(TRUE, FALSE))
  true_status <- c(TRUE)
  expect_error(
    evaluate_performance(test_results, true_status),
    "length of predicted and true status must match"
  )
  
  # Test non-logical values
  test_results <- list(method1 = c(1, 0))
  true_status <- c(TRUE, FALSE)
  expect_error(
    evaluate_performance(test_results, true_status),
    "predicted status must be logical"
  )
})

test_that("evaluate_performance print and plot methods work", {
  set.seed(123)
  n_samples <- 100
  true_status <- sample(c(TRUE, FALSE), n_samples, replace = TRUE)
  test_results <- list(
    method1 = sample(c(TRUE, FALSE), n_samples, replace = TRUE),
    method2 = sample(c(TRUE, FALSE), n_samples, replace = TRUE)
  )
  
  result <- evaluate_performance(test_results, true_status)
  
  # Test print method
  expect_output(print(result), "DAA Method Performance Evaluation")
  expect_output(print(result), "===============================")
  
  # Test plot method
  p <- plot(result, metric = "sensitivity")
  expect_s3_class(p, "ggplot")
  
  # Test plot with invalid metric
  expect_error(
    plot(result, metric = "invalid_metric"),
    "Specified metric not found in performance results"
  )
})

test_that("evaluate_performance ranking works correctly", {
  set.seed(123)
  n_samples <- 100
  true_status <- sample(c(TRUE, FALSE), n_samples, replace = TRUE)
  test_results <- list(
    method1 = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.8, 0.2)),
    method2 = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.6, 0.4)),
    method3 = sample(c(TRUE, FALSE), n_samples, replace = TRUE, prob = c(0.4, 0.6))
  )
  
  result <- evaluate_performance(test_results, true_status)
  
  # Check rank columns exist
  rank_cols <- c("sensitivity_rank", "specificity_rank", "precision_rank")
  expect_true(all(rank_cols %in% names(result)))
  
  # Check rank values are correct
  for (col in rank_cols) {
    expect_equal(sort(result[[col]]), 1:3)
  }
}) 