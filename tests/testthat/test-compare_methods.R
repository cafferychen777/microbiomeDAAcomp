test_that("compare_methods works correctly", {
  # Setup test data
  set.seed(123)
  methods <- rep(c("DESeq2", "ALDEx2", "edgeR"), each = 10)
  test_data <- data.frame(
    method = methods,
    accuracy = runif(30, 0.7, 0.9),
    precision = runif(30, 0.6, 0.9),
    recall = runif(30, 0.6, 0.9),
    f1_score = runif(30, 0.6, 0.9)
  )
  
  # Test statistical comparison
  stat_result <- compare_methods(test_data, comparison_type = "statistical")
  expect_s3_class(stat_result, "daa_comparison")
  expect_type(stat_result, "list")
  expect_named(stat_result, c("statistical", "summary"))
  expect_s3_class(stat_result$statistical$friedman, "htest")
  
  # Test visual comparison
  vis_result <- compare_methods(test_data, comparison_type = "visual")
  expect_s3_class(vis_result, "daa_comparison")
  expect_s3_class(vis_result$visual$performance_plot, "ggplot")
  
  # Test comprehensive comparison
  comp_result <- compare_methods(test_data, comparison_type = "comprehensive")
  expect_s3_class(comp_result, "daa_comparison")
  expect_named(comp_result, c("statistical", "visual", "summary"))
  
  # Test input validation
  expect_error(
    compare_methods(as.matrix(test_data)),
    "performance_results must be a data frame"
  )
  
  # Test missing columns
  bad_data <- test_data[, -1]
  expect_error(
    compare_methods(bad_data),
    "performance_results must contain columns: method, accuracy, precision, recall, f1_score"
  )
  
  # Test summary statistics
  expect_equal(nrow(stat_result$summary), 3)  # Three methods
  expect_true(all(
    c("mean_accuracy", "sd_accuracy", "mean_precision", 
      "mean_recall", "mean_f1") %in% 
      colnames(stat_result$summary)
  ))
  
  # Test print method
  expect_output(print(stat_result), "DAA Method Comparison Results")
  expect_output(print(stat_result), "Performance Summary:")
  expect_output(print(stat_result), "Statistical Test Results:")
})

test_that("compare_methods handles edge cases", {
  # Single method case
  single_method_data <- data.frame(
    method = rep("DESeq2", 5),
    accuracy = runif(5, 0.7, 0.9),
    precision = runif(5, 0.6, 0.9),
    recall = runif(5, 0.6, 0.9),
    f1_score = runif(5, 0.6, 0.9)
  )
  
  expect_error(
    compare_methods(single_method_data, "statistical"),
    "groups must have more than one level"
  )
  
  # Empty data frame
  empty_data <- data.frame(
    method = character(),
    accuracy = numeric(),
    precision = numeric(),
    recall = numeric(),
    f1_score = numeric()
  )
  
  expect_error(
    compare_methods(empty_data),
    "performance_results must contain data"
  )
}) 