test_that("plot_performance basic functionality works", {
  # Setup test data
  set.seed(123)
  test_data <- data.frame(
    method = rep(c("Method1", "Method2", "Method3"), each = 5),
    sensitivity = runif(15, 0.7, 0.9),
    specificity = runif(15, 0.6, 0.8),
    precision = runif(15, 0.7, 0.9),
    sensitivity_ci_lower = runif(15, 0.6, 0.7),
    sensitivity_ci_upper = runif(15, 0.8, 0.9),
    specificity_ci_lower = runif(15, 0.5, 0.6),
    specificity_ci_upper = runif(15, 0.7, 0.8),
    precision_ci_lower = runif(15, 0.6, 0.7),
    precision_ci_upper = runif(15, 0.8, 0.9)
  )
  class(test_data) <- c("daa_performance", "data.frame")
  
  # Test heatmap
  p_heatmap <- plot_performance(test_data, plot_type = "heatmap")
  expect_s3_class(p_heatmap, "ggplot")
  expect_true("GeomTile" %in% class(p_heatmap$layers[[1]]$geom))
  
  # Test boxplot
  p_boxplot <- plot_performance(test_data, plot_type = "boxplot")
  expect_s3_class(p_boxplot, "ggplot")
  expect_true("GeomBoxplot" %in% class(p_boxplot$layers[[1]]$geom))
  
  # Test violin plot
  p_violin <- plot_performance(test_data, plot_type = "violin")
  expect_s3_class(p_violin, "ggplot")
  expect_true("GeomViolin" %in% class(p_violin$layers[[1]]$geom))
})

test_that("plot_performance themes work correctly", {
  set.seed(123)
  test_data <- data.frame(
    method = rep(c("Method1", "Method2"), each = 5),
    sensitivity = runif(10),
    specificity = runif(10)
  )
  class(test_data) <- c("daa_performance", "data.frame")
  
  # Test different themes
  themes <- c("default", "dark", "classic")
  for (theme in themes) {
    p <- plot_performance(test_data, theme = theme)
    expect_s3_class(p, "ggplot")
  }
  
  # Test invalid theme fallback
  p_invalid <- plot_performance(test_data, theme = "invalid")
  expect_s3_class(p_invalid, "ggplot")
})

test_that("plot_performance handles confidence intervals correctly", {
  # Data with CIs
  test_data_with_ci <- data.frame(
    method = rep("Method1", 5),
    sensitivity = runif(5),
    sensitivity_ci_lower = runif(5),
    sensitivity_ci_upper = runif(5)
  )
  class(test_data_with_ci) <- c("daa_performance", "data.frame")
  
  # Data without CIs
  test_data_without_ci <- data.frame(
    method = rep("Method1", 5),
    sensitivity = runif(5)
  )
  class(test_data_without_ci) <- c("daa_performance", "data.frame")
  
  # Test with CIs
  p_with_ci <- plot_performance(test_data_with_ci, plot_type = "boxplot")
  expect_true(any(sapply(p_with_ci$layers, function(l) "GeomErrorbar" %in% class(l$geom))))
  
  # Test without CIs
  p_without_ci <- plot_performance(test_data_without_ci, plot_type = "boxplot")
  expect_false(any(sapply(p_without_ci$layers, function(l) "GeomErrorbar" %in% class(l$geom))))
})

test_that("plot_performance handles edge cases", {
  # Test invalid input class
  test_data <- data.frame(method = "Method1", value = 1)
  expect_error(
    plot_performance(test_data),
    "results must be a daa_performance object"
  )
  
  # Test single method
  single_method_data <- data.frame(
    method = rep("Method1", 3),
    sensitivity = runif(3)
  )
  class(single_method_data) <- c("daa_performance", "data.frame")
  p_single <- plot_performance(single_method_data)
  expect_s3_class(p_single, "ggplot")
  
  # Test single observation
  single_obs_data <- data.frame(
    method = "Method1",
    sensitivity = 0.8
  )
  class(single_obs_data) <- c("daa_performance", "data.frame")
  p_single_obs <- plot_performance(single_obs_data)
  expect_s3_class(p_single_obs, "ggplot")
})

test_that("plot_performance S3 methods work", {
  set.seed(123)
  test_data <- data.frame(
    method = rep(c("Method1", "Method2"), each = 5),
    sensitivity = runif(10)
  )
  class(test_data) <- c("daa_performance", "data.frame")
  
  # Test autoplot method
  p_auto <- autoplot.daa_performance(test_data)
  expect_s3_class(p_auto, "ggplot")
  
  # Test print method
  p <- plot_performance(test_data)
  expect_output(print(p), NA)  # Should not error
  
  # Test plotly conversion if available
  if (requireNamespace("plotly", quietly = TRUE)) {
    p_ly <- plot_performance(test_data)
    expect_true(is.plotly(p_ly))
  }
}) 