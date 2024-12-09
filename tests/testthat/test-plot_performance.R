test_that("plot_performance basic functionality works", {
  # Setup test data
  set.seed(123)
  test_data <- data.frame(
    method = rep(c("Method1", "Method2", "Method3"), each = 5),
    sensitivity = runif(15, 0.7, 0.9),
    specificity = runif(15, 0.6, 0.8),
    precision = runif(15, 0.7, 0.9)
  )
  class(test_data) <- c("daa_performance", "data.frame")

  # Test different plot types without plotly conversion
  p_heatmap <- plot_performance(test_data, plot_type = "heatmap", use_plotly = FALSE)
  expect_s3_class(p_heatmap, "ggplot")
  expect_true(any(sapply(p_heatmap$layers, function(l) "GeomTile" %in% class(l$geom))))

  p_boxplot <- plot_performance(test_data, plot_type = "boxplot", use_plotly = FALSE)
  expect_s3_class(p_boxplot, "ggplot")
  expect_true(any(sapply(p_boxplot$layers, function(l) "GeomBoxplot" %in% class(l$geom))))

  p_violin <- plot_performance(test_data, plot_type = "violin", use_plotly = FALSE)
  expect_s3_class(p_violin, "ggplot")
  expect_true(any(sapply(p_violin$layers, function(l) "GeomViolin" %in% class(l$geom))))
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
})

test_that("plot_performance handles edge cases", {
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
  expect_output(print(p), NA)
})
