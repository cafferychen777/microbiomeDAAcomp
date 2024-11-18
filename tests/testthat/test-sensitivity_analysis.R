# Create mock data and helper functions for testing
setup_test_env <- function() {
    # Create mock microbiome data
    mock_data <- list(
        counts = matrix(rpois(100, lambda = 10), nrow = 10),
        metadata = data.frame(
            group = rep(c("A", "B"), each = 5)
        )
    )

    # Mock DAA method that returns consistent results
    mock_daa_results <- list(
        significant_features = c("Feature1", "Feature2"),
        test_statistics = c(2.5, 3.1, 1.8, 0.5, 1.2),
        p_values = c(0.01, 0.02, 0.06, 0.8, 0.3)
    )

    # Assign mock functions to global environment
    assign("run_daa_method",
           function(data, method, params) mock_daa_results,
           envir = .GlobalEnv)

    assign("get_significant_features",
           function(results) results$significant_features,
           envir = .GlobalEnv)

    assign("get_test_statistics",
           function(results) results$test_statistics,
           envir = .GlobalEnv)

    return(mock_data)
}

test_that("sensitivity_analysis handles basic input correctly", {
    mock_data <- setup_test_env()

    results <- sensitivity_analysis(
        data = mock_data,
        parameters = list(alpha = c(0.01, 0.05)),
        method_names = "method1"
    )

    # Test structure
    expect_type(results, "list")
    expect_s3_class(results, "daa_sensitivity")
    expect_named(results, "method1")

    # Test content
    method_results <- results[["method1"]]
    expect_type(method_results, "list")
    expect_named(method_results, "alpha")

    # Test metrics
    alpha_results <- method_results[["alpha"]]
    expect_s3_class(alpha_results, "data.frame")
    expect_named(alpha_results,
                c("parameter_value", "n_significant",
                  "overlap_ratio", "effect_size"))
})

test_that("sensitivity_analysis handles multiple methods", {
    mock_data <- setup_test_env()

    results <- sensitivity_analysis(
        data = mock_data,
        parameters = list(alpha = c(0.01, 0.05)),
        method_names = c("method1", "method2")
    )

    expect_named(results, c("method1", "method2"))
})

test_that("sensitivity_analysis handles multiple parameters", {
    mock_data <- setup_test_env()

    results <- sensitivity_analysis(
        data = mock_data,
        parameters = list(
            alpha = c(0.01, 0.05),
            min_abundance = c(0.001, 0.01)
        ),
        method_names = "method1"
    )

    method_results <- results[["method1"]]
    expect_named(method_results, c("alpha", "min_abundance"))
})

test_that("calculate_overlap_ratio works correctly", {
    set1 <- c("A", "B", "C")
    set2 <- c("B", "C", "D")

    overlap <- calculate_overlap_ratio(set1, set2)
    expect_equal(overlap, 0.5)  # 2 common / 4 total

    # Edge cases
    expect_equal(calculate_overlap_ratio(character(0), set2), 0)
    expect_equal(calculate_overlap_ratio(set1, set1), 1)
})

test_that("calculate_effect_size works correctly", {
    result1 <- list(test_statistics = c(1, 2, 3))
    result2 <- list(test_statistics = c(1.1, 2.1, 3.1))

    effect <- calculate_effect_size(result1, result2)
    expect_true(effect > 0.9)  # Should be highly correlated

    # Different lengths should return NA
    result3 <- list(test_statistics = c(1, 2))
    expect_true(is.na(calculate_effect_size(result1, result3)))
})

test_that("sensitivity_analysis handles errors gracefully", {
    mock_data <- setup_test_env()

    # Invalid parameters
    expect_error(
        sensitivity_analysis(mock_data, "not_a_list", "method1"),
        "parameters must be a list"
    )

    expect_error(
        sensitivity_analysis(mock_data, list(), 123),
        "method_names must be a character vector"
    )

    # Empty parameters
    results <- sensitivity_analysis(
        mock_data,
        parameters = list(alpha = numeric(0)),
        method_names = "method1"
    )
    expect_equal(nrow(results[["method1"]][["alpha"]]), 0)
})

test_that("sensitivity_analysis results are reproducible", {
    mock_data <- setup_test_env()

    set.seed(123)
    result1 <- sensitivity_analysis(
        mock_data,
        parameters = list(alpha = c(0.01, 0.05)),
        method_names = "method1"
    )

    set.seed(123)
    result2 <- sensitivity_analysis(
        mock_data,
        parameters = list(alpha = c(0.01, 0.05)),
        method_names = "method1"
    )

    expect_equal(result1, result2)
})
