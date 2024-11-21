test_that("run_daa_methods handles input validation correctly", {
    expect_error(
        run_daa_methods(),
        "Must provide either 'data' object or both 'count_matrix' and 'group_info'"
    )
})

test_that("run_daa_methods works with simulated data", {
    # Generate simple test data
    sim_data <- simulate_data(n_samples = 20, n_taxa = 30, n_diff = 5)

    # Test with single method
    result_single <- run_daa_methods(
        data = sim_data,
        methods = "DESeq2"
    )

    expect_type(result_single, "list")
    expect_named(result_single, c("results", "summary", "runtime", "parameters"))
    expect_true("DESeq2" %in% names(result_single$results))
})

test_that("run_daa_methods handles method errors gracefully", {
    # Create a more reasonable invalid dataset
    bad_counts <- matrix(
        rpois(50, lambda = 0.1),  # Generate sparse counts using Poisson distribution
        nrow = 5,
        ncol = 10
    )
    groups <- factor(rep(1:2, each = 5))

    # Test if warnings are handled correctly
    expect_warning(
        {
            result <- run_daa_methods(
                count_matrix = bad_counts,
                group_info = groups,
                methods = "DESeq2"
            )
        },
        "DESeq2"  # Only match part of the warning message
    )

    # Verify that result structure is correct even with warnings
    expect_type(result, "list")
    expect_named(result, c("results", "summary", "runtime", "parameters"))
    expect_true(!is.null(result$results))  # Ensure results are not NULL
    expect_true(length(names(result$results)) > 0)  # Ensure results have names
    expect_true("DESeq2" %in% names(result$results))  # Check method name
})

test_that("run_daa_methods returns correct output structure", {
    sim_data <- simulate_data(n_samples = 20, n_taxa = 30, n_diff = 5)
    result <- run_daa_methods(
        data = sim_data,
        methods = c("DESeq2", "ALDEx2")
    )

    expect_named(result$summary, c("Method", "N_Significant", "Mean_Effect"))
    expect_equal(nrow(result$summary), 2)
    expect_type(result$runtime, "double")
    expect_length(result$runtime, 2)
})
