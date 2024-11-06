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
    # Create invalid data to trigger error
    bad_counts <- matrix(0, nrow = 5, ncol = 10)
    groups <- factor(rep(1:2, each = 5))
    
    expect_warning(
        run_daa_methods(
            count_matrix = bad_counts,
            group_info = groups,
            methods = "DESeq2"
        )
    )
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