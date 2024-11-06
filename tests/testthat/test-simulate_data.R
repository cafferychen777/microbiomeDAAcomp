test_that("simulate_data generates correct dimensions", {
    n_samples <- 60
    n_taxa <- 100
    result <- simulate_data(n_samples = n_samples, n_taxa = n_taxa)
    
    expect_equal(dim(result$counts), c(n_taxa, n_samples))
    expect_equal(length(result$truth), n_taxa)
    expect_equal(length(result$group_info), n_samples)
})

test_that("simulate_data respects group sizes", {
    group_sizes <- c(30, 40)
    result <- simulate_data(n_samples = 70, n_taxa = 100, group_sizes = group_sizes)
    
    expect_equal(table(result$group_info), group_sizes)
})

test_that("simulate_data handles differential abundance correctly", {
    n_diff <- 10
    result <- simulate_data(n_samples = 60, n_taxa = 100, n_diff = n_diff)
    
    expect_equal(sum(result$truth), n_diff)
})

test_that("simulate_data generates phylogenetic tree when requested", {
    result_with_tree <- simulate_data(n_samples = 60, n_taxa = 100, phylo_tree = TRUE)
    result_without_tree <- simulate_data(n_samples = 60, n_taxa = 100, phylo_tree = FALSE)
    
    expect_true(!is.null(result_with_tree$phylo_tree))
    expect_true(is.null(result_without_tree$phylo_tree))
})

test_that("simulate_data validates input correctly", {
    expect_error(
        simulate_data(n_samples = 60, n_taxa = 100, group_sizes = c(30, 40)),
        "Group sizes must sum to n_samples"
    )
    expect_error(
        simulate_data(n_samples = 60, n_taxa = 100, n_diff = 101),
        "Number of differential taxa cannot exceed total number of taxa"
    )
}) 