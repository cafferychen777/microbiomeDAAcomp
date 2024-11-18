#' Conduct Statistical Power Analysis
#' 
#' @param effect_sizes Numeric vector of effect sizes to evaluate
#' @param sample_sizes Numeric vector of sample sizes to evaluate
#' @param alpha Significance level
#' @param n_simulations Number of simulations for each combination
#'
#' @return Data frame containing power analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' power_results <- power_analysis(effect_sizes = seq(0.5, 2, 0.5),
#'                                sample_sizes = c(50, 100, 200))
#' }
power_analysis <- function(effect_sizes, 
                         sample_sizes, 
                         alpha = 0.05,
                         n_simulations = 1000) {
    # Input validation
    if (any(effect_sizes <= 0)) {
        stop("effect_sizes must be positive")
    }
    if (any(sample_sizes <= 0) || any(sample_sizes %% 1 != 0)) {
        stop("sample_sizes must be positive integers")
    }
    if (alpha <= 0 || alpha >= 1) {
        stop("alpha must be between 0 and 1")
    }
    
    # Initialize results data frame
    results <- expand.grid(
        effect_size = effect_sizes,
        sample_size = sample_sizes,
        power = NA
    )
    
    # Run simulations for each combination
    for (i in 1:nrow(results)) {
        effect_size <- results$effect_size[i]
        n <- results$sample_size[i]
        
        # Store p-values from simulations
        p_values <- numeric(n_simulations)
        
        # Run simulations
        for (j in 1:n_simulations) {
            # Generate data under alternative hypothesis
            control_group <- rnorm(n/2, mean = 0, sd = 1)
            treatment_group <- rnorm(n/2, mean = effect_size, sd = 1)
            
            # Perform t-test
            test_result <- t.test(treatment_group, control_group)
            p_values[j] <- test_result$p.value
        }
        
        # Calculate power (proportion of significant results)
        results$power[i] <- mean(p_values < alpha)
    }
    
    return(results)
} 