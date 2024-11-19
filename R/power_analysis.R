#' Conduct Statistical Power Analysis for Two-Sample T-Tests
#' 
#' @description
#' Performs Monte Carlo simulation-based power analysis for two-sample t-tests
#' by evaluating different combinations of effect sizes and sample sizes.
#' 
#' @param effect_sizes Numeric vector of effect sizes to evaluate. These represent
#'        the mean differences between treatment and control groups in standard
#'        deviation units (Cohen's d).
#' @param sample_sizes Numeric vector of sample sizes to evaluate. These represent
#'        the total sample size (will be split equally between groups).
#' @param alpha Significance level for hypothesis testing (Type I error rate).
#'        Default is 0.05.
#' @param n_simulations Number of Monte Carlo simulations to run for each
#'        combination of effect size and sample size. Default is 1000.
#'
#' @return A data frame containing:
#'         \itemize{
#'           \item effect_size: Evaluated effect size
#'           \item sample_size: Evaluated sample size
#'           \item power: Estimated statistical power (proportion of significant results)
#'         }
#'
#' @details
#' The function assumes:
#' - Equal sample sizes between groups
#' - Normal distribution of data
#' - Equal variances between groups
#' - Two-sided alternative hypothesis
#' Power is estimated using Monte Carlo simulation by generating data under
#' the alternative hypothesis and calculating the proportion of significant results.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' power_results <- power_analysis(
#'   effect_sizes = seq(0.5, 2, 0.5),
#'   sample_sizes = c(50, 100, 200)
#' )
#' 
#' # More precise estimation with more simulations
#' precise_results <- power_analysis(
#'   effect_sizes = c(0.3, 0.5, 0.8),
#'   sample_sizes = seq(20, 100, 20),
#'   n_simulations = 5000
#' )
#' 
#' # Plot results
#' library(ggplot2)
#' ggplot(power_results, aes(x = sample_size, y = power, color = factor(effect_size))) +
#'   geom_line() +
#'   geom_point()
#' }
#' 
#' @importFrom stats rnorm t.test
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