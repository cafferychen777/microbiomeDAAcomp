# Import required functions from base packages
#' @importFrom stats binom.test cor rbinom rnbinom rnorm sd setNames t.test
#' @importFrom utils read.csv packageVersion
#' @importFrom tidyselect all_of
#' @importFrom plotly ggplotly layout
#' @importFrom scales percent_format
NULL
# Define global variables used in package
utils::globalVariables(c(
    "method",
    "accuracy",
    "precision",
    "recall",
    "f1_score",
    "metric",
    "value",
    "physeq",
    "p_adjust_method",
    "run_daa_method",
    "cor",
    "sd",
    "binom.test",
    "read.csv",
    "all_of",
    "rnorm",
    "t.test",
    "setNames",
    "rnbinom",
    "rbinom",
    "ci_lower",
    "ci_upper"
))


