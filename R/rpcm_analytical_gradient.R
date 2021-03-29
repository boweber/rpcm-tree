
## Note: The parameter 'factorial_like_component' is required to
## pass the same argument to the rpcm_log_likelihood func by optim.
rpcm_analytical_gradient <- function(difficulty,
                                     col_sums,
                                     row_sums,
                                     item_time_limits,
                                     factorial_like_component) {
    esf_result <- row_sums$compute_gradient_component(
        difficulty,
        item_time_limits
    )
    if (any(is.nan(esf_result))) {
        stop(paste("Invalid difficulty value in gradient", toString(difficulty), "esf:", toString(esf_result)))
    }
    return(-(col_sums - esf_result))
}