rpcm_log_likelihood <- function(difficulty,
                                col_sums,
                                row_sums,
                                item_time_limits,
                                factorial_like_component) {
    cll <- sum(
        (item_time_limits + difficulty) * col_sums
    ) -
        factorial_like_component -
        row_sums$compute_likelihood_component(
            difficulty,
            item_time_limits
        )
    if (is.na(cll) | !is.finite(cll)) {
        cll <- -.Machine$double.xmax
    }
    return(-cll)
}