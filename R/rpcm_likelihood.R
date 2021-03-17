rpcm_log_likelihood <- function(difficulty,
                                col_sums,
                                row_sums,
                                item_time_limits,
                                engine = c("R", "C"),
                                factorial_like_component) {
    if (is_R6Class(row_sums, "RawScoreCollection")) {
        if (is_R6Class(item_time_limits, "TimeLimitCollection")) {
            cll <- sum(
                item_time_limits$summed_item_parameters(difficulty) * col_sums
            ) -
                factorial_like_component -
                row_sums$compute_likelihood_component(
                    difficulty,
                    item_time_limits
                )
        } else {
            stop(paste(
                toString(item_time_limits),
                "is invalid as time limits",
                "when row sums are of class RawScoreCollection."
            ))
        }
    } else if (engine == "C") {
        cll <- rpcm_log_likelihood_c(
            difficulty,
            col_sums,
            row_sums,
            item_time_limits,
            factorial_like_component
        )
    } else {
        ## log(exp(difficulty + item_time_limits)) = difficulty + item_time_limits
        cll <- sum((difficulty + item_time_limits) * col_sums) -
            factorial_like_component -
            sum(
                sapply(row_sums, function(row_sum) {
                    log(
                        rpcm_esf(
                            row_sum,
                            difficulty,
                            item_time_limits,
                            0,
                            engine
                        )[[1]]
                    )
                })
            )
    }
    if (is.na(cll) | !is.finite(cll)) {
        cll <- -.Machine$double.xmax
    }
    return(-cll)
}