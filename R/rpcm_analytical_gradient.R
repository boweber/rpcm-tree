
## Note: The parameter 'factorial_like_component' is required to
## pass the same argument to the rpcm_log_likelihood func by optim.
rpcm_analytical_gradient <- function(difficulty,
                                     col_sums,
                                     row_sums,
                                     item_time_limits,
                                     engine = c("R", "C"),
                                     factorial_like_component) {
    if (is_R6Class(row_sums, "RawScoreCollection")) {
        if (is_R6Class(item_time_limits, "TimeLimitCollection")) {
            esf_result <- row_sums$compute_gradient_component(
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
    } else {
        esf_result <- rep(0, length(col_sums))
        for (raw_score_index in seq_len(length(row_sums))) {
            esf <- rpcm_esf(
                row_sums[raw_score_index],
                difficulty,
                item_time_limits,
                1,
                engine
            )

            esf_result <- esf_result + ((1 / esf[[1]]) * esf[[2]])
        }
    }

    # gradient <- c()
    # for (item_index in seq_len(length(col_sums))) {
    #     item_gradient <- (col_sums[item_index] / difficulty[item_index]) -
    #         esf_result[item_index]
    #
    #     gradient <- c(gradient, item_gradient)
    # }

    return(-(col_sums - esf_result))
}