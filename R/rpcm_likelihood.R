rpcm_log_likelihood <- function(difficulty,
                                col_sums,
                                row_sums,
                                item_time_limits,
                                engine = c("R", "C"),
                                factorial_like_component) {

    ## Compute the conditional log likelihood

    cll <- sum(log(exp(difficulty + item_time_limits)) * col_sums) -
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

    if (is.na(cll) | !is.finite(cll)) {
        cll <- -.Machine$double.xmax
    }
    return(-cll)
}