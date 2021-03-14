rpcm_log_likelihood <- function(difficulty,
                                col_sums,
                                row_sums,
                                item_time_limits,
                                engine = c("R", "C"),
                                factorial_like_component) {
    ## Compute the esf part of the likelihood
    esf_component_result <- 0
    for (raw_score_index in seq_len(length(row_sums))) {
        esf <- rpcm_esf(
            row_sums[raw_score_index],
            difficulty,
            item_time_limits,
            0,
            raw_score_index,
            engine
        )[[1]]

        esf_component_result <- esf_component_result + log(esf)
    }

    ## Compute the conditional log likelihood

    cll <- sum(log(exp(difficulty + item_time_limits)) * col_sums) -
        factorial_like_component -
        esf_component_result

    if (is.na(cll) | !is.finite(cll)) {
        cll <- -.Machine$double.xmax
    }
    return(-cll)
}