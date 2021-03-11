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

        if (any(esf <= 0) || !is.numeric(esf)) {
            stop(paste(
                "The esf result",
                toString(esf),
                "is invalid (func: rpcm_log_likelihood).",
            ))
        }

        esf_component_result <- esf_component_result + log(esf)
    }

    ## Validate parameters

    if (length(difficulty) != length(item_time_limits)) {
        stop(paste(
            toString(difficulty),
            "is invalid as difficulty with item_time_limits",
            toString(item_time_limits),
            "(func: rpcm_log_likelihood)."
        ))
    }

    item_parameter <- exp(difficulty + item_time_limits)

    if (length(item_parameter) != length(col_sums)) {
        stop(paste(
            toString(item_parameter),
            "is invalid as item_parameter with col_sums",
            toString(col_sums),
            "(func: rpcm_log_likelihood)."
        ))
    }

    if (any(item_parameter <= 0) || !is.numeric(item_parameter)) {
        stop(paste(
            toString(item_parameter),
            "is invalid as item parameters (func: rpcm_log_likelihood)."
        ))
    }

    ## Compute the conditional log likelihood

    cll <- sum(log(item_parameter) * col_sums) -
        factorial_like_component -
        esf_component_result

    if (is.na(cll) | !is.finite(cll)) {
        cll <- -.Machine$double.xmax
    }
    return(-cll)
}