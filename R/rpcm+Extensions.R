make_cloglike <- function(col_sums,
                          row_sums,
                          item_time_limits,
                          y,
                          ...) {
    iterim_result <- sum(lfactorial(y))

    if (any(iterim_result <= 0) || !is.numeric(iterim_result)) {
        stop(paste(
            toString(iterim_result),
            "is invalid as iterim_result (func: make_cloglike)."
        ))
    }

    cloglike <- function(difficulty_parameters) {
        esf_result <- sapply(
            row_sums,
            function(raw_score) {
                esf <- rpcm_esf(
                    raw_score,
                    difficulty_parameters,
                    item_time_limits,
                    0,
                    ...
                )[[1]]
                if (any(esf <= 0) || !is.numeric(esf)) {
                    stop(paste(
                        "The esf result",
                        toString(esf),
                        "is invalid (func: make_cloglike).",
                    ))
                }
                return(log(esf))
            }
        )

        if (length(esf_result) != length(row_sums)) {
            stop(paste(
                "The esf result",
                toString(esf_result),
                "and the row sums",
                toString(row_sums),
                "have different length (func: make_cloglike)."
            ))
        }
        if (length(difficulty_parameters) != length(item_time_limits)) {
            stop(paste(
                toString(difficulty_parameters),
                "is invalid as difficulty_parameters with item_time_limits: ",
                toString(item_time_limits),
                " (func: make_cloglike)."
            ))
        }

        item_parameter <- difficulty_parameters * item_time_limits

        if (length(item_parameter) != length(col_sums)) {
            stop(paste(
                toString(item_parameter),
                "is invalid as difficulty_parameters with col_sums: ",
                toString(col_sums),
                "(func: make_cloglike)."
            ))
        }

        if (any(item_parameter <= 0) || !is.numeric(item_parameter)) {
            stop(paste(
                toString(item_parameter),
                "is invalid as item parameters (func: make_cloglike)."
            ))
        }

        if (any(esf_result <= 0) || !is.numeric(esf_result)) {
            stop(paste(
                toString(esf_result),
                "is invalid as esf_result (func: make_cloglike)."
            ))
        }

        cll <- sum(log(item_parameter) * col_sums) - iterim_result - sum(esf_result)

        if (is.na(cll) | !is.finite(cll)) {
            cll <- -.Machine$double.xmax
        }
        return(-cll)
    }
    return(cloglike)
}

make_gradient <- function(persons_raw_scores,
                          items_raw_scores,
                          item_time_limits,
                          ...) {
    gradient_func <- function(difficulty_parameters) {
        esf_result <- rep(0, length(items_raw_scores))
        for (raw_score_index in seq_len(length(persons_raw_scores))) {
            iterim_esf_result <- rpcm_esf(
                persons_raw_scores[raw_score_index],
                difficulty_parameters,
                item_time_limits,
                1,
                raw_score_index,
                ...
            )

            esf_result <- esf_result +
                ((1 / iterim_esf_result[[1]]) * iterim_esf_result[[2]])
        }

        if ((length(esf_result) != length(items_raw_scores))) {
            stop(paste(
                "The esf result",
                toString(esf_result),
                "in gradient_func is not valid."
            ))
        }

        gradient <- c()
        for (item_index in seq_len(length(items_raw_scores))) {
            item_gradient <- (items_raw_scores[item_index] / difficulty_parameters[item_index]) -
                esf_result[item_index]

            if (length(item_gradient) != 1) {
                stop(paste(
                    toString(item_gradient),
                    "is invalid as item gradient"
                ))
            }

            gradient <- c(gradient, item_gradient)
        }

        if (length(gradient) != length(items_raw_scores)) {
            stop(paste(
                "The gradient result",
                toString(gradient),
                "in gradient_func is not valid."
            ))
        }

        return(gradient)
    }
    return(gradient_func)
}