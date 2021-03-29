set.seed(29012021)

gen_test_data_1cov <- function(true_deltas_0, true_deltas_1, n_persons) {
    abilities <- rnorm(n_persons)
    out <- data.frame(
        covariate = sample(0:1, n_persons, replace = TRUE)
    )
    for (j in 1:length(true_deltas_0)) {
        lambdas <- ifelse(
            out[["covariate"]] == 1,
            exp(abilities + true_deltas_1[j]),
            exp(abilities + true_deltas_0[j])
        )
        out[[paste0("item", j)]] <- rpois(n_persons, lambdas)
    }
    return(out)
}

gen_test_data <- function(true_deltas, n_persons) {
    abilities <- rnorm(n_persons)
    out <- data.frame(
        item1 = numeric(n_persons)
    )
    for (j in 2:length(true_deltas)) {
        out[[paste0("item", j)]] <- numeric(n_persons)
    }
    for (j in 1:length(true_deltas)) {
        lambdas <- exp(abilities + true_deltas[j])
        out[[paste0("item", j)]] <- rpois(n_persons, lambdas)
    }
    return(out)
}

prev_rpcm_esf <- function(raw_score,
                          item_difficulties,
                          item_time_limits,
                          order) {
    possibilities <- poly_idx_cpp(
        raw_score,
        length(item_difficulties)
    )
    possibilities <- possibilities[complete.cases(possibilities), ]
    esf <- vector(mode = "list", length = (order + 1))

    summation <- function(item_index) {
        sum(
            apply(
                possibilities,
                1,
                function(y) {
                    exp(
                        sum(y * (item_difficulties + item_time_limits) - lfactorial(y))
                    ) * (
                        if (is.na(item_index)) 1 else y[item_index]
                    )
                }
            )
        )
    }

    esf[[1L]] <- summation(NA)
    if (order == 0) {
        return(esf)
    } else {
        esf[[2L]] <- sapply(
            seq_len(length(item_difficulties)),
            summation
        )
        return(esf)
    }
}

prev_rpcm_analytical_gradient <- function(difficulty,
                                          col_sums,
                                          row_sums,
                                          item_time_limits,
                                          factorial_like_component) {
    esf_result <- rep(0, length(col_sums))
    for (raw_score_index in seq_len(length(row_sums))) {
        esf <- prev_rpcm_esf(
            row_sums[raw_score_index],
            difficulty,
            item_time_limits,
            1
        )

        esf_result <- esf_result + ((1 / esf[[1]]) * esf[[2]])
    }
    return(-(col_sums - esf_result))
}

prev_rpcm_log_likelihood <- function(difficulty,
                                     col_sums,
                                     row_sums,
                                     item_time_limits,
                                     factorial_like_component) {
    cll <- sum((difficulty + item_time_limits) * col_sums) -
        factorial_like_component -
        sum(
            sapply(row_sums, function(row_sum) {
                log(
                    prev_rpcm_esf(
                        row_sum,
                        difficulty,
                        item_time_limits,
                        0
                    )[[1]]
                )
            })
        )

    if (is.na(cll) | !is.finite(cll)) {
        cll <- -.Machine$double.xmax
    }
    return(-cll)
}