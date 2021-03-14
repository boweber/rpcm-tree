rpcm_esf <- function(raw_score,
                     item_difficulties,
                     item_time_limits,
                     order,
                     raw_score_index,
                     ...) {
    possibilities <- raw_score_possibilities(
        raw_score,
        length(item_difficulties),
        ...
    )
    item_parameters <- exp(item_difficulties + item_time_limits)

    order <- round(order)[1L]
    esf <- list()[1L:(order + 1L)]
    names(esf) <- 0L:order

    esf[[1L]] <- sum(
        apply(
            possibilities,
            1,
            function(y) {
                exp(sum(y * log(item_parameters) - lfactorial(y)))
            }
        )
    )
    if (order == 0) {
        return(esf)
    } else {
        derivatives <- c()
        for (item_index in seq_len(length(item_difficulties))) {
            d <- sum(
                apply(
                    possibilities,
                    1,
                    function(y) {
                        exp(sum(y * log(item_parameters) - lfactorial(y))) * y[item_index]
                        ## / item_difficulties[item_index]
                    }
                )
            )

            derivatives <- c(derivatives, d)
        }

        esf[[2L]] <- derivatives
        return(esf)
    }
}