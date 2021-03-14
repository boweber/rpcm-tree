rpcm_esf <- function(raw_score,
                     item_difficulties,
                     item_time_limits,
                     order,
                     engine = c("R", "C")) {
    engine <- match.arg(engine)
    if (engine == "R") {
        possibilities <- raw_score_possibilities(
            raw_score,
            length(item_difficulties),
            engine
        )
        item_parameters <- exp(item_difficulties + item_time_limits)
        esf <- vector(mode = "list", length = (order + 1))

        summation <- function(item_index) {
            sum(
                apply(
                    possibilities,
                    1,
                    function(y) {
                        exp(
                            sum(y * log(item_parameters) - lfactorial(y))
                        ) * (
                            if (is.na(item_index)) 1 else y[item_index]
                        ) ## / item_difficulties[item_index]
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
    } else {
        return(rpcm_esf_c(
            raw_score,
            item_difficulties,
            item_time_limits,
            order
        ))
    }
}