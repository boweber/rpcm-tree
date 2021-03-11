
occurrence_possibilities <- function(esf_order,
                                     number_of_items,
                                     engine = c("R", "C")) {
    engine <- match.arg(engine)

    if (!is_integer(esf_order) || esf_order <= 0) {
        stop(paste(
            "The esf order",
            esf_order,
            "is not valid (func: occurrence_possibilities)."
        ))
    }

    if (!is_integer(number_of_items) || number_of_items <= 0) {
        stop(paste(
            "The number of items",
            number_of_items,
            "is not valid (func: occurrence_possibilities)."
        ))
    }

    if (engine == "R") {
        number_of_possibilities <- choose(
            esf_order + number_of_items,
            number_of_items
        )

        possibilities <- matrix(NA, number_of_possibilities, number_of_items)
        items <- rep(0, number_of_items)
        possibility_index <- 1
        max_parameter <- (esf_order + 1)^number_of_items

        for (parameter_index in seq_len(max_parameter)) {
            iterim_parameter_index <- parameter_index

            for (item_index in seq_len(number_of_items)) {
                iterim_item_index <- (esf_order + 1)^(number_of_items - item_index)
                rounded_value <- floor(iterim_parameter_index / iterim_item_index)
                items[item_index] <- rounded_value
                iterim_parameter_index <- iterim_parameter_index - rounded_value * iterim_item_index
            }

            if (sum(items) == esf_order) {
                possibilities[possibility_index, ] <- items
                possibility_index <- possibility_index + 1
            }
        }
        possibilities <- possibilities[complete.cases(possibilities), ]
        return(possibilities)
    } else {
        possibilities <- poly_idx_cpp(esf_order, number_of_items)
        possibilities <- possibilities[complete.cases(possibilities), ]
        return(possibilities)
    }
}

rpcm_esf <- function(raw_score,
                     item_difficulties,
                     item_time_limits,
                     order,
                     raw_score_index = NULL,
                     ...) {
    if (!(order %in% 0L:1L)) {
        stop(paste(
            order,
            "is invalid as order argument (func: rpcm_esf)."
        ))
    }
    if (length(item_difficulties) != length(item_time_limits)) {
        stop(paste(
            toString(item_difficulties),
            "as item_difficulties and",
            toString(item_time_limits),
            "as item_time_limits are invalid (func: rpcm_esf)."
        ))
    }
    if (raw_score <= 0 || !is_integer(raw_score)) {
        stop(paste(
            toString(raw_score),
            "is an invalid value for raw_score (func: rpcm_esf)."
        ))
    }

    if (length(item_difficulties) <= 0 || any(!is.numeric(item_difficulties))) {
        stop(paste(
            toString(item_difficulties),
            "is an invalid value for item_difficulties (func: rpcm_esf)."
        ))
    }

    if (any(!is.numeric(item_time_limits))) {
        stop(paste(
            toString(item_time_limits),
            "is an invalid value for item_time_limits (func: rpcm_esf)."
        ))
    }

    possibilities <- occurrence_possibilities(
        raw_score,
        length(item_difficulties),
        ...
    )
    item_parameters <- exp(item_difficulties + item_time_limits)

    ## Each item parameter needs to be positive due to the log function in the esf function
    if (any(item_parameters <= 0)) {
        stop(paste(
            toString(item_parameters),
            "is an invalid value for item_parameters with",
            toString(item_difficulties),
            "as item_difficulties and",
            toString(item_time_limits),
            "as item timelimits"
        ))
    }

    order <- round(order)[1L]
    esf <- list()[1L:(order + 1L)]
    names(esf) <- 0L:order

    esf[[1L]] <- sum(
        sapply(
            possibilities,
            function(y) {
                exp(sum(y * log(item_parameters) - lfactorial(y)))
            }
        )
    )
    if (!is.numeric(esf[[1]])) {
        stop(paste(
            toString(esf),
            "is not valid as esf value (func: rpcm_esf)."
        ))
    }
    if (order == 0) {
        return(esf)
    } else {
        if (is.null(raw_score_index)) {
            stop("The index of the raw score is required for the analytical gradient function")
        }
        derivatives <- c()
        for (item_index in seq_len(length(item_difficulties))) {
            derivatives <- c(
                derivatives,
                (possibilities[raw_score_index, item_index] / item_difficulties[item_index]) *
                    esf[[1L]]
            )
        }

        esf[[2L]] <- derivatives
        return(esf)
    }
}