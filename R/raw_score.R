library(R6)

raw_score <- R6Class("RawScore",
    private = list(
        ## an integer containing a row sum
        score = NA,
        # a matrix containing all summation possibilities
        possibilities = NA,
        ## an integer containing the number of items
        number_of_items = NA,
        get_possibilities = function() {
            ## using is.na results in a warning,
            ## if possibilites are already set
            if (!is.matrix(private$possibilities)) {
                private$possibilities <- raw_score_possibilities(
                    private$score,
                    private$number_of_items,
                    "C"
                )
            }
            return(private$possibilities)
        },
        esf_summation = function(item_index,
                                 item_parameters) {
            sum(
                apply(
                    private$get_possibilities(),
                    1,
                    function(y) {
                        gradient_factor <- if (is.na(item_index)) {
                            1
                        } else {
                            y[item_index]
                        }
                        if (gradient_factor == 0) {
                            return(0)
                        }
                        return(
                            exp(
                                sum(
                                    y * item_parameters - lfactorial(y)
                                )
                            ) * gradient_factor
                            ## / item_difficulties[item_index]
                        )
                    }
                )
            )
        }
    ),
    public = list(
        initialize = function(row_sum, number_of_items) {
            if (is_integer(row_sum, TRUE)) {
                private$score <- row_sum

                if (is_integer(number_of_items, TRUE)) {
                    private$number_of_items <- number_of_items
                } else {
                    stop(paste(
                        toString(number_of_items),
                        "is an invalid number of items."
                    ))
                }
            } else {
                stop(paste(toString(row_sum), "is an invalid raw score."))
            }
        },
        esf = function(item_difficulties, time_limit_collection, order) {
            if (!is_R6Class(time_limit_collection, "TimeLimitCollection")) {
                stop(paste(
                    toString(time_limit_collection),
                    "is not a TimeLimitCollection class."
                ))
            }

            esf <- vector(mode = "list", length = (order + 1))
            item_parameters <- time_limit_collection$summed_item_parameters(
                item_difficulties
            )

            esf[[1L]] <- private$esf_summation(NA, item_parameters)

            if (order == 0) {
                return(esf)
            } else {
                esf[[2L]] <- sapply(
                    seq_len(length(item_difficulties)),
                    function(index) {
                        private$esf_summation(index, item_parameters)
                    }
                )
                return(esf)
            }
        },
        is_row_sum = function(row_sum) {
            private$score == row_sum
        }
    )
)