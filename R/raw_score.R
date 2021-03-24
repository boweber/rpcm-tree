raw_score <- R6::R6Class("RawScore",
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
                possibilities <- poly_idx_cpp(
                    private$score,
                    private$number_of_items
                )
                ## removes rows where all values are 0
                private$possibilities <- possibilities[
                    apply(possibilities[, -1], 1, function(x) !all(x == 0)),
                ]
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
                            exp(sum(y * item_parameters - lfactorial(y))) *
                                gradient_factor
                            ## / item_difficulties[item_index]
                        )
                    }
                )
            )
        }
    ),
    public = list(
        initialize = function(row_sum, number_of_items) {
            if (is_count_data(row_sum)) {
                private$score <- row_sum

                if (is_count_data(number_of_items)) {
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
        esf = function(item_difficulties, time_limits, order) {
            esf <- vector(mode = "list", length = (order + 1))
            item_parameters <- item_difficulties + time_limits

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