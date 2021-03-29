raw_score_collection <- R6::R6Class("RawScoreCollection",
    private = list(
        ## all unique row sums saved as a raw score
        raw_scores = NA,
        ## a data frame containing occurrence count data
        ## for each raw score
        ## - the first column contains the row sum and is
        ## used as an identifier
        ## - the second column contains the occurrence
        ## count of each row sum / raw score
        details = NA,
        ## a helper function to retreive the occurrence count
        ## for a specific raw score
        get_raw_score_count = function(raw_score) {
            for (row_index in seq_len(nrow(private$details))) {
                if (raw_score$is_row_sum(
                    private$details[row_index, 1]
                )
                ) {
                    return(private$details[row_index, 2])
                }
            }
            stop(paste(
                raw_score,
                "is not saved in raw score collection. Raw score details:",
                toString(private$details)
            ))
        }
    ),
    public = list(
        initialize = function(row_sums, number_of_items) {
            raw_scores <- c()
            meta_data <- data.frame(RowSum = c(), Count = c())
            for (row_sum in row_sums) {
                did_find_raw_score <- FALSE
                ## Ensures that each raw score is unique
                for (row_index in nrow(raw_scores)) {
                    if (nrow(meta_data) == 0) {
                        ## raw scores is empty
                        ## append new element
                        break
                    }
                    if (meta_data[row_index, 1] == row_sum) {
                        ## the current row sum is already in the
                        ## raw_scores vector
                        ## -> increase the occurrence count by one
                        did_find_raw_score <- TRUE
                        meta_data[row_index, 2] <- meta_data[row_index, 2] + 1
                        break
                    }
                }
                if (!did_find_raw_score) {
                    ## creates a new raw score element
                    ## and appends the new element
                    raw_scores <- c(
                        raw_scores,
                        raw_score$new(row_sum, number_of_items)
                    )

                    ## updates the meta data
                    ## regarding the occurrence count
                    meta_data <- rbind(
                        meta_data,
                        data.frame(row_sum, 1)
                    )
                }
            }
            private$raw_scores <- raw_scores
            private$details <- meta_data
        },
        compute_likelihood_component = function(item_difficulties,
                                                time_limits) {
            result <- 0
            for (raw_score in private$raw_scores) {
                esf <- raw_score$esf(
                    item_difficulties,
                    time_limits,
                    0
                )[[1]]
                result <- result +
                    log(esf) *
                        private$get_raw_score_count(raw_score)
            }
            return(result)
        },
        compute_gradient_component = function(item_difficulties,
                                              time_limits) {
            esf_result <- rep(0, length(item_difficulties))
            for (raw_score in private$raw_scores) {
                esf <- raw_score$esf(
                    item_difficulties,
                    time_limits,
                    1
                )


                esf_result <- esf_result +
                    ((1 / esf[[1]]) * esf[[2]]) *
                        private$get_raw_score_count(raw_score)
                if (any(is.nan(esf_result))) {
                    stop(paste(
                        "Invalid difficulty value in compute_gradient_component.",
                        "item difficulities:",
                        toString(item_difficulties),
                        "esf 1:",
                        toString(esf[[1]]),
                        "esf 2:",
                        toString(esf[[2]]),
                        "raw score count:",
                        toString(private$get_raw_score_count(raw_score)),
                        "esf result:",
                        toString(esf_result)
                    ))
                }
            }

            return(esf_result)
        }
    )
)