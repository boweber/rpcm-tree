library(R6)

time_limit_collection <- R6Class("TimeLimitCollection",
    private = list(
        ## a vector containing the log
        ## item time limits
        time_limits = NA
    ),
    public = list(
        initialize = function(time_limits) {
            ## verify each time limit is a positive integer
            if (any(!is_integer(time_limits, TRUE))) {
                stop(paste(
                    toString(time_limits),
                    "is invalid as time limits."
                ))
            }

            private$time_limits <- log(time_limits)
        },
        summed_item_parameters = function(difficulties) {
            if (length(private$time_limits) != length(difficulties)) {
                stop(paste(
                    "Time limits",
                    toString(private$time_limits),
                    "are not compatible with item difficulties",
                    toString(difficulties)
                ))
            }
            return(private$time_limits + difficulties)
        }
    )
)