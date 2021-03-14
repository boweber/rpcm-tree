raw_score_possibilities <- function(raw_score,
                                    number_of_items,
                                    engine = c("R", "C")) {
    engine <- match.arg(engine)
    if (engine == "R") {

        ## MARK: R

        number_of_possibilities <- choose(
            raw_score + number_of_items,
            number_of_items
        )

        possibilities <- matrix(NA, number_of_possibilities, number_of_items)
        items <- rep(0, number_of_items)
        possibility_index <- 1
        max_parameter <- (raw_score + 1)^number_of_items

        for (parameter_index in seq_len(max_parameter)) {
            iterim_parameter_index <- parameter_index

            for (item_index in seq_len(number_of_items)) {
                iterim_item_index <- (raw_score + 1)^(number_of_items - item_index)
                rounded_value <- floor(iterim_parameter_index / iterim_item_index)
                items[item_index] <- rounded_value
                iterim_parameter_index <- iterim_parameter_index - rounded_value * iterim_item_index
            }

            if (sum(items) == raw_score) {
                possibilities[possibility_index, ] <- items
                possibility_index <- possibility_index + 1
            }
        }
        possibilities <- possibilities[complete.cases(possibilities), ]
        return(possibilities)
    } else {

        ## MARK: C

        possibilities <- poly_idx_cpp(raw_score, number_of_items)
        possibilities <- possibilities[complete.cases(possibilities), ]
        return(possibilities)
    }
}