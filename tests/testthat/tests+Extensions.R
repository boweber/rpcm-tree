set.seed(29012021)

generate_glmer_data <- function(person_count = 4, item_count = 4, lambda = 2) {
    generate_individual_data <- function(id, item_count) {
        result <- as.data.frame(
            cbind(
                rep(id, item_count),
                seq(item_count),
                rpois(item_count, lambda)
            )
        )
        colnames(result) <- c("ID", "Item", "Hit")
        return(result)
    }

    some_data <- generate_individual_data(1, item_count)
    for (person_index in 2:person_count) {
        some_data <- rbind(
            some_data,
            generate_individual_data(person_index, item_count)
        )
    }

    return(some_data)
}

transform_glmer_data <- function(some_data) {
    splitted_data <- split(some_data, some_data$ID)
    result <- NULL
    for (person_data_index in seq(splitted_data)) {
        result <- rbind(
            result,
            t(splitted_data[[person_data_index]]$Hit)
        )
    }
    return(result)
}