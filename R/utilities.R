is_count_data <- function(value) {
    is.numeric(value) && (value %% 1 == 0) && (value >= 0)
}