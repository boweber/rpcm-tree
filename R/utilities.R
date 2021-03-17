is_integer <- function(value, should_be_positive = FALSE) {
    result <- is.numeric(value) && (value %% 1 == 0)
    if (should_be_positive) {
        result <- result && (value >= 0)
    }
    return(result)
}

is_R6Class <- function(value, class_name) {
    for (value_class_name in class(value)) {
        if (!(value_class_name == "R6" || value_class_name == class_name)) {
            return(FALSE)
        }
    }
    return(TRUE)
}