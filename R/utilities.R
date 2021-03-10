is_integer <- function(value) {
    return(is.numeric(value) && (value %% 1 == 0))
}