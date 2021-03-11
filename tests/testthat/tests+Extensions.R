set.seed(29012021)

gen_test_data <- function(true_deltas, n_persons) {
    abilities <- rnorm(n_persons)
    out <- data.frame(
        item1 = numeric(n_persons)
    )
    for (j in 2:length(true_deltas)) {
        out[[paste0("item", j)]] <- numeric(n_persons)
    }
    for (j in 1:length(true_deltas)) {
        lambdas <- exp(abilities + true_deltas[j])
        out[[paste0("item", j)]] <- rpois(n_persons, lambdas)
    }
    return(out)
}