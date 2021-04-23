item_3_difference <- function(output,
                              is_numeric,
                              get_estimate_function,
                              did_find_dif,
                              actual_difference) {
    if (did_find_dif) {
        estimates <- get_estimate_function(
            output,
            is_numeric
        )
        predicted_difference <- estimates$focal[3] - estimates$reference[3]
    } else {
        ## dif was not detected -> difference is the same
        predicted_difference <- 0
    }
    return(predicted_difference - actual_difference)
}

## extracts all item estimates of the rpcm tree
## is_numeric is present just for convenience
rpcmtree_item_estimates <- function(rpcmtree_ouput, is_numeric) {
    get_estimates <- function(node) {
        estimates <- coef(rpcmtree_ouput, node = node)
        estimates <- estimates[
            stringr::str_sort(
                names(estimates),
                numeric = TRUE
            )
        ]
        return(estimates)
    }

    ## node 2 == reference
    ## node 3 == focal
    return(data.frame(
        reference = get_estimates(2),
        focal = get_estimates(3)
    ))
}

## Extracts the item estimates from a glmer ouput.
## transformed_test_data needs to be in the current scope
glmer_item_estimates <- function(glmer_output, is_numeric) {
    if (is_numeric) {
        ## TODO: Verify xlevels = 2 outputs the correct estimates
        glmer_effects <- effects::effect(
            "item*covariate",
            glmer_output,
            xlevels = 2
        )
    } else {
        glmer_effects <- effects::effect(
            "item*covariate",
            glmer_output
        )
    }
    estimates <- as.matrix(summary(glmer_effects)$effect)

    if (is_numeric) {
        colnames(estimates) <- c("reference", "focal")
    }

    estimates <- estimates[
        ## sorts the items
        stringr::str_sort(rownames(estimates), numeric = TRUE),
    ]
    estimates <- log(estimates)
    return(data.frame(
        reference = estimates[, which(colnames(estimates) == "reference")],
        focal = estimates[, which(colnames(estimates) == "focal")]
    ))
}