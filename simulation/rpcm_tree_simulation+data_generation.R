generate_abilities <- function(ability_difference,
                               number_of_people) {
    if (!(number_of_people %% 2 == 0)) {
        stop("Invalid number of people.")
    }
    if (ability_difference == 0) {
        ## For simulation studie I:
        ## - both group participants have the same abilities
        abilities <- rnorm(number_of_people / 2, mean = 0, sd = 1)
        return(data.frame(reference = abilities, focal = abilities))
    } else {
        ## For simulation studie II
        ## - the group participants have different abilities
        return(data.frame(
            reference = rnorm(
                number_of_people / 2,
                mean = (0 - ability_difference / 2),
                sd = 1
            ),
            focal = rnorm(
                number_of_people / 2,
                mean = (0 + ability_difference / 2),
                sd = 1
            )
        ))
    }
}

generate_item_parameters <- function(item_3_delta = 0) {
    c(
        1.4, 0.7, 1.2 + item_3_delta, 0.6, 0.5, 1.5, 0.5, 1.2, 1.2, 0.9,
        0.5, 0.5, 1.2, 1.4, 1.1, 0.5, 1.4, 1.2, 1.5, 1.5
    )
}

generate_test_data <- function(focal_group_deltas,
                               reference_group_deltas,
                               number_of_people,
                               ability_difference = 0,
                               should_be_binary = TRUE,
                               quantile_prob = 0.25) {
    abilities <- generate_abilities(ability_difference, number_of_people)
    test_data <- data.frame(
        ## a predictor variable to define randomly the reference
        ## and focal group
        covariate = sample(
            0:ifelse(should_be_binary, 1, 100),
            number_of_people,
            replace = TRUE
        ),
        ## An identifier.
        id = 1:number_of_people
    )

    observations <- matrix(
        NA,
        nrow = number_of_people,
        ncol = length(focal_group_deltas)
    )

    if (!should_be_binary) {
        test_data$covariate <- test_data$covariate / 100 ## numerical reasons
        ## the desired cutpoint to create focal and reference group
        numeric_cutpoint <- quantile(test_data$covariate, probs = quantile_prob)
    }

    ## binary case: 0 < 1 -> TRUE -> reference group
    splitting_rule <- test_data$covariate <
        ifelse(should_be_binary, 1, numeric_cutpoint)

    if (should_be_binary) {
        ## factors the covariate in binary case
        test_data$covariate <- factor(
            test_data$covariate,
            levels = c(0, 1),
            labels = c("reference", "focal")
        )
    }

    for (item_index in seq_len(length(focal_group_deltas))) {
        ## Creates lambdas based on the splitting rule
        lambdas <- ifelse(
            splitting_rule,
            exp(abilities$reference + reference_group_deltas[item_index]),
            exp(abilities$focal + focal_group_deltas[item_index])
        )
        ## Append the random generated observations
        observations[, item_index] <- rpois(number_of_people, lambdas)
    }
    colnames(observations) <- paste("item", seq_len(length(focal_group_deltas)))
    test_data$observations <- observations
    return(test_data)
}

transform_to_glmer_data <- function(test_data) {
    ## transform test data to a long format compatible with glmer

    ## creates a new data frame with each item observations
    ## as the data frames column
    ## (previously: The observations are stored as a matrix in the data frame)
    glmer_test_data <- data.frame(
        covariate = test_data$covariate,
        id = test_data$id,
        test_data$observations
    )
    ## transforms the data frame to a long format, like for example:
    ## covariate id   item observation
    ## 1  1 item.1          11
    ## 1  2 item.1           0
    ## 1  3 item.1           1
    ## 0  4 item.1           3
    ## 0  5 item.1           2
    ## 1  6 item.1           0
    return(tidyr::gather(
        glmer_test_data,
        key = "item",
        value = "observation",
        -id, -covariate
    ))
}