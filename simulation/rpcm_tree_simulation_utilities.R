## MARK: - Generate data

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
        ## An idenitifier.
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

## MARK: - Simulation

single_case_simulation <- function(with_dif = TRUE,
                                   use_binary = TRUE,
                                   sample_size = 300,
                                   fitting_function = rpcmtree::glmer_fit,
                                   alpha_niveau = 0.05,
                                   item_3_delta = 1,
                                   ability_difference = 0,
                                   numeric_cutpoint = 0.5,
                                   should_log) {
    ## generates item parameters for both focal and reference group
    ## dependent on possible present DIF.
    focal_group_item_param <- generate_item_parameters(
        ifelse(with_dif, item_3_delta, 0)
    )
    reference_group_item_param <- generate_item_parameters()
    ## generates test data
    test_data <- generate_test_data(
        focal_group_item_param,
        reference_group_item_param,
        sample_size,
        ability_difference = ability_difference,
        use_binary,
        numeric_cutpoint
    )
    ## calulates the rpcm tree
    rpcm_tree_result <- rpcmtree::rpcm_tree(
        observations ~ covariate,
        data = test_data,
        fitting_func = fitting_function,
        alpha = alpha_niveau
    )
    if (should_log) {
        print("Finished Calculating the rpcm tree")
    }
    ## transforms the test data to the glmer compatible long format
    transformed_test_data <- transform_to_glmer_data(test_data)

    group_specific_intercept <- lme4::glmer(
        observation ~ 0 + item + covariate + (1 | id),
        data = transformed_test_data,
        family = "poisson",
        control = glmerControl(
            optimizer = "bobyqa",
            optCtrl = list(maxfun = 2e5)
        )
    )

    group_by_item_intercept <- lme4::glmer(
        observation ~ 0 + item + covariate + (1 | id) + item * covariate,
        data = transformed_test_data,
        family = "poisson",
        control = glmerControl(
            optimizer = "bobyqa",
            optCtrl = list(maxfun = 2e5)
        )
    )
    ## LR-test
    glmer_result <- anova(
        group_specific_intercept,
        group_by_item_intercept
    )

    if (should_log) {
        print("Finished the LR-Test")
    }

    ## compute results
    return(list(
        focal_item_param = focal_group_item_param,
        reference_item_param = reference_group_item_param,
        rpcm = rpcm_tree_result,
        rpcm_did_find_dif = did_find_dif(rpcm_tree_result),
        glmer = glmer_result,
        group_by_item_intercept = group_by_item_intercept,
        ## TODO: Verify < alpha is correct compared to <=
        glmer_did_find_dif = glmer_result$`Pr(>Chisq)`[2] < alpha_niveau,
        transformed_test_data = transformed_test_data
    ))
}

## MARK: - Utilities

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

## computes the rmse
rmse <- function(predicted, actual) {
    sqrt(
        (1 / length(predicted)) *
            sum((predicted - actual)^2)
    )
}

## checks if the rpcm tree found difs
did_find_dif <- function(tree) {
    ## length(rt) returns the number of nodes
    length(rt) > 1
}

## extracts all item estimates of the rpcm tree
tree_item_estimates <- function(tree) {
    get_estimates <- function(node) {
        estimates <- coef(tree, node = node)
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

adjusted_rand_index <- function(tree,
                                cutpoint,
                                lrcutpoint,
                                tree_did_find_dif,
                                glmer_did_find_dif) {
    ## original data
    initial_covariate <- tree[[1]]$data$covariate

    if (tree_did_find_dif) {
        numeric_cutpoint <- quantile(initial_covariate, cutpoint)
        ## compute the ari for the tree

        rpcm_group_ids <- function(tree_node, group_id) {
            if (group_id == 0) {
                ## reference group
                predicted_group <- tree_node$data$covariate < numeric_cutpoint

                ## TRUE == 1, which is not the desired group_id 0. Therefore
                ## use the inverse of the result.
                predicted_group <- !predicted_group
            } else {
                predicted_group <- tree_node$data$covariate >= numeric_cutpoint
            }
            return(data.frame(
                actual_group = as.factor(
                    rep.int(group_id, length(tree_node$data$covariate))
                ),
                predicted_group = as.factor(as.numeric(predicted_group))
            ))
        }

        reference_group <- rpcm_group_ids(tree[[2]], 0)
        focal_group <- rpcm_group_ids(tree[[3]], 1)
        ## combines focal and reference group
        tree_groups <- rbind(reference_group, focal_group)
        ## computes the ari
        tree_result <- mclust::adjustedRandIndex(
            tree_groups$predicted_group,
            tree_groups$actual_group
        )
    } else {
        tree_result <- NA
    }

    if (glmer_did_find_dif) {

        ## Divides the original covariate in focal and reference group based
        ## on a cutpoint.
        ## Returns a by-id sorted vector containing group-ids, which
        ## idenitify the reference and focal group.
        ## 0 == reference group
        ## 1 == focal group
        static_group_ids <- function(quantile_prob) {
            covariate_split <- quantile(initial_covariate, quantile_prob)
            group_ids <- initial_covariate < covariate_split
            group_ids <- as.factor(as.numeric(group_ids))
            return(group_ids)
        }

        ## compute the ari for LR
        glmer_result <- mclust::adjustedRandIndex(
            static_group_ids(lrcutpoint),
            static_group_ids(cutpoint)
        )
    } else {
        glmer_result <- NA
    }

    return(data.frame(
        tree_ari = tree_result,
        glmer_ari = glmer_result
    ))
}

## Extracts the item estimates from a glmer ouput.
## transformed_test_data needs to be in the current scope
glmer_item_estimates <- function(glmer_result, is_numeric, transformed_test_data) {
    if (is_numeric) {
        ## TODO: Verify xlevels = 2 outputs the correct estimates
        glmer_effects <- effects::effect("item*covariate", glmer_result, xlevels = 2)
    } else {
        glmer_effects <- effects::effect("item*covariate", glmer_result)
    }
    estimates <- as.matrix(summary(glmer_effects)$effect)

    if (is_numeric) {
        estimates <- t(estimates)
        rownames(estimates) <- c("reference", "focal")
    }

    estimates <- estimates[
        ## sorts the items
        , stringr::str_sort(colnames(estimates), numeric = TRUE)
    ]
    estimates <- log(estimates)
    return(data.frame(
        reference = estimates[which(rownames(estimates) == "reference"), ],
        focal = estimates[which(rownames(estimates) == "focal"), ]
    ))
}

compute_error_rate <- function(dif_observations,
                               with_dif,
                               simulation_count) {
    ## number of times, dif was detected
    number_of_detected_dif <- sum(
        ifelse(with_dif, !dif_observations, dif_observations),
        na.rm = TRUE
    )
    return(number_of_detected_dif / simulation_count)
}