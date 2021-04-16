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
        ## An idenitifier. Required to compute the ARI
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
                                   fitting_function,
                                   alpha_niveau = 0.05,
                                   item_3_delta = 1,
                                   ability_difference = 0,
                                   numeric_cutpoint = 0.5) {
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
    rpcm_tree_result <- rpcm_tree(
        observations ~ covariate,
        data = test_data,
        fitting_func = fitting_function,
        alpha = alpha_niveau
    )
    ## transforms the test data to the glmer compatible long format
    transformed_test_data <- transform_to_glmer_data(test_data)

    group_specific_intercept <- glmer(
        observation ~ 0 + item + covariate + (1 | id),
        data = transformed_test_data,
        family = "poisson",
        control = glmerControl(
            optimizer = "bobyqa",
            optCtrl = list(maxfun = 2e5)
        )
    )

    group_by_item_intercept <- glmer(
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

    ## compute results

    return(list(
        focal_item_param = focal_group_item_param,
        reference_item_param = reference_group_item_param,
        rpcm = rpcm_tree_result,
        rpcm_did_find_dif = did_find_dif(rpcm_tree_result),
        glmer = glmer_result,
        group_by_item_intercept = group_by_item_intercept,
        glmer_did_find_dif = glmer$`Pr(>Chisq)`[2] < alpha_niveau
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
        return(log(estimates))
    }

    ## node 2 == reference
    ## node 3 == focal
    return(data.frame(
        reference = get_estimates(2),
        focal = get_estimates(3)
    ))
}

adjusted_rand_index <- function(tree, cutpoint, lrcutpoint) {
    ## extracts the original data frm the tree
    initial_data <- data.frame(
        id = tree[[1]]$data$id,
        covariate = tree[[1]]$data$covariate
    )

    ## Divides the original covariate in focal and reference group based
    ## on a cutpoint.
    ## Returns a by-id sorted vector containing group-ids, which
    ## idenitify the reference and focal group.
    ## 0 == reference group
    ## 1 == focal group
    group_ids <- function(test_data, quantile_prob) {
        covariate_split <- quantile(test_data$covariate, quantile_prob)
        test_data$group_id <- with(
            test_data,
            ## mark as reference group
            replace(covariate, covariate < covariate_split, 0)
        )
        test_data$group_id <- with(
            test_data,
            ## mark as focal groups
            replace(covariate, covariate != 0, 1)
        )
        test_data$group_id <- as.factor(test_data$group_id)
        ## sort by id
        test_data <- test_data[order(test_data$id), ]
        return(test_data$group_id)
    }

    ## MARK: true groups
    actual_groups <- group_ids(initial_data, cutpoint)

    ## compute the ari for the tree
    reference_group <- data.frame(
        id = tree[[2]]$data$id,
        group_id = rep.int(0, length(tree[[2]]$data$id))
    )
    focal_group <- data.frame(
        id = tree[[3]]$data$id,
        group_id = rep.int(1, length(tree[[3]]$data$id))
    )
    ## combines focal and reference group
    tree <- rbind(reference_group, focal_group)
    ## sorts combination based on the id
    tree <- tree[order(tree$id), ]
    tree$group_id <- as.factor(tree$group_id)

    ## computes the ari
    tree_result <- mclust::adjustedRandIndex(
        tree$group_id,
        actual_groups
    )

    ## compute the ari for LR
    glmer_result <- mclust::adjustedRandIndex(
        group_ids(initial_data, lrcutpoint),
        actual_groups
    )

    return(data.frame(
        tree_ari = tree_result,
        glmer_ari = glmer_result
    ))
}

## Extracts the item estimates from a glmer ouput.
glmer_item_estimates <- function(glmer_result) {
    estimates <- as.matrix(
        summary(effects::effect("covariate*item", glmer_result))$effect
    )

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