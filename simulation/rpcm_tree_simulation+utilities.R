single_case_simulation <- function(with_dif = TRUE,
                                   use_binary = TRUE,
                                   sample_size = 300,
                                   fitting_function = rpcmtree::glmer_fit,
                                   alpha_niveau = 0.05,
                                   item_3_delta = 1,
                                   ability_difference = 0,
                                   numeric_cutpoint = 0.5,
                                   calculate_rmse = TRUE,
                                   ## Used for debugging
                                   skip_rpcm = FALSE,
                                   skip_glmer = FALSE) {
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

    rpcmtree_did_find_dif <- FALSE
    lr_did_find_dif <- FALSE
    rpcm_tree_result <- NA
    ## Just a component to compute the rmse
    ## and not the rmse itself!
    rpcmtree_rmse <- NA
    rpcmtree_time <- NA
    glmer_time <- NA
    glmer_rmse <- NA
    actual_difference <- ifelse(with_dif, item_3_delta, 0)

    if (!skip_rpcm) {
        start_time <- Sys.time()
        rpcm_tree_result <- try(rpcmtree::rpcm_tree(
            observations ~ covariate,
            data = test_data,
            fitting_func = fitting_function,
            alpha = alpha_niveau
        ))
        rpcmtree_time <- difftime(Sys.time(), start_time, units = "secs")[[1]]
        if (!is.error(rpcm_tree_result)) {
            rpcmtree_did_find_dif <- length(rpcm_tree_result) > 1

            if (calculate_rmse) {
                rpcmtree_rmse <- item_3_difference(
                    rpcm_tree_result,
                    !use_binary,
                    rpcmtree_item_estimates,
                    rpcmtree_did_find_dif,
                    actual_difference
                )
            }
        }
    }

    if (!skip_glmer) {
        ## transforms the test data to the glmer compatible long format
        transformed_test_data <- transform_to_glmer_data(test_data)
        if (calculate_rmse) {
            ## in order to use the effect function, transformed_test_data
            ## must be in the global environment
            ## https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf
            assign("transformed_test_data",
                transformed_test_data,
                env = .GlobalEnv
            )
        }
        start_time <- Sys.time()
        try({
            group_specific_intercept <- lme4::glmer(
                observation ~ 0 + item + covariate + (1 | id),
                data = transformed_test_data,
                family = "poisson",
                control = lme4::glmerControl(
                    optimizer = "bobyqa",
                    optCtrl = list(maxfun = 2e5)
                )
            )

            group_by_item_intercept <- lme4::glmer(
                observation ~ 0 + item + covariate + (1 | id) + item * covariate,
                data = transformed_test_data,
                family = "poisson",
                control = lme4::glmerControl(
                    optimizer = "bobyqa",
                    optCtrl = list(maxfun = 2e5)
                )
            )
            ## LR-test
            glmer_result <- lavaan::anova(
                group_specific_intercept,
                group_by_item_intercept
            )
        })

        glmer_time <- difftime(Sys.time(), start_time, units = "secs")[[1]]
        if (!is.error(glmer_result) && !is.error(group_by_item_intercept)) {
            lr_did_find_dif <- glmer_result$`Pr(>Chisq)`[2] < alpha_niveau

            if (calculate_rmse) {
                glmer_rmse <- item_3_difference(
                    group_by_item_intercept,
                    !use_binary,
                    glmer_item_estimates,
                    lr_did_find_dif,
                    actual_difference
                )
                remove("transformed_test_data", envir = .GlobalEnv)
            }
        }
    }
    ## compute results
    return(list(
        rpcm_tree = rpcm_tree_result,
        rpcmtree_did_find_dif = rpcmtree_did_find_dif,
        lr_did_find_dif = lr_did_find_dif,
        rpcm_difference = rpcmtree_rmse,
        lr_difference = glmer_rmse,
        rpcmtree_time = rpcmtree_time,
        lr_time = glmer_time
    ))
}

## http://adv-r.had.co.nz/Exceptions-Debugging.html
is.error <- function(x) inherits(x, "try-error")

## Type 1 error rate:
##  the simulation study was conducted with no DIF.
##  the results for item 3 with no DIF was used to obtain the error rate.
##  % of significant test results (delta == 0)

## Type 2 error rate:
##  - the item included a prespecified difference between the focal and
##    the reference group.
##  - differences to detect their impacts
##  % of significant test results (delta == 1.5)

compute_error_rate <- function(dif_observations,
                               with_dif,
                               simulation_count) {
    ## number of times, dif was detected
    number_of_detected_dif <- sum(
        if (with_dif) !dif_observations else dif_observations,
        na.rm = TRUE
    )
    return(number_of_detected_dif / simulation_count)
}

log_current_condition <- function(condition) {
    print("* Current simulation condition:")
    print(paste(
        "   - dif is simulated:",
        toString(condition$dif)
    ))
    print(paste(
        "   - covariate is binary:",
        toString(condition$binary)
    ))
    print(paste(
        "   - cutpoint is set to:",
        toString(condition$cutpoint)
    ))
    print(paste(
        "   - ability is set to:",
        toString(condition$ability)
    ))
}

log_condition_results <- function(condition_results) {
    print("* Condition results:")
    print(paste(
        "   - rpcm dif detections:",
        toString(condition_results$rpcmtree_did_find_dif)
    ))
    print(paste(
        "   - lr test dif detections:",
        toString(condition_results$lr_did_find_dif)
    ))
    print(paste(
        "   - rpcm computation times:",
        toString(condition_results$rpcmtree_time)
    ))
    print(paste(
        "   - lr test computation times:",
        toString(condition_results$lr_time)
    ))
    print(paste(
        "   - rpcm estimate difference:",
        toString(condition_results$rpcmtree_difference)
    ))
    print(paste(
        "   - glmer estimate difference:",
        toString(condition_results$lr_difference)
    ))
    print(paste(
        "   - rpcm aris:",
        toString(condition_results$rpcmtree_ari)
    ))
    print(paste(
        "   - glmer aris:",
        toString(condition_results$lr_ari)
    ))
}

append_condition_results <- function(condition_results,
                                     simulation_results,
                                     condition_index,
                                     conditions,
                                     with_ari_and_rmse,
                                     simulation_count) {
    simulation_results$rpcm_error_rate[condition_index] <- compute_error_rate(
        condition_results$rpcmtree_did_find_dif,
        conditions[condition_index, ]$dif,
        simulation_count
    )

    simulation_results$glmer_error_rate[condition_index] <- compute_error_rate(
        condition_results$lr_did_find_dif,
        conditions[condition_index, ]$dif,
        simulation_count
    )

    ## Time

    simulation_results$rpcmtree_time[condition_index] <- mean(
        condition_results$rpcmtree_time
    )

    simulation_results$lr_time[condition_index] <- mean(
        condition_results$lr_time
    )

    simulation_results$rpcmtree_time_min[condition_index] <- min(
        condition_results$rpcmtree_time
    )

    simulation_results$lr_time_min[condition_index] <- min(
        condition_results$lr_time
    )

    simulation_results$rpcmtree_time_max[condition_index] <- max(
        condition_results$rpcmtree_time
    )

    simulation_results$lr_time_max[condition_index] <- max(
        condition_results$lr_time
    )

    ## RMSE & ARI

    if (conditions[condition_index, ]$dif && with_ari_and_rmse) {
        simulation_results$rpcm_rmse[condition_index] <- rmse(
            condition_results$rpcmtree_difference
        )

        simulation_results$glmer_rmse[condition_index] <- rmse(
            condition_results$lr_difference
        )
        simulation_results$rpcm_aris[condition_index] <-
            mean(condition_results$rpcmtree_ari, na.rm = TRUE)

        simulation_results$glmer_aris[condition_index] <-
            mean(condition_results$lr_ari, na.rm = TRUE)
    }
    return(simulation_results)
}

set_row_names <- function(simulation_results, conditions) {
    simulation_results <- as.data.frame(simulation_results)

    get_row_name <- function(condition_row) {
        paste(
            "DIF:", condition_row["dif"] == 1,
            "BINARY:", if (is.na(condition_row["binary"])) {
                TRUE
            } else {
                condition_row["binary"] == 1
            },
            "CUTPOINT:", if (is.na(condition_row["cutpoint"])) {
                0.5
            } else {
                condition_row["cutpoint"]
            },
            "ABILITY:", if (is.na(condition_row["ability"])) {
                0
            } else {
                condition_row["ability"]
            }
        )
    }
    row.names(simulation_results) <- apply(conditions, 1, get_row_name)
    return(simulation_results)
}