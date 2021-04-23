
## MARK: - Simulation

single_case_simulation <- function(with_dif = TRUE,
                                   use_binary = TRUE,
                                   sample_size = 300,
                                   fitting_function = rpcmtree::glmer_fit,
                                   alpha_niveau = 0.05,
                                   item_3_delta = 1,
                                   ability_difference = 0,
                                   numeric_cutpoint = 0.5,
                                   should_log = TRUE,
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
    glmer_rmse <- NA

    if (!skip_rpcm) {
        rpcm_tree_result <- rpcmtree::rpcm_tree(
            observations ~ covariate,
            data = test_data,
            fitting_func = fitting_function,
            alpha = alpha_niveau
        )

        if (should_log) {
            print("Finished Calculating the rpcm tree")
        }
        rpcmtree_did_find_dif <- length(rpcm_tree_result) > 1
        if (calculate_rmse) {
            rpcmtree_rmse <- item_3_difference(
                rpcm_tree_result,
                !use_binary,
                rpcmtree_item_estimates,
                rpcmtree_did_find_dif,
                item_3_delta
            )
        }
    }

    if (!skip_glmer) {
        ## transforms the test data to the glmer compatible long format
        transformed_test_data <- transform_to_glmer_data(test_data)
        if (calculate_rmse) {
            ## in order to use the effect function transformed_test_data
            ## must be in the global environment
            ## https://cran.r-project.org/web/packages/car/vignettes/embedding.pdf
            assign("transformed_test_data",
                transformed_test_data,
                env = .GlobalEnv
            )
        }

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
        ## TODO: Verify < alpha is correct compared to <=
        lr_did_find_dif <- glmer_result$`Pr(>Chisq)`[2] < alpha_niveau

        if (calculate_rmse) {
            glmer_rmse <- item_3_difference(
                group_by_item_intercept,
                !use_binary,
                glmer_item_estimates,
                lr_did_find_dif,
                item_3_delta
            )
            remove("transformed_test_data", envir = .GlobalEnv)
        }
    }

    ## compute results
    return(list(
        rpcm_tree = rpcm_tree_result,
        rpcmtree_did_find_dif = rpcmtree_did_find_dif,
        lr_did_find_dif = lr_did_find_dif,
        rpcmtree_rmse = rpcmtree_rmse,
        glmer_rmse = glmer_rmse,
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