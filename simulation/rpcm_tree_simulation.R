## MARK: - Install and load required libraries

if (!require("devtools")) install.packages("devtools")
library("devtools")
if (!require("rpcmtree")) devtools::install_github("boweber/rpcm-tree")
library("rpcmtree", attach.required = TRUE)
library("partykit") ## mob
library("lme4") ## glmer
library("mclust") ## adjustedRandIndex
library("tidyverse") ## str_sort
library("effects") ## effect

## MARK: - Prepare simulation

use_glmer <- TRUE
alpha_niveau <- 0.05
fitting_function <- if (use_glmer) rpcmtree::glmer_fit else rpcmtree::rpcm_fit

### loads helper and data generation functions
source("rpcm_tree_simulation_utilities.R")
simulation_count <- 1
sample_size <- 300
numeric_cutpoint <- 0.25
lr_cutpoint <- 0.5
## item_3_delta == item difficulty difference between focal and reference group
item_3_delta <- 1.5

## Experimental settings:

## Identical for all experiments
## - number of simulations (simulation_count)
## - number of items (length(generate_item_parameters()))
## - number of observations (sample_size)
##  -> all responses were generated with identical item and person parameters,
##  -> or with item and person parameters differing between groups
## - item parameters (generate_item_parameters)
##  -> arbitraily chosen
##  -> ensure adequate overlap between item & person
##     parameter distribution (reference group)
##  -> value delta (0 <= delta <= 1.5) was added to the third
##     item parameter (focal group)
## - person parameters (generate_abilities)
##  -> reference group: N(0 - d / 2, 1)
##  -> focal group: N(0 + d / 2, 1)
##      * -0.5 <= d <= 0.5 (indicates ability difference)
##      * d == 0 (no ability difference)
##      * d < 0 DIF & ability disadvantage (focal group)
##      * d > 0 DIF disatvantages focal while ability difference favors it


## MARK: - Simulation Studie 1


## Simulation Studie I
## illustrate performance of RaschTree (RT) and LR-Test (LR)
## - H_0: no DIF vs H_1: DIF
## LR:
## - pre-specified reference/focal in case of numeric covariate
## - split at median
## delta == 0 corresponds to H_0
##  -> all responses are generated with the same item parameters
## only one covariate (either binary or numeric)
## Cutpoint location: median (or 80)

should_simulate_dif <- c(FALSE, TRUE)
should_be_binary_predictor <- c(FALSE, TRUE)

conditions <- expand.grid(
    ## a true dif value simulates DIF
    dif = should_simulate_dif,
    ## a true binary value uses a binary predictor as a covariate
    binary = should_be_binary_predictor
)

## 0.5 is just relevant for dif == TRUE & binary == FALSE
conditions$cutpoint <- rep(0.5, nrow(conditions))

conditions <- rbind(
    conditions,
    data.frame(
        dif = TRUE,
        binary = FALSE,
        cutpoint = 0.25
    )
)


simulation_results <- vector(mode = "list", length = 6)

for (current_condition in seq_len(nrow(conditions))) {
    condition_results <- vector(mode = "list", length = 6)

    for (iteration_number in seq_len(simulation_count)) {
        results <- single_case_simulation(
            with_dif = conditions[current_condition, ]$dif,
            use_binary = conditions[current_condition, ]$binary,
            sample_size = sample_size,
            fitting_function = fitting_function,
            alpha_niveau = alpha_niveau,
            item_3_delta = item_3_delta,
            ability_difference = 0,
            numeric_cutpoint = conditions[current_condition, ]$cutpoint
        )

        condition_results[[1]][iteration_number] <- results$rpcm_did_find_dif
        condition_results[[2]][iteration_number] <- results$glmer_did_find_dif

        if (conditions[current_condition, ]$dif) {

            ## MARK: ARI

            if (!conditions[current_condition, ]$binary) {
                ari_results <- adjusted_rand_index(
                    esults$rpcm,
                    numeric_cutpoint,
                    lr_cutpoint,
                    results$rpcm_did_find_dif,
                    results$glmer_did_find_dif
                )

                condition_results[[3]][iteration_number] <- ari_results$tree_ari
                condition_results[[4]][iteration_number] <- ari_results$glmer_ari
            }

            ## MARK: RPMSE
            ## predicted <- (item_3 estimate of the reference_group) -
            ##                       (item_3 estimate of the focal_group)
            ## actual <- (true difficulty of item 3 for reference group) -
            ##                       (true difficulty of item 3 for focal group)
            ## Here: actual is equal to the item_3_delta value

            ## * RPCM-Tree

            if (results$rpcm_did_find_dif) {
                rpcm_estimates <- tree_item_estimates(results$rpcm)
                rpcm_predicted <- rpcm_estimates$reference[3] -
                    rpcm_estimates$focal[3]

                condition_results[[5]][iteration_number] <- rpcm_predicted -
                    item_3_delta
            }

            ## * LR-Test

            if (results$glmer_did_find_dif) {
                glmer_estimates <- glmer_item_estimates(
                    results$group_by_item_intercept
                )
                glmer_predicted <- glmer_estimates$reference[3] -
                    glmer_estimates$focal[3]

                condition_results[[6]][iteration_number] <- glmer_predicted -
                    item_3_delta
            }
        }
    }

    ## Compute the results of the current simulation conditions

    ## Type 1 error rate:
    ##  the simulation study was conducted with no DIF.
    ##  the results for item 3 with no DIF was used to obtain the error rate.
    ##  % of significant test results (delta == 0)

    ## Type 2 error rate:
    ##  - the item included a prespecified difference between the focal and
    ##    the reference group.
    ##  - differences to detect their impacts
    ##  % of significant test results (delta == 1.5)

    simulation_results[[1]][current_condition] <- compute_error_rate(
        condition_results[[1]],
        conditions[current_condition, ]$dif,
        simulation_count
    )

    simulation_results[[2]][current_condition] <- compute_error_rate(
        condition_results[[2]],
        conditions[current_condition, ]$dif,
        simulation_count
    )

    if (conditions[current_condition, ]$dif) {

        ## TODO: Handle NA case

        simulation_results[[3]][current_condition] <- sqrt(
            (1 / length(condition_results[[5]])) *
                sum((condition_results[[5]])^2)
        )

        simulation_results[[4]][current_condition] <- sqrt(
            (1 / length(condition_results[[6]])) *
                sum((condition_results[[6]])^2)
        )

        simulation_results[[5]][current_condition] <-
            mean(condition_results[[3]])

        simulation_results[[6]][current_condition] <-
            mean(condition_results[[4]])
    }
}

## MARK: - Simulation Studie 2

## illustrate the effect of a true ability difference between
## reference and focal group on the type I error rate
## and power of the LR test and Rasch tree

ability_differences <- c(-0.5, 0.5)

conditions <- expand.grid(
    ## a true dif value simulates DIF
    dif = should_simulate_dif,
    ## a value to generate a difference in ability between focal and reference
    ability = ability_differences
)

simulation_2_results <- vector(mode = "list", length = 6)

for (current_condition in seq_len(nrow(conditions))) {
    condition_results <- vector(mode = "list", length = 2)

    for (iteration_number in seq_len(simulation_count)) {
        results <- single_case_simulation(
            with_dif = conditions[current_condition, ]$dif,
            use_binary = TRUE,
            sample_size = sample_size,
            fitting_function = fitting_function,
            alpha_niveau = alpha_niveau,
            item_3_delta = item_3_delta,
            ability_difference = conditions[current_condition, ]$ability,
            ## Just a dummy: Irrelevant in this simulation study
            numeric_cutpoint = numeric_cutpoint
        )

        condition_results[[1]][iteration_number] <- results$rpcm_did_find_dif
        condition_results[[2]][iteration_number] <- results$glmer_did_find_dif
    }

    simulation_2_results[[1]][current_condition] <- compute_error_rate(
        condition_results[[1]],
        conditions[current_condition, ]$dif,
        simulation_count
    )

    simulation_2_results[[2]][current_condition] <- compute_error_rate(
        condition_results[[2]],
        conditions[current_condition, ]$dif,
        simulation_count
    )
}