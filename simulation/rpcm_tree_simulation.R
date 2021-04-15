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

for (current_condition in seq_len(nrow(conditions))) {
    condition_results <- vector(mode = "list", length = 8)

    for (iteration_number in seq_len(simulation_count)) {
        numeric_cutpoint <- 0.25
        lr_cutpoint <- 0.5
        results <- single_case_simulation(
            with_dif = conditions[current_condition, ]$dif,
            use_binary = conditions[current_condition, ]$binary,
            sample_size = sample_size,
            fitting_function = fitting_function,
            alpha_niveau = alpha_niveau,
            item_3_delta = 1.5,
            ability_difference = 0,
            numeric_cutpoint = numeric_cutpoint
        )

        condition_results[[1]][iteration_number] <- results$rpcm_did_find_dif
        condition_results[[2]][iteration_number] <- results$glmer_did_find_dif

        if (conditions[current_condition, ]$dif) {
            if (results$rpcm_did_find_dif) {
                rpcm_estimates <- tree_item_estimates(results$rpcm)

                condition_results[[3]][iteration_number] <- rmse(
                    predicted = rpcm_estimates$reference,
                    actual = results$reference_item_param
                )
                condition_results[[4]][iteration_number] <- rmse(
                    predicted = irpcm_estimates$focal,
                    actual = results$focal_item_param
                )

                if (!conditions[current_condition, ]$binary) {
                    ari_results <- adjusted_rand_index(
                        esults$rpcm,
                        numeric_cutpoint,
                        lr_cutpoint
                    )
                    condition_results[[5]][
                        iteration_number
                    ] <- ari_results$tree_ari
                    if (results$glmer_did_find_dif) {
                        condition_results[[6]][
                            iteration_number
                        ] <- ari_results$glmer_ari
                    }
                }
            }

            if (results$glmer_did_find_dif) {
                glmer_estimates <- glmer_item_estimates(
                    results$group_by_item_intercept
                )

                condition_results[[7]][iteration_number] <- rmse(
                    predicted = glmer_estimates$reference,
                    actual = results$reference_item_param
                )
                condition_results[[8]][iteration_number] <- rmse(
                    predicted = glmer_estimates$focal,
                    actual = results$focal_item_param
                )
            }
        }
    }

    ## Compute the results of the current simulation conditions

    ## Type 1 error rate:
    ##  the simulation study was conducted with no DIF.
    ##  the results for item 3 with no DIF was used to obtain the error rate.
    ##  % of significant test results (delta == 0)

    ## Type 2 error rate:
    ##  item 3 is the target item for DIF detection
    ##  - the item included a prespecified difference between the focal and
    ##    the reference group.
    ##  - differences to detect their impacts
    ##  % of significant test results (delta == 1.5)
}


## MARK: - Simulation Studie 2

## illustrate the effect of a true ability difference between
## reference and focal group on the type I error rate and power of the LR test and Rasch tree

ability_differences <- c(-0.5, 0.5)

conditions <- expand.grid(
    ## a true dif value simulates DIF
    dif = should_simulate_dif,
    ## a value to generate a difference in ability between focal and reference
    ability = ability_differences
)

for (current_condition in seq_len(nrow(conditions))) {
    condition_results <- vector(mode = "list", length = 2)

    for (iteration_number in seq_len(simulation_count)) {
        results <- single_case_simulation(
            with_dif = conditions[current_condition, ]$dif,
            use_binary = TRUE,
            sample_size = sample_size,
            fitting_function = fitting_function,
            alpha_niveau = alpha_niveau,
            item_3_delta = 1.5,
            ability_difference = conditions[current_condition, ]$ability,
            numeric_cutpoint = 0.25 ## irrelevant in this simulation study
        )

        condition_results[[1]][iteration_number] <- results$rpcm_did_find_dif
        condition_results[[2]][iteration_number] <- results$glmer_did_find_dif
    }
}