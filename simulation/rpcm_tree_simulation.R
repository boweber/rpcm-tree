## MARK: - Install and load required libraries

if (!require("devtools")) install.packages("devtools")
if (!require("rpcmtree")) devtools::install_github("boweber/rpcm-tree")
library("rpcmtree")
library("partykit") ## mob
library("lme4") ## glmer
library("mclust") ## adjustedRandIndex
library("tidyverse") ## str_sort
library("effects") ## effect
library("merDeriv") ## estfun.glmerMod
library("tictoc") ## tic toc
## parallelisation
library("doParallel")
library("doRNG")

## MARK: - Prepare simulation

should_log <- TRUE
use_glmer <- TRUE
alpha_niveau <- 0.05
number_of_clusters <- 2 # NA
fitting_function <- if (use_glmer) rpcmtree::glmer_fit else rpcmtree::rpcm_fit

### loads helper and data generation functions
source("simulation/rpcm_tree_simulation+data_generation.R")
source("simulation/rpcm_tree_simulation+rmse.R")
source("simulation/rpcm_tree_simulation+ari.R")
source("simulation/rpcm_tree_simulation+utilities.R")
simulation_count <- 2
sample_size <- 300
## the cutpoint of the LR-Test
## Here 0.5 == median
lr_cutpoint <- 0.5
## item_3_delta == item difficulty difference between focal and reference group
## only present when dif is simulated
item_3_delta <- 0.35

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

simulation_1_results <- vector(mode = "list")
if (should_log) tic("Total")

number_of_clusters <- ifelse(
    is.na(number_of_clusters),
    parallel::detectCores() - 1,
    number_of_clusters
)
cluster <- parallel::makeCluster(number_of_clusters)
doParallel::registerDoParallel(cluster)
doRNG::registerDoRNG(17)

for (current_condition in seq_len(nrow(conditions))) {
    ## debugging tool: Skip conditions
    if (any(c() == current_condition)) next

    if (should_log) {
        log_current_condition(
            conditions[current_condition, ]
        )
        tic(paste("condition number", toString(current_condition)))
    }
    condition_results <- foreach::foreach(
        iteration_number = seq_len(simulation_count),
        .combine = "rbind"
    ) %dopar% {
        library("partykit")
        single_case_result <- single_case_simulation(
            with_dif = conditions[current_condition, ]$dif,
            use_binary = conditions[current_condition, ]$binary,
            sample_size = sample_size,
            fitting_function = fitting_function,
            alpha_niveau = alpha_niveau,
            item_3_delta = item_3_delta,
            ability_difference = 0,
            numeric_cutpoint = conditions[current_condition, ]$cutpoint,
            calculate_rmse = conditions[current_condition, ]$dif
        )
        tree_ari <- NA
        lr_ari <- NA
        if (conditions[current_condition, ]$dif &&
            !conditions[current_condition, ]$binary &&
            !is.error(single_case_result$rpcm_tree)) {
            ari_results <- adjusted_rand_index(
                single_case_result$rpcm_tree,
                conditions[current_condition, ]$cutpoint,
                lr_cutpoint,
                single_case_result$rpcmtree_did_find_dif,
                single_case_result$lr_did_find_dif
            )
            tree_ari <- ari_results$tree_ari
            lr_ari <- ari_results$glmer_ari
        }

        return(data.frame(
            rpcmtree_did_find_dif = single_case_result$rpcmtree_did_find_dif,
            lr_did_find_dif = single_case_result$lr_did_find_dif,
            rpcmtree_difference = single_case_result$rpcm_difference,
            lr_difference = single_case_result$lr_difference,
            rpcmtree_time = single_case_result$rpcmtree_time,
            lr_time = single_case_result$lr_time,
            rpcmtree_ari = tree_ari,
            lr_ari = lr_ari
        ))
    }

    if (should_log) toc()
    simulation_1_results <- append_condition_results(
        condition_results = condition_results,
        simulation_results = simulation_1_results,
        condition_index = current_condition,
        conditions = conditions,
        should_log = should_log,
        with_ari_and_rmse = TRUE,
        simulation_count = simulation_count
    )
}
if (should_log) toc()
## Total: 4043.519 sec elapsed iteration_count == 4
save(simulation_1_results, file = "Simulation_Study_I.RData")

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

simulation_2_results <- vector(mode = "list")
if (should_log) tic("Total")

for (current_condition in seq_len(nrow(conditions))) {
    condition_results <- vector(mode = "list")

    if (should_log) {
        log_current_condition(
            conditions[current_condition, ]
        )
        tic(paste("condition number", toString(current_condition)))
    }
    condition_results <- foreach::foreach(
        iteration_number = seq_len(simulation_count),
        .combine = "rbind"
    ) %dopar% {
        library("partykit") ## necessary for mob function
        single_case_result <- single_case_simulation(
            with_dif = conditions[current_condition, ]$dif,
            use_binary = TRUE,
            sample_size = sample_size,
            fitting_function = fitting_function,
            alpha_niveau = alpha_niveau,
            item_3_delta = item_3_delta,
            ability_difference = conditions[current_condition, ]$ability,
            ## Just a dummy: Irrelevant in this simulation study
            numeric_cutpoint = 0.5,
            calculate_rmse = FALSE
        )
        return(data.frame(
            rpcmtree_did_find_dif = single_case_result$rpcmtree_did_find_dif,
            lr_did_find_dif = single_case_result$lr_did_find_dif,
            rpcmtree_time = single_case_result$rpcmtree_time,
            lr_time = single_case_result$lr_time
        ))
    }
    if (should_log) toc()
    simulation_2_results <- append_condition_results(
        condition_results = condition_results,
        simulation_results = simulation_2_results,
        condition_index = current_condition,
        conditions = conditions,
        should_log = should_log,
        with_ari_and_rmse = FALSE,
        simulation_count = simulation_count
    )
}
if (should_log) toc() ## 1484.347 sec elapsed
save(simulation_2_results, file = "Simulation_Study_II.RData")