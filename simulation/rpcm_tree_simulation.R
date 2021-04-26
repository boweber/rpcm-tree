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
number_of_clusters <- 2
fitting_function <- if (use_glmer) rpcmtree::glmer_fit else rpcmtree::rpcm_fit

### loads helper and data generation functions
source("simulation/rpcm_tree_simulation+data_generation.R")
source("simulation/rpcm_tree_simulation+rmse.R")
source("simulation/rpcm_tree_simulation+ari.R")
source("simulation/rpcm_tree_simulation+utilities.R")
simulation_count <- 120
sample_size <- 300
## the cutpoint of the LR-Test
## Here 0.5 == median
lr_cutpoint <- 0.5
## item_3_delta == item difficulty difference between focal and reference group
## only present when dif is simulated
item_3_delta <- 0.4

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

simulation_results <- vector(mode = "list")

if (should_log) {
    tic("Total")
}

# MB for parallelization
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
    iterations_results <- foreach::foreach(
        iteration_number = seq_len(simulation_count)
    ) %dopar% {
        library("partykit")
        iteration_results <- vector(mode = "list")

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

        iteration_results$rpcmtree_did_find_dif <-
            single_case_result$rpcmtree_did_find_dif
        iteration_results$lr_did_find_dif <-
            single_case_result$lr_did_find_dif

        iteration_results$rpcmtree_time <- single_case_result$rpcmtree_time
        iteration_results$lr_time <- single_case_result$lr_time

        if (conditions[current_condition, ]$dif) {

            ## MARK: ARI

            if (!conditions[current_condition, ]$binary && !is.error(single_case_result$rpcm_tree)) {
                ari_results <- adjusted_rand_index(
                    single_case_result$rpcm_tree,
                    conditions[current_condition, ]$cutpoint,
                    lr_cutpoint,
                    single_case_result$rpcmtree_did_find_dif,
                    single_case_result$lr_did_find_dif
                )
                iteration_results$rpcmtree_ari <- ari_results$tree_ari
                iteration_results$glmer_ari <- ari_results$glmer_ari
            }

            ## MARK: RPMSE
            ## predicted <- (item_3 estimate of the reference_group) -
            ##                  (item_3 estimate of the focal_group)
            ## actual <- (true difficulty of item 3 for reference group) -
            ##               (true difficulty of item 3 for focal group)
            ## Here: actual is equal to the item_3_delta value in case
            ##       of dif
            iteration_results$rpcm_difference <-
                single_case_result$rpcmtree_rmse

            iteration_results$lr_difference <-
                single_case_result$glmer_rmse
        }
        return(iteration_results)
    }
    if (should_log) toc()

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

    condition_results <- vector(mode = "list")

    for (simulation_index in seq_len(length(iterations_results))) {
        condition_results$rpcm_dif_detections[simulation_index] <-
            iterations_results[[simulation_index]]$rpcmtree_did_find_dif
        condition_results$glmer_dif_detections[simulation_index] <-
            iterations_results[[simulation_index]]$lr_did_find_dif

        condition_results$rpcmtree_times[simulation_index] <-
            iterations_results[[simulation_index]]$rpcmtree_time
        condition_results$lr_times[simulation_index] <-
            iterations_results[[simulation_index]]$lr_time

        condition_results$rpcm_differences[simulation_index] <-
            iterations_results[[simulation_index]]$rpcm_difference
        condition_results$glmer_differences[simulation_index] <-
            iterations_results[[simulation_index]]$lr_difference

        condition_results$rpcm_aris[simulation_index] <-
            iterations_results[[simulation_index]]$rpcmtree_ari
        condition_results$glmer_aris[simulation_index] <-
            iterations_results[[simulation_index]]$glmer_ari
    }

    if (should_log) {
        log_condition_results(condition_results)
    }

    simulation_results$rpcm_error_rate[current_condition] <- compute_error_rate(
        condition_results$rpcm_dif_detections,
        conditions[current_condition, ]$dif,
        simulation_count
    )

    simulation_results$glmer_error_rate[current_condition] <- compute_error_rate(
        condition_results$glmer_dif_detections,
        conditions[current_condition, ]$dif,
        simulation_count
    )

    simulation_results$rpcmtree_time[current_condition] <- mean(
        condition_results$rpcmtree_time
    )

    simulation_results$lr_time[current_condition] <- mean(
        condition_results$lr_times
    )

    if (conditions[current_condition, ]$dif) {
        simulation_results$rpcm_rmse[current_condition] <- rmse(
            condition_results$rpcm_differences
        )

        simulation_results$glmer_rmse[current_condition] <- rmse(
            condition_results$glmer_differences
        )
        if (is.vector(condition_results$rpcm_aris)) {
            simulation_results$rpcm_aris[current_condition] <-
                mean(condition_results$rpcm_aris, na.rm = TRUE)

            simulation_results$glmer_aris[current_condition] <-
                mean(condition_results$glmer_aris, na.rm = TRUE)
        }
    }
}
if (should_log) toc()
## Total: 4043.519 sec elapsed iteration_count == 4
save(simulation_results, file = "Simulation_Study_I.RData")



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
    iterations_results <- foreach::foreach(
        iteration_number = seq_len(simulation_count)
    ) %dopar% {
        library("partykit")
        iteration_results <- vector(mode = "list")
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
        iteration_results$rpcmtree_did_find_dif <-
            single_case_result$rpcmtree_did_find_dif
        iteration_results$lr_did_find_dif <-
            single_case_result$lr_did_find_dif

        iteration_results$rpcmtree_time <- single_case_result$rpcmtree_time
        iteration_results$lr_time <- single_case_result$lr_time
        return(iteration_results)
    }

    if (should_log) toc()

    condition_results <- vector(mode = "list")

    for (simulation_index in seq_len(length(iterations_results))) {
        condition_results$rpcm_dif_detections[simulation_index] <-
            iterations_results[[simulation_index]]$rpcmtree_did_find_dif
        condition_results$glmer_dif_detections[simulation_index] <-
            iterations_results[[simulation_index]]$lr_did_find_dif

        condition_results$rpcmtree_times[simulation_index] <-
            iterations_results[[simulation_index]]$rpcmtree_time
        condition_results$lr_times[simulation_index] <-
            iterations_results[[simulation_index]]$lr_time
    }

    if (should_log) {
        log_condition_results(condition_results)
    }

    simulation_2_results$rpcm_error_rate[current_condition] <- compute_error_rate(
        condition_results$rpcm_dif_detections,
        conditions[current_condition, ]$dif,
        simulation_count
    )

    simulation_2_results$glmer_error_rate[current_condition] <- compute_error_rate(
        condition_results$glmer_dif_detections,
        conditions[current_condition, ]$dif,
        simulation_count
    )
}
if (should_log) toc() ## 1484.347 sec elapsed
save(simulation_2_results, file = "Simulation_Study_II.RData")