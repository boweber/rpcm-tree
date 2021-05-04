## MARK. - Load input parameters

argument_list <- list(
    optparse::make_option(
        c("--run_simulation_study_1"),
        action = "store",
        type = "logical",
        default = TRUE,
        help = "Runs simulation study 1 [default]"
    ),
    optparse::make_option(
        c("--run_simulation_study_2"),
        action = "store",
        type = "logical",
        default = TRUE,
        help = "Runs simulation study 2 [default]"
    ),
    optparse::make_option(
        c("-l", "--should_log"),
        action = "store",
        type = "logical",
        default = TRUE,
        help = "Logs the current state of the simulation [default]"
    ),
    optparse::make_option(
        c("-g", "--use_glmer"),
        action = "store",
        type = "logical",
        default = TRUE,
        help = "Uses glmer as a fitting function [default]"
    ),
    optparse::make_option(
        c("-a", "--alpha_niveau"),
        action = "store",
        type = "double",
        default = 0.05,
        help = "Uses glmer as a fitting function [default %default]"
    ),
    optparse::make_option(
        c("-c", "--number_of_clusters"),
        action = "store",
        type = "integer",
        default = NULL,
        help = "Specify the number of clusters [default all available cores]"
    ),
    optparse::make_option(
        c("-r", "--number_of_repetitions"),
        action = "store",
        type = "integer",
        default = 500,
        help = "Specify the number of repetitions [default %default]"
    ),
    optparse::make_option(
        c("-s", "--sample_size"),
        action = "store",
        type = "integer",
        default = 500,
        help = "Specify the sample size [default %default]"
    ),
    optparse::make_option(
        c("-d", "--item_3_delta"),
        action = "store",
        type = "double",
        default = 0.23,
        help = "Specify the difficulty difference for item 3 delta in case of DIF [default %default]"
    ),
    optparse::make_option(
        c("-p", "--lr_cutpoint"),
        action = "store",
        type = "double",
        default = 0.5,
        help = "Specify the cutpoint for the lr test [default %default]"
    ),
    optparse::make_option(
        c("--output_file_path"),
        action = "store",
        type = "character",
        default = ".",
        help = "Specify the output file path [default %default]"
    )
)

options <- optparse::parse_args(
    optparse::OptionParser(
        usage = "%prog [options] file",
        option_list = argument_list
    ),
    positional_arguments = TRUE
)$options

## MARK: - Prepare workspace

library("doParallel")

source("rpcm_tree_simulation+rpcmtree.R")
source("rpcm_tree_simulation+data_generation.R")
source("rpcm_tree_simulation+rmse.R")
source("rpcm_tree_simulation+ari.R")
source("rpcm_tree_simulation+utilities.R")

should_log <- options$should_log
alpha_niveau <- options$alpha_niveau
fitting_function <- if (options$use_glmer) glmer_fit else rpcm_fit
simulation_count <- options$number_of_repetitions
sample_size <- options$sample_size
lr_cutpoint <- options$lr_cutpoint
## item_3_delta == item difficulty difference
## between focal and reference group
## only present when dif is simulated
item_3_delta <- options$item_3_delta

## MARK. - Setup parallelisation

number_of_clusters <- ifelse(
    is.na(options$number_of_clusters),
    parallel::detectCores() - 1,
    options$number_of_clusters
)
cluster <- parallel::makeCluster(number_of_clusters)
doParallel::registerDoParallel(cluster)
doRNG::registerDoRNG(17)

## MARK: - Simulation Studie 1

if (options$run_simulation_study_1) {
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

    if (should_log) {
        tictoc::tic("Total")
        print("Starting simulation study I")
    }

    simulation_1_results <- vector(mode = "list")
    for (current_condition in seq_len(nrow(conditions))) {
        ## debugging tool: Skip conditions
        if (any(c() == current_condition)) next

        if (should_log) {
            log_current_condition(
                conditions[current_condition, ]
            )
            tictoc::tic(paste("condition number", toString(current_condition)))
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
        if (should_log) {
            log_condition_results(condition_results)
            tictoc::toc()
        }

        simulation_1_results <- append_condition_results(
            condition_results = condition_results,
            simulation_results = simulation_1_results,
            condition_index = current_condition,
            conditions = conditions,
            with_ari_and_rmse = TRUE,
            simulation_count = simulation_count
        )
    }
    if (should_log) tictoc::toc()
    simulation_1_results <- set_row_names(simulation_1_results, conditions)
} else {
    simulation_1_results <- NA
}

## MARK: - Simulation Studie 2

if (options$run_simulation_study_2) {
    ability_differences <- c(-0.5, 0.5)
    should_simulate_dif <- c(FALSE, TRUE)

    conditions <- expand.grid(
        ## a true dif value simulates DIF
        dif = should_simulate_dif,
        ## a value to generate a difference in ability between focal and reference
        ability = ability_differences
    )

    if (should_log) {
        tictoc::tic("Total")
        print("Starting simulation study II")
    }
    simulation_2_results <- vector(mode = "list")
    for (current_condition in seq_len(nrow(conditions))) {
        condition_results <- vector(mode = "list")

        if (should_log) {
            log_current_condition(
                conditions[current_condition, ]
            )
            tictoc::tic(paste("condition number", toString(current_condition)))
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
        if (should_log) {
            log_condition_results(condition_results)
            tictoc::toc()
        }
        simulation_2_results <- append_condition_results(
            condition_results = condition_results,
            simulation_results = simulation_2_results,
            condition_index = current_condition,
            conditions = conditions,
            with_ari_and_rmse = FALSE,
            simulation_count = simulation_count
        )
    }
    if (should_log) tictoc::toc() ## 1484.347 sec elapsed
    simulation_2_results <- set_row_names(simulation_2_results, conditions)
} else {
    simulation_2_results <- NA
}

simulation_results <- list(
    study_1 = simulation_1_results,
    study_2 = simulation_2_results
)

save(
    simulation_results,
    file = file.path(options$output_file_path, "Simulation_Results.RData")
)
parallel::stopCluster(cluster)