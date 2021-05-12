load("simulation/results/Simulation_1_Results.RData")
load("simulation/results/Simulation_2_Results.RData")

round_values <- function(x) round(x, digits = 3)
simulation_1_results[, 3:8] <- apply(
    simulation_1_results[, 3:8],
    2,
    function(x) round_values(x / 60)
)
not_time_related_indices <- setdiff(seq_len(ncol(simulation_1_results)), 3:8)
simulation_1_results[, not_time_related_indices] <- apply(
    simulation_1_results[, not_time_related_indices],
    2,
    round_values
)

simulation_1_column_names <- c(
    "error_rate",
    "mean_time",
    "min_time",
    "max_time",
    "rmse",
    "mean_ari"
)

rpcm_tree_results <- simulation_1_results[, c(1, 3, 5, 7, 9, 11)]
colnames(rpcm_tree_results) <- simulation_1_column_names
rpcm_tree_results

lr_test_results <- simulation_1_results[, c(2, 4, 6, 8, 10, 12)]
colnames(lr_test_results) <- simulation_1_column_names
lr_test_results