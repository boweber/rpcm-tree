load("Simulation_Study_I.RData")
load("Simulation_Study_II.RData")

simulation_1_results

simulation_1_column_names <- c(
    "error_rate",
    "mean_time",
    "min_time",
    "max_time",
    "rmse",
    "mean_ari"
)
rpcm_tree <- simulation_1_results[, c(1, 3, 5, 7, 9, 11)]
lr_test <- simulation_1_results[, c(2, 4, 6, 8, 10, 12)]
colnames(rpcm_tree) <- simulation_1_column_names
colnames(lr_test) <- simulation_1_column_names


simulation_1_list <- list(rpcm_tree, lr_test)

table(rpcm_tree)