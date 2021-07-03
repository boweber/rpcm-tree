load("simulation/results/Simulation_1_Results.RData")

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


## Simulation study I - computation times

## Extracted from log-file
load("simulation/results/Simulation_1_Computation_Times.RData")

simulation_1_computation_times$computation_times <- round(log(
    simulation_1_computation_times$computation_times / 60
), 3)

ggplot2::ggplot(simulation_1_computation_times, ggplot2::aes(x = condition, y = computation_times, fill = method)) +
    ggplot2::geom_boxplot(alpha = 0.7, lwd = 0.1) +
    ggplot2::scale_y_continuous(
        name = "log computation times",
        # breaks = seq(0, 25, 1),
        # limits = c(0, 25)
    ) +
    ggplot2::scale_x_discrete(
        name = ggplot2::element_blank(),
        labels = c(
            "without DIF \n numeric",
            "with DIF \n numeric (0.5)",
            "without DIF \n binary",
            "with DIF \n binary",
            "with DIF \n numeric (0.25)"
        )
    ) +
    ggplot2::theme(
        ## plot.title = ggplot2::element_text(size = 14, face = "bold"),
        text = ggplot2::element_text(size = 12),
        ## axis.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(size = 11),
        legend.position = "bottom"
    )
ggplot2::ggsave("/simulation_study_1_computation_times.pdf")

## Simulation study I - bias

## Extracted from log-file
load("simulation/results/Simulation_1_Estimate_Differences.RData")

bias <- with(
    simulation_1_estimate_differences,
    aggregate(
        estimate_differences,
        by = list(method, condition),
        FUN = mean
    )
)
bias$x <- round_values(bias$x)