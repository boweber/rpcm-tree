library("grDevices")
load("simulation/results/Simulation_2_Results.RData")
load("simulation/results/Simulation_2_Results_a.RData")
load("simulation/results/Simulation_2_Results_b.RData")

round_values <- function(x) round(x, digits = 3)
simulation_2_results[, 3:ncol(simulation_2_results)] <- apply(
    simulation_2_results[, 3:ncol(simulation_2_results)],
    2,
    function(x) round_values(x / 60)
)
simulation_2_results[, seq_len(2)] <- apply(
    simulation_2_results[, seq_len(2)],
    2,
    round_values
)

simulation_2_column_names <- c(
    "error_rate",
    "mean_time",
    "min_time",
    "max_time",
)

## Extracted from log-file
load("simulation/results/Simulation_2_Computation_Times.RData")

simulation_2_computation_times$computation_times <- round(log(
    simulation_2_computation_times$computation_times / 60
), 3)

ggplot2::ggplot(simulation_2_computation_times, ggplot2::aes(x = condition, y = computation_times, fill = method)) +
    ggplot2::geom_boxplot(alpha = 0.7, lwd = 0.1) +
    ggplot2::scale_y_continuous(
        name = "log computation times",
        # breaks = seq(0, 25, 1),
        # limits = c(0, 25)
    ) +
    ggplot2::scale_x_discrete(
        name = ggplot2::element_blank(),
        labels = c(
            expression(atop("without DIF", Delta ~ " = -0.5")),
            expression(atop("with DIF", Delta ~ " = -0.5")),
            expression(atop("without DIF", Delta ~ " = 0.5")),
            expression(atop("with DIF", Delta ~ " = 0.5"))
        )
    ) +
    ggplot2::theme(
        ## plot.title = ggplot2::element_text(size = 14, face = "bold"),
        text = ggplot2::element_text(size = 12),
        ## axis.title = ggplot2::element_text(face = "bold"),
        axis.text.x = ggplot2::element_text(size = 11),
        legend.position = "bottom"
    )

ggplot2::ggsave("simulation_study_2_computation_times.pdf")