load("simulation/results/Simulation_1_Results.RData")

draw_venn_diagram <- function(ari_value,
                              method_color = "#7FC6A4",
                              file_name = "test") {
    plot <- VennDiagram::draw.pairwise.venn(
        area1 = 1,
        area2 = 1,
        cross.area = ari_value,
        category = c("recovered groups", "true groups"),
        lty = rep("blank", 2),
        fill = c(method_color, "#2D2327"),
        alpha = rep(0.5, 2),
        cat.pos = c(-15, 15),
        cat.col = c(method_color, "#2D2327"),
        # cat.dist = rep(0.025, 2),
        cex = c(0, 1, 0),
    )
    pdf(paste(
        "/Users/boweber/Documents/Studium/Bachelorarbeit/Text/Bachelorarbeit/Graphics/",
        file_name,
        ".pdf",
        sep = ""
    ))
    grid::grid.draw(plot)
    dev.off()
}

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

rpcm_tree_color <- "#62466B"
draw_venn_diagram(ari_value = rpcm_tree_results$mean_ari[2], method_color = rpcm_tree_color, file_name = "rpcm_tree-median")
draw_venn_diagram(ari_value = rpcm_tree_results$mean_ari[5], method_color = rpcm_tree_color, file_name = "rpcm_tree-first_quartile")

lr_test_results <- simulation_1_results[, c(2, 4, 6, 8, 10, 12)]
colnames(lr_test_results) <- simulation_1_column_names
lr_test_results

lr_test_color <- "#8C93A8"
draw_venn_diagram(ari_value = lr_test_results$mean_ari[2], method_color = lr_test_color, file_name = "lr_test-median")
draw_venn_diagram(ari_value = lr_test_results$mean_ari[5], method_color = lr_test_color, file_name = "lr_test-first_quartile")

load("simulation/results/Simulation_2_Results.RData")

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