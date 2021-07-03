library("partykit")

## https://rdrr.io/cran/carData/man/Baumann.html
data("Baumann", package = "carData")

baumann_data <- data.frame(group = Baumann$group)
baumann_data$observations <- as.matrix(
    cbind(
        Baumann$pretest.1,
        Baumann$pretest.2,
        Baumann$post.test.1,
        Baumann$post.test.2,
        Baumann$post.test.3
    )
)

tree <- rpcmtree::rpcmtree(
    observations ~ group,
    data = baumann_data,
    fitting_func = rpcmtree::glmer_fit,
    alpha = 0.05
)

plot(tree)