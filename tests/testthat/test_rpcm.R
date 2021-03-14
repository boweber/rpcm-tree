library(tidyr)
library(dplyr)
load_all()

test_that("raw score possibilities", {
    raw_score <- 10
    number_of_items <- 4
    expect_true(
        all(
            rowSums(
                (raw_score_possibilities(raw_score, number_of_items))
            ) == raw_score
        )
    )
    raw_score <- 7
    number_of_items <- 6

    expect_true(
        all(
            rowSums(
                (raw_score_possibilities(raw_score, number_of_items, "C"))
            ) == raw_score
        )
    )
})

test_that("rpcm esf", {
    skip("rpcm esf c version not yet functional")
    y <- matrix(c(
        3, 0, 1,
        2, 1, 0,
        1, 3, 3,
        0, 1, 1
    ), ncol = 3, byrow = TRUE)

    item_time_limits <- rep.int(1, ncol(y))
    persons_raw_scores <- rowSums(y)
    items_raw_scores <- colSums(y)

    esf_r <- rpcm_esf(
        items_raw_scores[1],
        apply(y, 2, mean),
        item_time_limits,
        0,
        1
    )

    esf_c <- rpcm_esf_c(
        items_raw_scores[1],
        apply(y, 2, mean),
        item_time_limits,
        0,
        1
    )
    expect_equal(esf_r[[1]], esf_c[[1]])
})

test_that("rpcm gradient", {
    item_time_limits <- log(c(1, 1, 1))
    row_sums <- c(11, 11, 12)
    col_sums <- c(8, 10, 16)
    parameters <- c(3, 5, 4)
    custom_gradient <- rpcm_analytical_gradient(
        difficulty = parameters,
        col_sums = col_sums,
        row_sums = row_sums,
        item_time_limits = item_time_limits,
        engine = "C"
    )
    numeric_gradient <- numDeriv::grad(
        func = rpcm_log_likelihood,
        x = parameters,
        col_sums = col_sums,
        row_sums = row_sums,
        item_time_limits = item_time_limits,
        engine = "C",
        factorial_like_component = 3 ## dummy (not relevant for the gradient)
    )
    expect_equal(custom_gradient, numeric_gradient)
})

test_that("rpcm test", {
    source("tests+Extensions.R")

    true_deltas <- seq(from = 2, to = 4, by = 1)
    number_of_people <- 4
    testdata <- gen_test_data(true_deltas, number_of_people)
    testdata_long <- testdata %>%
        mutate(id = 1:nrow(testdata)) %>%
        gather(-id, key = "item", value = "count")

    tictoc::tic("glmer")
    glmer_fit <- lme4::glmer(count ~ 0 + item + (1 | id), data = testdata_long, family = "poisson")
    glmer_result <- unname(exp(lme4::fixef(glmer_fit)))
    tictoc::toc()
    tictoc::tic("rpcm")
    rpcm_result <- unname(rpcm(testdata)$coefficients)
    tictoc::toc()

    expect_equal(
        glmer_result,
        rpcm_result,
        tolerance = 0.01
    )
})