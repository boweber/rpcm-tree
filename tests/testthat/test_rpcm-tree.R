library(tidyr)
library(dplyr)
load_all()

test_that("RawScore - initilizer", {
    row_sum <- 6
    number_of_items <- 4

    expect_error(
        raw_score$new(row_sum, number_of_items),
        NA
    )

    row_sum <- NA
    expect_error(raw_score$new(row_sum, number_of_items))

    row_sum <- ""
    expect_error(raw_score$new(row_sum, number_of_items))

    row_sum <- -1
    expect_error(raw_score$new(row_sum, number_of_items))

    row_sum <- 1.1
    expect_error(raw_score$new(row_sum, number_of_items))

    row_sum <- 6
    number_of_items <- NA
    expect_error(raw_score$new(row_sum, number_of_items))

    number_of_items <- ""
    expect_error(raw_score$new(row_sum, number_of_items))

    number_of_items <- -1
    expect_error(raw_score$new(row_sum, number_of_items))

    number_of_items <- 1.1
    expect_error(raw_score$new(row_sum, number_of_items))
})

test_that("RawScore - esf component", {
    test_row_sum <- 4
    number_of_items <- 3

    item_difficulties <- rep(0.8, number_of_items)
    item_time_limits <- rep(1, number_of_items)
    order <- 1

    test_raw_score <- raw_score$new(
        test_row_sum,
        number_of_items
    )

    raw_score_esf <- test_raw_score$esf(
        item_difficulties,
        item_time_limits,
        order
    )

    prev_rpcm_esf_component <- function() {
        possibilities <- poly_idx_cpp(
            test_row_sum,
            length(item_difficulties)
        )
        possibilities <- possibilities[
            apply(possibilities[, -1], 1, function(x) !all(x == 0)),
        ]
        esf <- vector(mode = "list", length = (order + 1))

        summation <- function(item_index) {
            sum(
                apply(
                    possibilities,
                    1,
                    function(y) {
                        exp(
                            sum(y * (item_difficulties + item_time_limits) - lfactorial(y))
                        ) * (
                            if (is.na(item_index)) 1 else y[item_index]
                        ) ## / item_difficulties[item_index]
                    }
                )
            )
        }
        esf[[1L]] <- summation(NA)
        if (order == 0) {
            return(esf)
        } else {
            esf[[2L]] <- sapply(
                seq_len(length(item_difficulties)),
                summation
            )
            return(esf)
        }
    }

    prev_result <- prev_rpcm_esf_component()

    expect_equal(raw_score_esf[[1]], prev_result[[1]])
    if (order == 1) {
        expect_equal(raw_score_esf[[2]], prev_result[[2]])
    }
})

test_that("RawScore - is_row_sum", {
    number_of_items <- 4
    row_sum <- 3
    score <- raw_score$new(row_sum, number_of_items)

    expect_true(score$is_row_sum(row_sum))
    expect_false(score$is_row_sum(5))
})

test_that("RawScoreCollection - initilizer", {
    row_sums <- 4:8
    number_of_items <- 3

    expect_warning(
        raw_score_collection$new(row_sums, number_of_items),
        NA
    )
})

test_that("poly_idx_cpp - raw score possibilities", {
    raw_score <- 7
    number_of_items <- 6

    possibilities <- poly_idx_cpp(
        raw_score,
        number_of_items
    )
    possibilities <- possibilities[
        apply(possibilities[, -1], 1, function(x) !all(x == 0)),
    ]

    expect_true(all(rowSums(possibilities) == raw_score))
})

test_that("rpcm_tree - glmerfit", {
    source("tests+Extensions.R")

    # DIF auf allen 3 Items
    testdata <- gen_test_data_1cov(c(2, 2, 2), c(2.3, 2.3, 2.3), 100)

    ## prepare data
    testdata$response <- as.matrix(testdata[, 2:4])
    testdata <- testdata[, -(2:4)]

    tree <- rpcm_tree(
        response ~ covariate,
        data = testdata,
        fitting_func = glmer_fit
    )

    print(tree)
})

test_that("rpcm_tree - rpcmfit", {
    ## skip("estfun.rpcm is not yet implemented")
    source("tests+Extensions.R")

    # DIF auf allen 3 Items
    testdata <- gen_test_data_1cov(c(2, 2, 2), c(2.3, 2.3, 2.3), 100)

    ## prepare data
    testdata$response <- as.matrix(testdata[, 2:4])
    testdata <- testdata[, -(2:4)]

    tree <- rpcm_tree(
        response ~ covariate,
        data = testdata,
        fitting_func = rpcm_fit
    )
    print(tree)
})

test_that("prev: analytical gradient", {
    source("tests+Extensions.R")
    item_time_limits <- log(c(1, 1, 1))
    row_sums <- c(11, 11, 12)
    col_sums <- c(8, 10, 16)
    parameters <- c(3, 5, 4)
    ## dummy (not relevant for the gradient)
    factorial_like_component <- 3

    custom_gradient <- prev_rpcm_analytical_gradient(
        difficulty = parameters,
        col_sums = col_sums,
        row_sums = row_sums,
        item_time_limits = item_time_limits,
        factorial_like_component = factorial_like_component
    )

    numeric_gradient <- numDeriv::grad(
        func = prev_rpcm_log_likelihood,
        x = parameters,
        col_sums = col_sums,
        row_sums = row_sums,
        item_time_limits = item_time_limits,
        factorial_like_component = factorial_like_component
    )
    expect_equal(custom_gradient, numeric_gradient)
})

test_that("likelihood", {
    source("tests+Extensions.R")
    item_time_limits <- log(c(1, 1, 1))
    row_sums <- c(11, 11, 12)
    col_sums <- c(8, 10, 16)
    parameters <- c(3, 5, 4)
    ## dummy (not relevant for the gradient)
    factorial_like_component <- 3

    prev_like <- prev_rpcm_log_likelihood(
        parameters,
        col_sums,
        row_sums,
        item_time_limits,
        factorial_like_component
    )

    raw_score_values <- raw_score_collection$new(
        row_sums,
        length(item_time_limits)
    )

    new_like <- rpcm_log_likelihood(
        parameters,
        col_sums,
        raw_score_values,
        item_time_limits,
        factorial_like_component
    )
    expect_equal(prev_like, new_like)
})

test_that("gradient", {
    source("tests+Extensions.R")
    item_time_limits <- log(c(1, 1, 1))
    row_sums <- c(11, 11, 12)
    col_sums <- c(8, 10, 16)
    parameters <- c(3, 5, 4)
    ## dummy (not relevant for the gradient)
    factorial_like_component <- 3

    prev_grad <- prev_rpcm_analytical_gradient(
        parameters,
        col_sums,
        row_sums,
        item_time_limits,
        factorial_like_component
    )

    raw_score_values <- raw_score_collection$new(
        row_sums,
        length(item_time_limits)
    )

    new_grad <- rpcm_analytical_gradient(
        parameters,
        col_sums,
        raw_score_values,
        item_time_limits,
        factorial_like_component
    )
    expect_equal(prev_grad, new_grad)
})

test_that("analytical gradient", {
    item_time_limits <- log(c(1, 1, 1))
    row_sums <- raw_score_collection$new(
        c(11, 11, 12),
        length(item_time_limits)
    )
    col_sums <- c(8, 10, 16)
    parameters <- c(3, 5, 4)
    ## dummy (not relevant for the gradient)
    factorial_like_component <- 3

    expect_warning(
        custom_gradient <- rpcm_analytical_gradient(
            difficulty = parameters,
            col_sums = col_sums,
            row_sums = row_sums,
            item_time_limits = item_time_limits,
            factorial_like_component = factorial_like_component
        ),
        regexp = NA
    )

    numeric_gradient <- numDeriv::grad(
        func = rpcm_log_likelihood,
        x = parameters,
        col_sums = col_sums,
        row_sums = row_sums,
        item_time_limits = item_time_limits,
        factorial_like_component = factorial_like_component
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
    glmer_fit <- lme4::glmer(
        count ~ 0 + item + (1 | id),
        data = testdata_long,
        family = "poisson"
    )
    glmer_result <- unname(exp(lme4::fixef(glmer_fit)))
    tictoc::toc()
    tictoc::tic("rpcm")
    rpcm_result <- unname(rpcm(testdata)$coefficients)
    tictoc::toc()

    expect_equal(
        glmer_result,
        rpcm_result,
        tolerance = 0.1
    )
})