load_all()

test_that("occurrence possibilities", {
    raw_score <- 10
    number_of_items <- 4

    ## time for raw_score = 5, number_of_items = 9 -> 211.1 s
    expect_true(
        all(
            rowSums(
                (occurrence_possibilities(raw_score, number_of_items))
            ) == raw_score
        )
    )
    ## time -> 6.6 s (includes time to compute r version with raw score = 10 and number of items 4)

    skip("Skipping occurrence possibilities C test for performance reasons")
    raw_score <- 5
    number_of_items <- 9

    expect_true(
        all(
            rowSums(
                (occurrence_possibilities(raw_score, number_of_items, "C"))
            ) == raw_score
        )
    )
})

test_that("rpcm gradient", {
    y <- matrix(c(
        3, 0, 1,
        2, 1, 0,
        1, 3, 3,
        0, 1, 1
    ), ncol = 3, byrow = TRUE)

    item_time_limits <- rep.int(1, ncol(y))
    persons_raw_scores <- rowSums(y)
    items_raw_scores <- colSums(y)

    cloglike <- make_cloglike(
        items_raw_scores,
        persons_raw_scores,
        item_time_limits,
        y,
        "C"
    )

    gradient <- make_gradient(
        persons_raw_scores,
        items_raw_scores,
        item_time_limits,
        "C"
    )
    parameters <- apply(y, 2, mean)
    expect_equal(
        unname(gradient(parameters)),
        numDeriv::grad(cloglike, parameters)
    )
})

test_that("rpcm", {
    skip("rpcm currently not relevant")
    y <- matrix(c(
        3, 0, 1,
        2, 1, 0,
        1, 3, 3,
        0, 1, 1
    ), ncol = 3, byrow = TRUE)
    expect_warning(result <- rpcm(y), regexp = NA)
})

test_that("rpcm", {
    source("tests+Extensions.R")
    attention <- generate_glmer_data()
    attention$Item <- as.factor(attention$Item)

    fit2_item_estimates <- rpcm(
        transform_glmer_data(attention)
    )

    fit1 <- lme4::glmer(
        Hit ~ -1 + Item + (1 | ID),
        data = attention,
        family = poisson
    )

    expect_equal(
        unname(exp(lme4::fixef(fit1))),
        fit2_item_estimates$coefficients,
        tolerance = 0.00001
    )
})