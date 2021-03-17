load_all()

test_that("TimeLimitCollection - summed_item_parameters", {
    time_limits <- 1:24
    difficulties <- 25:48

    collection <- time_limit_collection$new(time_limits)

    expect_equal(
        collection$summed_item_parameters(difficulties),
        log(time_limits) + difficulties
    )

    difficulties <- 1:3
    expect_error(
        collection$summed_item_parameters(difficulties)
    )
})

test_that("TimeLimitCollection - initilizer", {
    time_limits <- c("", 1, 2, 3)
    expect_error(
        time_limit_collection$new(time_limits)
    )

    time_limits <- c(2.4, 3)
    expect_error(
        time_limit_collection$new(time_limits)
    )

    time_limits <- -1:3
    expect_error(
        time_limit_collection$new(time_limits)
    )

    time_limits <- 1:3

    expect_error(
        time_limit_collection$new(time_limits),
        NA
    )
})

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

test_that("is_R6Class", {
    time_limits <- 1:24
    difficulties <- 25:48

    collection <- time_limit_collection$new(time_limits)
    expect_true(
        is_R6Class(collection, "TimeLimitCollection")
    )

    row_sum <- 6
    number_of_items <- 4

    score <- raw_score$new(row_sum, number_of_items)

    expect_false(
        is_R6Class(score, "TimeLimitCollection")
    )
})

test_that("RawScore - esf", {
    time_limits <- 1:2
    difficulties <- 3:4
    number_of_items <- length(difficulties)
    row_sum <- 3
    order <- 1

    collection <- time_limit_collection$new(time_limits)
    score <- raw_score$new(row_sum, number_of_items)

    expect_warning(
        score$esf(difficulties, collection, 1),
        NA
    )

    expect_error(
        score$esf(difficulties, time_limits, 1)
    )

    rpcm_esf_result <- rpcm_esf(
        row_sum,
        difficulties,
        log(time_limits),
        order,
        engine = "R",
        possibility_engine = "C"
    )

    r6_esf <- score$esf(difficulties, collection, order)

    expect_equal(r6_esf[[1]], rpcm_esf_result[[1]])
    expect_equal(r6_esf[[2]], rpcm_esf_result[[2]])
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

test_that("RawScoreCollection - compute_likelihood_component", {
    row_sums <- 4:8
    item_difficulties <- 4:7
    number_of_items <- length(item_difficulties)
    time_limits <- rep.int(1, number_of_items)

    time_limit_collection <- time_limit_collection$new(
        time_limits
    )


    collection <- raw_score_collection$new(row_sums, number_of_items)
    collection_like_comp <- collection$compute_likelihood_component(
        item_difficulties,
        time_limit_collection
    )

    like_comp <- sum(
        sapply(row_sums, function(row_sum) {
            log(
                rpcm_esf(
                    row_sum,
                    item_difficulties,
                    log(time_limits),
                    0,
                    engine = "R",
                    possibility_engine = "C"
                )[[1]]
            )
        })
    )
    expect_equal(like_comp, collection_like_comp)
})

test_that("RawScoreCollection - compute_gradient_component", {
    row_sums <- 4:8
    item_difficulties <- 4:7
    number_of_items <- length(item_difficulties)
    time_limits <- rep.int(1, number_of_items)

    time_limit_collection <- time_limit_collection$new(
        time_limits
    )

    collection <- raw_score_collection$new(row_sums, number_of_items)
    collection_grad_comp <- collection$compute_gradient_component(
        item_difficulties,
        time_limit_collection
    )

    esf_result <- rep(0, length(item_difficulties))
    for (raw_score_index in seq_len(length(row_sums))) {
        esf <- rpcm_esf(
            row_sums[raw_score_index],
            item_difficulties,
            time_limits,
            1,
            engine = "R",
            possibility_engine = "C"
        )

        esf_result <- esf_result + ((1 / esf[[1]]) * esf[[2]])
    }
    expect_equal(esf_result, collection_grad_comp)
})