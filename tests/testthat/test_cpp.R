load_all()


test_that("raw score possibilities", {
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
        1,
        engine = "R",
        possibility_engine = "C"
    )

    esf_c <- rpcm_esf(
        items_raw_scores[1],
        apply(y, 2, mean),
        item_time_limits,
        1,
        engine = "C"
    )
    expect_equal(esf_r[[1]], esf_c[[1]])
    expect_equal(esf_r[[2]], esf_c[[2]])
})