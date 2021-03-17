load_all()

test_that("rpcm_tree - glmerfit", {
    source("tests+Extensions.R")

    true_deltas <- seq(from = 2, to = 4, by = 1)
    number_of_people <- 4
    testdata <- gen_test_data(true_deltas, number_of_people)

    tree <- rpcm_tree(
        data = testdata,
        fitting_func = glmer_fit
    )
    print(tree)
})