
## Almost identical compared to the functions rstree() and raschtree()
## provided by the psychotree package.

#' Creates a rasch poisson count model tree.
#'
#' @param formula A symbolic description of the model to be fit.
#' @param data A data frame containing the variables in the model.
#' @param reltol Argument for optim.
#' @param maxit Argument for optim.
#' @param fitting_func A fitting function for the model.
#' By default the rpcm_fit function is used.
#' @param ... Arguments passed to the underlying functions.
#' @return A rasch poisson count model tree based on the input data.
rpcm_tree <- function(formula,
                      data,
                      reltol = 1e-10,
                      maxit = 100L,
                      fitting_func = rpcm_fit,
                      ...) {
    call <- match.call(expand.dots = TRUE)

    ## use dots for setting up mob_control
    control <- partykit::mob_control(...)
    control$ytype <- "matrix"

    raschcontrol <- list(
        reltol = reltol,
        maxit = maxit
    )
    ## call mob
    mob <- match.call(expand.dots = FALSE)
    mob$fit <- fitting_func
    mob$control <- control
    for (nameIndex in names(raschcontrol)) {
        if (!is.null(raschcontrol[[nameIndex]])) {
            mob[[nameIndex]] <- raschcontrol[[nameIndex]]
        }
    }
    if ("..." %in% names(mob)) mob[["..."]] <- NULL
    mob[[1L]] <- as.name("mob")
    rval <- eval(mob, parent.frame())

    ## extend class and keep original call
    rval$info$call <- call
    class(rval) <- c("rpcmtree", class(rval))
    return(rval)
}