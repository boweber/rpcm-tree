glmer_fit <- function(y,
                      x = NULL,
                      start = NULL,
                      weights = NULL,
                      offset = NULL,
                      ...,
                      estfun = FALSE,
                      object = FALSE) {
    if (!(is.null(x) || NCOL(x) == 0L)) warning("x not used")
    if (!is.null(offset)) warning("offset not used")

    y_data_frame <- as.data.frame(y)
    y_data_frame <- tidyr::gather(
        dplyr::mutate(
            y_data_frame,
            id = seq_len(nrow(y))
        ),
        -id,
        key = "item",
        value = "count"
    )

    glmer_result <- lme4::glmer(
        count ~ 0 + item + (1 | id),
        data = y_data_frame,
        offset = if (is.null(offset)) {
            NULL
        } else {
            log(offset)
        },
        family = "poisson",
        control = lme4::glmerControl(
            optimizer = "bobyqa",
            optCtrl = list(maxfun = 2e5)
        )
    )
    ## estfun: empirical estimating function (score/gradient contributions)
    contributions <- NULL
    if (estfun) {
        contributions <- merDeriv::estfun.glmerMod(glmer_result)
        ## drops id column
        contributions <- contributions[, -ncol(contributions)]
    }

    rval <- list(
        coefficients = lme4::fixef(glmer_result),
        objfun = -as.numeric(summary(glmer_result)[[6]]),
        estfun = contributions,
        object = if (object) glmer_result else NULL
    )
    return(rval)
}

rpcm_tree <- function(formula,
                      data,
                      reltol = 1e-10,
                      maxit = 100L,
                      fitting_func = glmer_fit,
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