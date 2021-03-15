
## Almost identical compared to the functions raschfit() and rsmfit()
## provided by the psychotree package.

rpcm_fit <- function(y,
                     x = NULL,
                     start = NULL,
                     weights = NULL,
                     offset = NULL,
                     ...,
                     estfun = FALSE,
                     object = FALSE) {
    if (!(is.null(x) || NCOL(x) == 0L)) warning("x not used")
    if (!is.null(offset)) warning("offset not used")

    rval <- rpcm(
        y,
        ...,
        hessian = object | estfun
    )

    ## FIXME: estfun.rpcm is not yet implemented

    rval <- list(
        coefficients = rval$coefficients,
        objfun = -rval$loglik,
        estfun = if (estfun) estfun.rpcm(rval) else NULL,
        object = if (object) rval else NULL
    )
    return(rval)
}

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
    y_data_frame <- y_data_frame %>%
        mutate(id = seq_len(nrow(y))) %>%
        gather(-id, key = "item", value = "count")

    glmer_result <- lme4::glmer(
        count ~ 0 + item + (1 | id),
        data = y_data_frame,
        family = "poisson"
    )

    rval <- list(
        coefficients = exp(lme4::fixef(glmer_result)),
        objfun = summary(glmer_result)[[6]],
        estfun = if (estfun) glmer_result@optinfo$derivs$gradient else NULL,
        object = if (object) glmer_result else NULL
    )
    return(rval)
}