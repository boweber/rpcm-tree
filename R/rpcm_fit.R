
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
        item_time_limits = offset,
        ...,
        hessian = object | estfun
    )

    rval <- list(
        coefficients = rval$coefficients,
        objfun = -rval$loglik,
        estfun = if (estfun) estfun.rpcm(rval) else NULL,
        object = if (object) rval else NULL
    )
    return(rval)
}

estfun.rpcm <- function(x, ...) {
    colSums(x$data) - x$elementary_symmetric_functions
}