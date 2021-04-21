
#' A wrapper function of the glmer function.
#'
#' @param y A matrix containing response values of
#' each participant for each item.
#' @export
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
        offset = ifelse(is.null(offset), NULL, log(offset)),
        family = "poisson",
        control = lme4::glmerControl(
            optimizer = "bobyqa",
            optCtrl = list(maxfun = 2e5)
        )
    )

    ## estfun: empirical estimating function (score/gradient contributions)
    if (estfun) {
        contributions <- merDeriv::estfun.glmerMod(glmer_result)
        ## drops id column
        contributions <- contributions[, -ncol(contributions)]
    } else {
        contributions <- NULL
    }

    rval <- list(
        coefficients = lme4::fixef(glmer_result),
        objfun = -as.numeric(summary(glmer_result)[[6]]),
        estfun = contributions,
        object = ifelse(object, glmer_result, NULL)
    )
    return(rval)
}