library(tidyr)
library(lme4)
library(dplyr)

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

    glmer_result <- glmer(
        count ~ 0 + item + (1 | id),
        data = y_data_frame,
        family = "poisson"
    )

    rval <- list(
        coefficients = exp(fixef(glmer_result)),
        objfun = summary(glmer_result)[[6]],
        estfun = if (estfun) glmer_result@optinfo$derivs$gradient else NULL,
        object = if (object) glmer_result else NULL
    )
    return(rval)
}