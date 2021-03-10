
rpcm <- function(y,
                 item_time_limits = NULL,
                 start = NULL,
                 reltol = 1e-10,
                 maxit = 100L,
                 full = TRUE,
                 hessian = TRUE,
                 gradtol = reltol,
                 iterlim = maxit,
                 ...) {
    y <- data.matrix(y)

    if (any(!sapply(y, is_integer))) {
        stop("the rasch poisson count model does not support non integer values.")
    }

    ### sets default values for missing values
    if (missing(reltol) && !missing(gradtol) && !is.null(gradtol)) {
        reltol <- gradtol
    }
    if (missing(maxit) && !missing(iterlim) && !is.null(iterlim)) {
        maxit <- iterlim
    }

    number_of_items <- ncol(y)
    peoples_total_scores <- rowSums(y)
    items_total_scores <- colSums(y)

    if (any(peoples_total_scores <= 0)) {
        stop(paste(
            toString(peoples_total_scores),
            "is invalid as peoples_total_scores (func: rpcm)."
        ))
    }

    if (any(items_total_scores <= 0)) {
        stop(paste(
            toString(items_total_scores),
            "is invalid as items_total_scores (func: rpcm)."
        ))
    }

    ## sets default item timelimits
    if (is.null(item_time_limits)) {
        item_time_limits <- rep.int(1, number_of_items)
    } else {
        if (length(item_time_limits) != number_of_items) {
            stop("The provided item time limits are not compatible with the items in y.")
        }
    }

    ### sets missing item names
    if (is.null(colnames(y))) {
        colnames(y) <- paste(
            "Item ",
            gsub(" ", "0", format(1:number_of_items)),
            sep = ""
        )
    }

    ## initial values for optim
    initial_values <- apply(y, 2, mean)
    ## conditional log likelihood function, which should be maximized by optim
    cloglik <- make_cloglike(
        items_total_scores,
        peoples_total_scores,
        item_time_limits,
        y,
        engine = "C"
    )
    ## accompanying analytical gradient func
    # gradient <- make_gradient(
    #     peoples_total_scores,
    #     items_total_scores,
    #     item_time_limits,
    #     engine = "C"
    # )

    opt <- optim(
        par = initial_values,
        fn = cloglik,
        gr = NULL,
        method = "BFGS",
        # lower = (1 / .Machine$double.xmax),
        hessian = hessian,
        control = list(
            reltol = reltol,
            maxit = maxit,
            ...
        )
    )

    best_parameters <- opt$par
    # names(best_parameters) <- colnames(y)

    # hessian_estimation <- opt$hessian
    # hessian_estimation <- chol2inv(chol(hessian_estimation))
    # rownames(hessian_estimation) <-
    #     colnames(hessian_estimation) <-
    #    names(best_parameters)

    rval <- list(
        coefficients = best_parameters,
        # vcov = hessian_estimation,
        loglik = -opt$value,
        df = number_of_items - 1,
        data = y,
        elementary_symmetric_functions = NULL,
        code = opt$convergence,
        iterations = tail(na.omit(opt$counts), 1L),
        reltol = reltol
    )
    class(rval) <- "rasch_poisson_count_model"
    return(rval)
}