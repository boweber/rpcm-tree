rpcm <- function(y,
                 item_time_limits = NULL,
                 start = NULL,
                 reltol = 1e-10,
                 maxit = 100L,
                 hessian = TRUE,
                 gradtol = reltol,
                 iterlim = maxit,
                 ...) {
    y <- data.matrix(y)
    number_of_items <- ncol(y)

    ### verifies that every entry is a positive integer
    if (any(!sapply(
        y,
        function(value) is.numeric(value) && (value %% 1 == 0) && (value >= 0)
    ))) {
        stop("The rasch poisson count model does not support non positive integer values.")
    }

    ### sets default values for missing values
    if (missing(reltol) && !missing(gradtol) && !is.null(gradtol)) {
        reltol <- gradtol
    }
    if (missing(maxit) && !missing(iterlim) && !is.null(iterlim)) {
        maxit <- iterlim
    }

    ### sets default item timelimits
    if (is.null(item_time_limits)) {
        item_time_limits <- rep.int(1, number_of_items)
    } else {
        if (length(item_time_limits) != number_of_items) {
            stop("The provided item time limits are not compatible with the items in y.")
        }
        if (any(number_of_items <= 0)) {
            stop("The provided item time limits must be higher that 0.")
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

    time_collection <- time_limit_collection$new(item_time_limits)
    ## item_time_limits <- log(item_time_limits)
    score_collection <- raw_score_collection$new(rowSums(y), number_of_items)

    opt <- optim(
        par = apply(y, 2, mean),
        fn = rpcm_log_likelihood,
        gr = rpcm_analytical_gradient,
        col_sums = colSums(y),
        row_sums = score_collection,
        item_time_limits = time_collection,
        engine = "C",
        factorial_like_component = sum(lfactorial(y)),
        method = "BFGS",
        hessian = hessian,
        control = list(
            reltol = reltol,
            maxit = maxit,
            ...
        )
    )
    best_parameters <- opt$par
    names(best_parameters) <- colnames(y)

    hessian_estimation <- opt$hessian
    if (hessian) {
        ## hessian_estimation <- chol2inv(chol(hessian_estimation))
    }
    rownames(hessian_estimation) <-
        colnames(hessian_estimation) <-
        names(best_parameters)

    rval <- list(
        coefficients = best_parameters,
        vcov = hessian_estimation,
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