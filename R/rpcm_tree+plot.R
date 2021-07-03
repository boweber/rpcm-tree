itempar.rpcmtree <- function(object, node = NULL, ...) {
    browser()
    ids <- if (is.null(node)) {
        partykit::nodeids(object, terminal = TRUE)
    } else {
        node
    }
    myitempar <- function(obj) coef(itempar(obj, ...))
    if (length(ids) == 1L) {
        psychotree::apply_to_models(
            object,
            node = ids,
            FUN = myitempar,
            drop = TRUE
        )
    } else {
        do.call(
            "rbind",
            psychotree::apply_to_models(
                object,
                node = ids,
                FUN = myitempar,
                drop = FALSE
            )
        )
    }
}

plot.rpcmtree <- function(x,
                          type = c("profile", "regions"),
                          terminal_panel = NULL,
                          tp_args = list(...),
                          tnex = 2L,
                          drop_terminal = TRUE,
                          ...) {
    if (!is.null(terminal_panel)) {
        if (!missing(type)) {
            warning("only one of 'type' and 'terminal_panel' should be specified")
        }
    } else {
        terminal_panel <- switch(match.arg(type),
            "regions" = psychotree::node_regionplot,
            "profile" = psychotree::node_profileplot
        )
    }
    partykit::plot.modelparty(x,
        terminal_panel = terminal_panel,
        tp_args = tp_args, tnex = tnex, drop_terminal = drop_terminal, ...
    )
}