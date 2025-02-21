#' Log utilities

#' Passthrough logging.
#'
#' @import logger glue
#' @noRd
log_info <- function(message, ...) {
    logger::log_info(message, ...)
}

#' Passthrough logging.
#'
#' @import logger glue
#' @noRd
log_debug <- function(...) {
    logger::log_debug(...)
}

#' Passthrough logging.
#'
#' @import logger glue
#' @noRd
log_error <- function(...) {
    logger::log_error(...)
}

#' Passthrough logging.
#'
#' @import logger glue
#' @noRd
log_warn <- function(...) {
    logger::log_warn(...)
}

#' util for checking ps.
check_is_phyloseq <- function(x, argName = NULL, allow_psExtra = TRUE) {
    stopif_ps_extra(x, argName = argName, Ncallers = 2)
    isPhyloseq <- is(x, "phyloseq") && (allow_psExtra || !is(x, "psExtra"))

    if (!isPhyloseq) {
        CLASSES <- if (allow_psExtra) '"phyloseq" or "psExtra"' else '"phyloseq"'

        rlang::abort(call = rlang::caller_env(), message = c(
            paste("argument", argName, "must be a", CLASSES, "object"),
            i = paste0("argument is class: ", paste(class(x), collapse = " "))
        ))
    }
}

check_is_psExtra <- function(x, argName = NULL) {
    stopif_ps_extra(x, argName = argName, Ncallers = 2)
    if (!is(x, "psExtra")) {
        rlang::abort(call = rlang::caller_env(), message = c(
            paste("argument", argName, 'must be a "psExtra" object'),
            i = paste0("argument is class: ", paste(class(x), collapse = " "))
        ))
    }
}

stopif_ps_extra <- function(object, argName = NULL, Ncallers = 1) {
    if (!inherits(object, "ps_extra")) {
        return(object)
    }
    rlang::abort(call = rlang::caller_env(n = Ncallers), message = c(
        "x" = paste("argument", argName, "is a deprecated 'ps_extra' object"),
        "!" = "If possible, regenerate the object with the current microViz version",
        "i" = paste(
            "Or convert your ps_extra object to the new psExtra class",
            "with {.help [{.fun upgrade_ps_extra_to_psExtra}](microViz::upgrade_ps_extra_to_psExtra)}"
        )
    ))
}
