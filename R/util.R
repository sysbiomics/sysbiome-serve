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


# Mapping file


