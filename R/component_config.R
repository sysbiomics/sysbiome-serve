
# Need to check for other
PLACE_HOLDER_CHOICE <- "Select an option"

#' Check if a Value is Available
#'
#' This function checks whether a given value is available (i.e., not empty, NULL, or a placeholder choice that isn't a real selection).
#'
#' @param x The value to check.
#'
#' @return A logical value: TRUE if x is available, otherwise FALSE.
#'
#' @examples
#' is_choice_valid(NULL) # FALSE
#' is_choice_valid("") # FALSE
#' is_choice_valid("Select an option") # FALSE
#' is_choice_valid("Valid choice") # TRUE
#'
is_choice_valid <- function(x) {

    placeholder_choice <- "Select an option" # This can be changed later
    if (is.null(x)) {
        return(FALSE)
    } else if (identical(x, "")) {
        return(FALSE)
    } else if (identical(x, placeholder_choice)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}
