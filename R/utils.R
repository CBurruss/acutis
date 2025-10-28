#' @title Frankie's custom not operator
#' 
#' @description 
#' The `%not%` operator returns the logical negation of the `%in%` operator.
#' It provides a more readable way to check if elements are not in a set.
#' 
#' @param x Vector of values to be tested
#' @param table Vector of values to be tested against
#' 
#' @return Logical vector indicating whether each element of `x` is not in `table`
#' 
#' @examples 
#' # Basic usage
#' c("apple", "banana", "cherry") %not% c("banana", "date")
#' 
#' # With dplyr
#' df <- data.frame(col = c("apple", "pear", "orange", "banana", "grape"))
#' df |> dplyr::filter(col %not% c("apple", "banana", "orange"))
#' 
#' @export
`%not%` <- Negate(`%in%`)

#' @title Not like 
#' 
#' @description 
#' Searches for inverse of matches to a pattern within each element 
#' of a character vector.
#'
#' @param pattern Character pattern to inverse match.
#' @param col Column to test (unquoted, NSE compatible)
#' @param ignore.case Logical; passed to !grepl()
#'
#' @return Logical vector
#' @examples 
#' df <- data.frame(col = c("apple", "pear", "orange", "banana", "grape"))
#' df |> dplyr::filter(not_like("an", col))
#'
#' @export
not_like <- function(pattern, col, ignore.case = FALSE) {
  # Capture the quosure
  col_quo <- rlang::enquo(col)
  
  # Get the data from the calling environment
  data <- rlang::eval_tidy(col_quo, env = rlang::caller_env())
  
  # If we got a function instead of data, we're likely in a special environment
  # Try one level up
  if (is.function(data)) {
    data <- rlang::eval_tidy(col_quo, env = rlang::caller_env(2))
  }
  
  !grepl(pattern, data, ignore.case = ignore.case)
}

#' @title Not NA
#' 
#' @description 
#' Returns a logical vector indicating which elements are not NA.
#' This is the complement of `is.na()`.
#' 
#' @param x Vector of values to test
#' @param ... Additional arguments (for consistency with other functions)
#' 
#' @return Logical vector of the same length as `x`
#' 
#' @examples
#' not.na(c(1, NA, 3, NA, 5))
#' 
#' @export
not.na <- function(x, ...) {
  !is.na(x)
}