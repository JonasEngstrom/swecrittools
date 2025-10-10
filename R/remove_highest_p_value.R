#' Remove Highest p-value
#'
#' Take a p-value table returned by `make_p_value_table()` and return a list of terms excluding the one with the highest value, excluding the intercept.
#'
#' @param p_value_table A p-value table returned by `make_p_value_table()`.
#' @param independent_variables_column_name List of independent variables.
#'
#' @returns A list of term names.
#' @export
#'
#' @seealso [make_p_value_table()]
#'
#' @md
#'
#' @examples
#' tibble::tibble(
#'   independent_variables = c('(Intercept)', 'x1', 'x2', 'x3', 'x4'),
#'   p_value = c(1.18e-5, 4.36e-33, 4.97e-14, 6.02e-23, 8.44e-1)
#' ) |>
#'   remove_highest_p_value()
remove_highest_p_value <- function(p_value_table, independent_variables_column_name = 'independent_variables') {
  p_value_table <-
    p_value_table |>

    # Remove (Intercept) term.
    dplyr::filter(!!rlang::sym(independent_variables_column_name) != '(Intercept)') |>

    # Remove term with highest p-value.
    dplyr::filter(dplyr::if_all(dplyr::last_col(), ~ . != max(., na.rm = TRUE))) |>

    # Extract term names.
    dplyr::pull(1) |>

    # Remove TRUE or categorical variable categories from end of names.
    stringr::str_remove('(\\.|\\^|TRUE).*')

  # Make a table of unique terms from the table generated above.
  unique_p_value_table <-
    p_value_table |>
    unique()

  # Check that the two tables have the same length, in order to avoid terms with several categories being confused by function calling remove_highest_p_value.
  stopifnot(length(p_value_table) == length(unique_p_value_table))

  # Return checked table.
  return(unique_p_value_table)
}
