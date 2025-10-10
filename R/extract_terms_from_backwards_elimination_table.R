#' Extract Terms from Backwards Elimination Table
#'
#' Returns the remaining terms from a table returned from the function `make_backwards_elimination_table()`.
#'
#' @param backwards_elimination_table Tibble returned from `make_backwards_elimination_table()`.
#' @param independent_variables_column_name The name of the column with the terms. Defaults to 'independent_variables'.
#'
#' @returns A list of names of terms.
#' @export
#'
#' @md
#'
#' @examples
#' tibble::tibble(
#'   independent_variables = c('(Intercept)', 'abc', 'def'),
#'   p_values = c(.5, .5, .5)
#' ) |>
#' extract_terms_from_backwards_elimination_table()
extract_terms_from_backwards_elimination_table <- function(backwards_elimination_table, independent_variables_column_name = 'independent_variables') {
  backwards_elimination_table |>

    # Remove (Intercept) column.
    dplyr::filter(!!rlang::sym(independent_variables_column_name) != '(Intercept)') |>

    # Drop terms that have been eliminated through backward elimination.
    tidyr::drop_na() |>

    # Get term names.
    dplyr::pull(1) |>

    # Remove TRUE or categorical variable categories from end of names.
    stringr::str_remove('(\\.|\\^|TRUE).*') |>

    # Remove duplicates in case of several categories being extracted from the same variable.
    unique() |>

    # Return resulting list.
    (\(x) return(x))()
}
