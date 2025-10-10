#' Make p-value Table
#'
#' Make a table of p-value for terms of a generalized linear model.
#'
#' @param tibble_to_analyze Tibble containing dependent and independent variables.
#' @param dependent_variable Dependent variable in `tibble_to_analyze`.
#' @param independent_variables List of independent variables.
#' @param p_value_column_name Name of the output p-value column.
#' @param initial_p_value_column_name Name of column of p-values output when running `summary()` on a generalized linear model.
#' @param term_column_name Name of column of terms output when running `summary()` on a generalized linear model.
#'
#' @returns A tibble of terms and p-values.
#' @export
#'
#' @importFrom data.table :=
#'
#' @md
#'
#' @examples
#' tibble::tibble(a = c(1, 2, 3), b = c(1, 2, 3), c = c(1, 2, 3)) |>
#'   make_p_value_table('c', c('a', 'b'))
make_p_value_table <- function(tibble_to_analyze, dependent_variable, independent_variables, p_value_column_name = 'p_value', initial_p_value_column_name = 'Pr(>|t|)', term_column_name = 'term') {
  # Check that list of independent variables contains at least one variable.
  stopifnot(length(independent_variables) >= 1)

  tibble_to_analyze |>

    # Fit a generalized linear model with list of independent variables predicting dependent variable.
    (\(x) stats::glm(stats::formula(paste(dependent_variable, ' ~ ', paste(independent_variables[independent_variables != '(Intercept)'], collapse = ' + '))), data = x))() |>
    summary() |>

    # Extract coefficients and terms.
    stats::coefficients() |>
    tibble::as_tibble(rownames = paste(term_column_name)) |>

    # Extract p-values per term.
    dplyr::select(
      independent_variables = paste(term_column_name),
      !!paste(p_value_column_name) := paste(initial_p_value_column_name)
    ) |>

    # Return table of terms and p-values.
    (\(x) return(x))()
}
