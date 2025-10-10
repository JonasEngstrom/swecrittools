#' Perform Backward Elimination of Terms in a Linear Regression Model
#'
#' Takes a tibble of independent and dependent variables, makes a linear model, eliminates the term with the highest p-value, makes a new linear model and so on, until the term with the highest p-value is under a p-value limit. Outputs a tibble documenting the runs, p-values, and remaining terms.
#'
#' @param tibble_to_analyze The tibble to analyze.
#' @param dependent_variable The name of the column containing the dependent variable.
#' @param independent_variables A list of names of columns containing the independent variables.
#' @param p_value_limit The p-value limit.
#' @param p_value_column_prefix The prefix for the p-value columns in the output. Suffixed by run number.
#'
#' @returns A tibble of runs, p-values, and remaining terms.
#' @export
#'
#' @examples
#' tibble::tibble(
#'   x1 = rnorm(50, 5, 2),
#'   x2 = rnorm(50, 3, 1.5),
#'   x3 = rnorm(50, 10, 5),
#'   x4 = rnorm(50, 0, 1),
#'   y = 3 + 2 * x1 - 1 * x2 + 0.5 * x3 + rnorm(50, 0, 1)
#' ) |>
#'   make_backwards_elimination_table('y', c('x1', 'x2', 'x3', 'x4'))
make_backwards_elimination_table <- function(tibble_to_analyze, dependent_variable, independent_variables, p_value_limit = 0.05, p_value_column_prefix = 'p_values_run_') {

  # Initialize run counter.
  run <- 1

  aggregate_p_value_table <-
    tibble_to_analyze |>

    # Make a table of p-values per term.
    make_p_value_table(
      dependent_variable,
      independent_variables,
      paste(p_value_column_prefix, run, sep = '')
    )

  # Keep removing variables and making reduced models until highest p-value is below threshold value.
  while (aggregate_p_value_table |> dplyr::select(dplyr::last_col()) |> max(na.rm = TRUE) > p_value_limit) {

    # Remove variable with the highest p-value.
    new_independent_variables <-
      aggregate_p_value_table |>
      remove_highest_p_value(paste(p_value_column_prefix, run, sep = ''))

    # Increase run counter.
    run <- run + 1

    # Make another table for p-values of remaining variables.
    new_p_value_table <-
      tibble_to_analyze |>
      make_p_value_table(
        dependent_variable,
        new_independent_variables,
        paste(p_value_column_prefix, run, sep = '')
      )

    # Combine p-value tables.
    aggregate_p_value_table <-
      aggregate_p_value_table |>
      dplyr::left_join(
        new_p_value_table,
        by = dplyr::join_by(independent_variables)
      )
  }

  # Sort table, so that removed variables are at the bottom of each column.
  for (i in names(aggregate_p_value_table)[-1]) {
    aggregate_p_value_table <-
      aggregate_p_value_table |>
      dplyr::arrange(!!rlang::sym(i))
  }

  # Move (Intercept) term to top before export.
  aggregate_p_value_table <-
    aggregate_p_value_table |>
    dplyr::arrange(dplyr::desc(independent_variables == '(Intercept)'))

  # Return resulting table.
  return(aggregate_p_value_table)
}
