#' Get Averages of MICE Imputed Values
#'
#' Runs MICE imputation using specified predictors over set of tibble columns and returns descriptive statistics over the run.
#'
#' For example if the column weight is imputed, the function will return min_weight, mean_weight, median_weight, and max_weight calculated from all the imputations performed, so if MICE is set to run 200 times, the value returned will be an average of all those runs.
#'
#' @param data The tibble containing data to be imputed.
#' @param id_column The index column of the data tibble.
#' @param imputed_variables The variables to impute and the imputation method, as a named list---e.g. list('weight' = 'pmm', 'height' = 'pmm'). Methods are form MICE’s `mice()` function
#' @param predictors The predictors to use to impute variables.
#' @param ... Additional paramters passed to MICE’s `mice()` function, e.g. `m` and `seed`. See the documentation for `mice()`.
#'
#' @returns A tibble with and index column and descriptive statistics for each imputed column.
#' @export
#'
#' @seealso [mice::mice()]
#'
#' @md
#'
#' @examples
#' tibble::tibble(
#'   a=c(1,2,3),
#'   b=c(1,NA,3),
#'   c=c(1,2,3)
#' ) |>
#'   average_impute(
#'     'a',
#'     list('b' = 'pmm'),
#'     c('a', 'c')
#'    )
average_impute <- function(data, id_column, imputed_variables, predictors, ...) {
  # Extract variable names for method list.
  method_list <- mice::make.method(data)

  # Turn off imputation for variables except those specified in function call.
  method_list[] <- ''
  for (i in names(imputed_variables)) {
    method_list[i] <- imputed_variables[[i]]
  }

  predictor_matrix <- mice::make.predictorMatrix(data)

  # Turn off predictors not specified in the function call.
  predictor_matrix[,] <- 0
  for (i in rownames(predictor_matrix)) {
    predictor_matrix[i, predictors] <- 1
  }

  # Impute.
  imputed_values <-
    data |>
    mice::mice(
      method = method_list,
      predictorMatrix = predictor_matrix,
      ...
    )

  # Calculate min, mean, median, and max over imputations.
  missing_data <-
    imputed_values |>
    mice::complete(action = 'long') |>
    tibble::tibble() |>
    dplyr::select(
      dplyr::all_of(
        c(
          id_column,
          names(imputed_variables)
        )
      )
    )|>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_column)))

  for (i in names(imputed_variables)) {
    missing_data <-
      missing_data |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::all_of(names(imputed_variables)),
          .fns = list(
            min = ~ min(.x),
            mean = ~ mean(.x),
            median = ~ median(.x),
            max = ~ max(.x)
          ),
          .names = '{.col}_{.fn}'
        )
      )
  }

  missing_data <-
    missing_data |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::all_of(names(imputed_variables))) |>
    dplyr::distinct()

  return(missing_data)
}
