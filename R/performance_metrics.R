#' Performance Metrics
#'
#' Takes a combination of values typically found in a confusion matrix and calculates the predictive performance metrics described on [this Wikipedia page](https://en.wikipedia.org/wiki/Confusion_matrix#Table_of_confusion).
#'
#' The function is meant as a quick way to get multiple performance metrics while experimenting with models. Nomenclature of return values is therefore informal with several redundant names for certain values, letting the the user choose their preferred terminology.
#'
#' Arguments are returned together with calculated values. Missing arguments are calculated, as far as sufficient data is provided. The function throws an error if insufficient data or contradictory data is provided.
#'
#'
#' @param total_population The number of people in the entire population.
#' @param real_positives The number of people with the condition in the population.
#' @param real_negatives The number of people without the condition in the population.
#' @param true_positives The number of people with the condition with a positive test result.
#' @param false_positives The number of people without the condition with a positive test result.
#' @param true_negatives The number of people without the condition with a negative test result.
#' @param false_negatives The number of people without the condition with a positive test result.
#'
#' @returns A list of arguments and calculated performance metrics (see table).
#' @export
#'
#' @md
#'
#' @examples
#' # The function can be called without all arguments, as long as sufficient data is provided.
#' performance_metrics(
#'   total_population = 2030,
#'   real_positives = 30,
#'   real_negatives = 2000,
#'   true_positives = 20,
#'   false_negatives = 10,
#'   false_positives = 180,
#'   true_negatives = 1820
#' )
#'
#' performance_metrics(
#'   total_population = 2030,
#'   real_positives = 30,
#'   real_negatives = 2000,
#'   true_positives = 20,
#'   false_negatives = 10,
#'   false_positives = 180
#' )
#'
#' performance_metrics(
#'   total_population = 2030,
#'   real_positives = 30,
#'   real_negatives = 2000,
#'   true_positives = 20,
#'   false_positives = 180
#' )
#'
#' performance_metrics(
#'   real_positives = 30,
#'   real_negatives = 2000,
#'   true_positives = 20,
#'   false_positives = 180
#' )
performance_metrics <- function(
    total_population = integer(),
    real_positives = integer(),
    real_negatives = integer(),
    true_positives = integer(),
    false_positives = integer(),
    true_negatives = integer(),
    false_negatives = integer()
) {

  # Make a list of all argument names.
  all_arguments <-
    performance_metrics |>
    args() |>
    as.list() |>
    unlist() |>
    names()

  # Make a list of non-empty arguments.
  non_empty_arguments <-
    # Get list of all arguments that have a value.
    match.call() |>
    as.list() |>
    (\(x) x[2:length(x)])()

  # Make list of names of non-missing arguments that are mathematical integers.
  correct_arguments <-
    non_empty_arguments |>
    # Check whether argument contains a value (has length), is an integer (no
    # remainder after division by one), and that a non-numeric value is has not
    # been specificed (tryCatch).
    purrr::map(\(x) tryCatch(length(x) != 0 & isTRUE(x %% 1 == 0), error = function(e) FALSE)) |>
    # Filter out arguments matching the criteria above.
    (\(x) x[x == TRUE])() |>
    # Get the names of the arguments in question.
    names()

  # Transfer initially known values of arguments to a list of known values.
  known_values <-
    NA |>
    rep(length(all_arguments)) |>
    as.list()

  names(known_values) <- all_arguments

  for (variable_name in correct_arguments) {
    known_values[variable_name] <- non_empty_arguments[variable_name]
  }

  # Set up Boolean to determine whether a new value was calculated when going
  # through the following loop.
  new_calculated_value <- TRUE

  while (new_calculated_value) {
    # Assume no new value is calculated through this run of the loop.
    new_calculated_value <- FALSE

    # Equations to calculate missing values in confusion matrix.
    equations <- list(
      real_positives = known_values$true_positives + known_values$false_negatives,
      real_negatives = known_values$true_negatives + known_values$false_positives,
      total_population = known_values$true_positives + known_values$false_positives + known_values$true_negatives + known_values$false_negatives,
      true_positives = ifelse(!is.na(known_values$real_positives) && !is.na(known_values$false_negatives), known_values$real_positives - known_values$false_negatives, known_values$true_positives),
      false_positives = ifelse(!is.na(known_values$real_negatives) && !is.na(known_values$true_negatives), known_values$real_negatives - known_values$true_negatives, known_values$false_positives),
      true_negatives = ifelse(!is.na(known_values$real_negatives) && !is.na(known_values$false_positives), known_values$real_negatives - known_values$false_positives, known_values$true_negatives),
      false_negatives = ifelse(!is.na(known_values$real_positives) && !is.na(known_values$true_positives), known_values$real_positives - known_values$true_positives, known_values$false_negatives)
    )

    # Add newly calculated values to list of known values.
    for (variable_name in all_arguments) {
      if (is.na(known_values[[variable_name]]) && !is.na(equations[[variable_name]])) {
        known_values[[variable_name]] <- equations[[variable_name]]
        new_calculated_value <- TRUE

        # Check for consistency among calculated values and user entered values.
      } else if (!is.na(known_values[[variable_name]]) && !is.na(equations[[variable_name]]) && known_values[[variable_name]] != equations[[variable_name]]) {
        paste0('Inconsistent data in argument ', variable_name, ' or in calculation thereof.') |>
          stop()
      }
    }
  }

  # Check that enough data was provided to calculate entire confusion matrix.
  if (known_values |> is.na() |> any()) {
    stop('Insufficient data to calculate confusion matrix.')
  }

  # Check that the calculated total population is reasonable.
  if (known_values$total_population != known_values$true_positives + known_values$false_positives + known_values$true_negatives + known_values$false_negatives) {
    stop('Incorrect data entry.')
  }

  # Calculate values derived from confusion matrix.
  known_values[['test_outcome_positive']] <- known_values[['predicted_positive']] <- known_values$true_positives + known_values$false_positives
  known_values[['test_outcome_negative']] <- known_values[['predicted_negative']] <- known_values$true_negatives + known_values$false_negatives
  known_values[['accuracy']] <- (known_values$true_positives + known_values$true_negatives) / known_values$total_population
  known_values[['true_positive_rate']] <- known_values[['recall']] <- known_values[['sensitivity']] <- known_values[['probability_of_detection']] <- known_values[['hit_rate']] <- known_values[['power']] <- known_values$true_positives / known_values$real_positives
  known_values[['false_negative_rate']] <- known_values[['miss_rate']] <- known_values[['type_II_errors']] <- known_values$false_negatives / known_values$real_positives
  known_values[['false_positive_rate']] <- known_values[['fall_out']] <- known_values[['probability_of_false_alarm']] <- known_values[['type_I_errors']] <- known_values$false_positives / known_values$real_negatives
  known_values[['specificity']] <- known_values[['selectivity']] <- known_values[['true_negative_rate']] <- known_values$true_negatives / known_values$real_negatives
  known_values[['prevalence']] <- known_values$real_positives / known_values$total_population
  known_values[['positive_predictive_value']] <- known_values[['precision']] <- known_values$true_positives / (known_values$true_positives + known_values$false_positives)
  known_values[['f1_score']] <- 2 * ((known_values$precision * known_values$recall) / (known_values$precision + known_values$recall))
  known_values[['false_omission_rate']] <- known_values$false_negatives / (known_values$false_negatives + known_values$true_negatives)
  known_values[['positive_likelihood_ratio']] <- known_values$true_positive_rate / known_values$false_positive_rate
  known_values[['negative_likelihood_ratio']] <- known_values$false_negative_rate / known_values$true_negative_rate
  known_values[['false_discovery_rate']] <- known_values$false_positives / (known_values$true_positives + known_values$false_positives)
  known_values[['negative_predictive_value']] <- known_values$true_negatives / (known_values$false_negatives + known_values$true_negatives)
  known_values[['diagnostic_odds_ratio']] <- known_values$positive_likelihood_ratio / known_values$negative_likelihood_ratio
  known_values[['informedness']] <- known_values[['bookmaker_informedness']] <- known_values$true_positive_rate + known_values$true_negative_rate - 1
  known_values[['prevalence_threshold']] <- (sqrt(known_values$true_positive_rate * known_values$false_positive_rate) - known_values$false_positive_rate) / (known_values$true_positive_rate - known_values$false_positive_rate)
  known_values[['hits']] <- known_values$true_positives
  known_values[['misses']] <- known_values[['underestimations']] <- known_values$false_negatives
  known_values[['false_alarms']] <- known_values[['overestimations']] <- known_values$false_positives
  known_values[['correct_rejections']] <- known_values$true_negatives
  known_values[['markedness']] <- known_values[['delta_p']] <- known_values$positive_predictive_value + known_values$negative_predictive_value - 1
  known_values[['balanced_accuracy']] <- (known_values$true_positive_rate + known_values$true_negative_rate) / 2
  known_values[['fowlkes_mallows_index']] <- sqrt(known_values$positive_predictive_value * known_values$true_positive_rate)
  known_values[['phi']] <- known_values[['matthews_correlation_coefficient']] <- sqrt(known_values$true_positive_rate * known_values$true_negative_rate * known_values$positive_predictive_value * known_values$negative_predictive_value) - sqrt(known_values$false_negative_rate * known_values$false_positive_rate * known_values$false_omission_rate * known_values$false_discovery_rate)
  known_values[['threat_score']] <- known_values[['critical_success_index']] <- known_values[['jaccard_index']] <- known_values$true_positives / (known_values$true_positives + known_values$false_negatives + known_values$false_positives)

  return(known_values)
}
