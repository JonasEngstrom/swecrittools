test_object <-
  performance_metrics(
    total_population = 2030,
    real_positives = 30,
    real_negatives = 2000,
    true_positives = 20,
    false_negatives = 10,
    false_positives = 180,
    true_negatives = 1820
  )

desired_return_object <- list(
  real_positives = 30,
  real_negatives = 2000,
  true_positives = 20,
  false_positives = 180,
  true_negatives = 1820,
  false_negatives = 10
)

desired_return_object <-
  list(
    total_population = desired_return_object$real_positives + desired_return_object$real_negatives,
    predicted_positive = desired_return_object$true_positives + desired_return_object$false_positives,
    predicted_negative = desired_return_object$false_negatives + desired_return_object$true_negatives,
    accuracy = (desired_return_object$true_positives + desired_return_object$true_negatives) / (desired_return_object$real_positives + desired_return_object$real_negatives),
    true_positive_rate = desired_return_object$true_positives / desired_return_object$real_positives,
    false_negative_rate = desired_return_object$false_negatives / desired_return_object$real_positives,
    false_positive_rate = desired_return_object$false_positives / desired_return_object$real_negatives,
    true_negative_rate = desired_return_object$true_negatives / desired_return_object$real_negatives,
    prevalence = desired_return_object$real_positives / (desired_return_object$real_positives + desired_return_object$real_negatives),
    positive_predictive_value = desired_return_object$true_positives / (desired_return_object$true_positives + desired_return_object$false_positives),
    false_omission_rate = desired_return_object$false_negatives / (desired_return_object$true_negatives + desired_return_object$false_negatives),
    positive_likelihood_ratio = desired_return_object$true_positive_rate / desired_return_object$false_positive_rate,
    negative_likelihood_ratio = desired_return_object$false_negative_rate / desired_return_object$true_negative_rate,
    false_discovery_rate = desired_return_object$false_positives / (desired_return_object$true_positives + desired_return_object$false_positives),
    negative_predictive_value = desired_return_object$true_negatives / (desired_return_object$true_negatives + desired_return_object$false_negatives),
    hits = desired_return_object$true_positives,
    underestimations = desired_return_object$false_negatives,
    misses = desired_return_object$false_negatives,
    overestimations = desired_return_object$false_positives,
    false_alarms = desired_return_object$false_positives,
    correct_rejections = desired_return_object$true_negatives
  ) |>
  c(desired_return_object)

desired_return_object <-
  list(
    test_outcome_positive = desired_return_object$predicted_positive,
    test_outcome_negative = desired_return_object$predicted_negative,
    power = desired_return_object$true_positive_rate,
    hit_rate = desired_return_object$true_positive_rate,
    probability_of_detection = desired_return_object$true_positive_rate,
    sensitivity = desired_return_object$true_positive_rate,
    recall = desired_return_object$true_positive_rate,
    type_II_errors = desired_return_object$false_negative_rate,
    miss_rate = desired_return_object$false_negative_rate,
    type_I_errors = desired_return_object$false_positive_rate,
    probability_of_false_alarm = desired_return_object$false_positive_rate,
    fall_out = desired_return_object$false_positive_rate,
    selectivity = desired_return_object$true_negative_rate,
    specificity = desired_return_object$true_negative_rate,
    precision = desired_return_object$positive_predictive_value,
    f1_score = (2 * desired_return_object$positive_predictive_value * desired_return_object$true_positive_rate) / (desired_return_object$positive_predictive_value + desired_return_object$true_positive_rate),
    diagnostic_odds_ratio = desired_return_object$positive_likelihood_ratio / desired_return_object$negative_likelihood_ratio,
    informedness = desired_return_object$true_positive_rate + desired_return_object$true_negative_rate - 1,
    prevalence_threshold = (sqrt(desired_return_object$true_positive_rate * desired_return_object$false_positive_rate) - desired_return_object$false_positive_rate) / (desired_return_object$true_positive_rate - desired_return_object$false_positive_rate),
    markedness = desired_return_object$positive_predictive_value + desired_return_object$negative_predictive_value - 1,
    balanced_accuracy = (desired_return_object$true_positive_rate + desired_return_object$true_negative_rate) / 2,
    fowlkes_mallows_index = sqrt(desired_return_object$positive_predictive_value * desired_return_object$true_predictive_rate),
    phi = sqrt(desired_return_object$true_positive_rate * desired_return_object$true_negative_rate * desired_return_object$positive_predictive_value * desired_return_object$negative_predictive_value) - sqrt(desired_return_object$false_negative_rate * desired_return_object$false_positive_rate * desired_return_object$false_omission_rate * desired_return_object$false_discovery_rate),
    threat_score = desired_return_object$true_positives / (desired_return_object$true_positives + desired_return_object$false_negatives + desired_return_object$false_positives)
  ) |>
  c(desired_return_object)

desired_return_object <-
  list(
    bookmaker_informedness = desired_return_object$informedness,
    delta_p = desired_return_object$markedness,
    matthews_correlation_coefficient = desired_return_object$phi,
    critical_success_index = desired_return_object$threat_score,
    jaccard_index = desired_return_object$threat_score
  ) |>
  c(desired_return_object)

test_that('total_population is calculated correctly, given otherwise complete data', {
  performance_metrics(
    real_positives = 30,
    real_negatives = 2000,
    true_positives = 20,
    false_negatives = 10,
    false_positives = 180,
    true_negatives = 1820
  ) |>
    _[['total_population']] |>
    expect_equal(2030)
})

test_that('real_positives is calculated correctly, given otherwise complete data', {
  performance_metrics(
    total_population = 2030,
    real_negatives = 2000,
    true_positives = 20,
    false_negatives = 10,
    false_positives = 180,
    true_negatives = 1820
  ) |>
    _[['real_positives']] |>
    expect_equal(30)
})

test_that('real_negatives is calculated correctly, given otherwise complete data', {
  performance_metrics(
    total_population = 2030,
    real_positives = 30,
    true_positives = 20,
    false_negatives = 10,
    false_positives = 180,
    true_negatives = 1820
  ) |>
    _[['real_negatives']] |>
    expect_equal(2000)
})

test_that('true_positives is calculated correctly, given otherwise complete data', {
  performance_metrics(
    total_population = 2030,
    real_positives = 30,
    real_negatives = 2000,
    false_negatives = 10,
    false_positives = 180,
    true_negatives = 1820
  ) |>
    _[['true_positives']] |>
    expect_equal(20)
})

test_that('false_negatives is calculated correctly, given otherwise complete data', {
  performance_metrics(
    total_population = 2030,
    real_positives = 30,
    real_negatives = 2000,
    true_positives = 20,
    false_positives = 180,
    true_negatives = 1820
  ) |>
    _[['false_negatives']] |>
    expect_equal(10)
})

test_that('false_positives is calculated correctly, given otherwise complete data', {
  performance_metrics(
    total_population = 2030,
    real_positives = 30,
    real_negatives = 2000,
    true_positives = 20,
    false_negatives = 10,
    true_negatives = 1820
  ) |>
    _[['false_positives']] |>
    expect_equal(180)
})

test_that('true_negatives is calculated correctly, given otherwise complete data', {
  performance_metrics(
    total_population = 2030,
    real_positives = 30,
    real_negatives = 2000,
    true_positives = 20,
    false_negatives = 10,
    false_positives = 180
  ) |>
    _[['true_negatives']] |>
    expect_equal(1820)
})

test_that('performance_metrics throws an error when only total_population argument is given', {
  performance_metrics(total_population = 2) |>
    expect_error('Insufficient data to calculate confusion matrix.')
})

test_that('performance_metrics throws an error when only real_positives argument is given', {
  performance_metrics(real_positives = 2) |>
    expect_error('Insufficient data to calculate confusion matrix.')
})

test_that('performance_metrics throws an error when only real_negatives argument is given', {
  performance_metrics(real_negatives = 2) |>
    expect_error('Insufficient data to calculate confusion matrix.')
})

test_that('performance_metrics throws an error when only true_positives argument is given', {
  performance_metrics(true_positives = 2) |>
    expect_error('Insufficient data to calculate confusion matrix.')
})

test_that('performance_metrics throws an error when only false_negatives argument is given', {
  performance_metrics(false_negatives = 2) |>
    expect_error('Insufficient data to calculate confusion matrix.')
})

test_that('performance_metrics throws an error when only false_positives argument is given', {
  performance_metrics(false_positives = 2) |>
    expect_error('Insufficient data to calculate confusion matrix.')
})

test_that('performance_metrics throws an error when only true_negatives argument is given', {
  performance_metrics(true_negatives = 2) |>
    expect_error('Insufficient data to calculate confusion matrix.')
})

test_that('performance_metrics throws an error when total population does not add up correctly, due to an incorrect total_population argument', {
  performance_metrics(
    total_population = 20301,
    true_positives = 20,
    false_negatives = 10,
    false_positives = 180,
    true_negatives = 1820
  ) |>
    expect_error('Inconsistent data in argument total_population or in calculation thereof.')
})

test_that('performance_metrics returns expected variables', {
  expect_equal(
    test_object |> names() |> length(),
    desired_return_object |> names() |> length()
  )

  test_object |>
    names() |>
    purrr::map(\(x) test_object[[x]] == desired_return_object[[x]]) |>
    unlist() |>
    all() |>
    expect_true()
})

test_that('true_positive_rate calculated in two different ways yields the same result', {
  expect_equal(test_object$true_positives / test_object$real_positives, 1 - test_object$false_negative_rate)
})

test_that('false_negative_rate calculated in two different ways yields the same result', {
  expect_equal(test_object$false_negatives / test_object$real_positives, 1 - test_object$true_positive_rate)
})

test_that('false_positive_rate calculated in two different ways yields the same result', {
  expect_equal(test_object$false_positives / test_object$real_negatives, 1 - test_object$true_negative_rate)
})

test_that('true_negative_rate calculated in two different ways yields the same result', {
  expect_equal(test_object$true_negatives / test_object$real_negatives, 1 - test_object$false_positive_rate)
})

test_that('positive_predicitive_value calculated in two different ways yields the same result', {
  expect_equal(test_object$true_positives / (test_object$true_positives + test_object$false_positives), 1 - test_object$false_discovery_rate)
})

test_that('f1_score calculated in two different ways yields the same result', {
  expect_equal((2 * test_object$positive_predictive_value * test_object$true_positive_rate) / (test_object$positive_predictive_value + test_object$true_positive_rate), (2 * test_object$true_positives) / (2 * test_object$true_positives + test_object$false_positives + test_object$false_negatives))
})

test_that('false_omission_rate calculated in two different ways yields the same result', {
  expect_equal(test_object$false_negatives / (test_object$true_negatives + test_object$false_negatives), 1 - test_object$negative_predictive_value)
})

test_that('false_discovery_rate calculated in two different ways yields the same result', {
  expect_equal(test_object$false_positives / (test_object$true_positives + test_object$false_positives), 1 - test_object$positive_predictive_value)
})

test_that('negative_predictive_value calculated in two different ways yields the same result', {
  expect_equal(test_object$true_negatives / (test_object$true_negatives + test_object$false_negatives), 1 - test_object$false_omission_rate)
})
