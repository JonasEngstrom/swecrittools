test_that("remove_highest_p_value returns expected value", {
  tibble::tibble(
    independent_variables = c('(Intercept)', 'x1', 'x2', 'x3', 'x4'),
    p_value = c(1.18e-5, 4.36e-33, 4.97e-14, 6.02e-23, 8.44e-1)) |>
    remove_highest_p_value() |>
    expect_equal(c('x1', 'x2', 'x3'))
})

test_that("remove_highest_p_value fails if levels of a categorical variable are removed, creating ambigous variable names", {
  tibble::tibble(
    independent_variables = c('(Intercept)', 'x1', 'x2', 'x3', 'x4.g', 'x4.h', 'x4.i'),
    p_value = c(1.18e-5, 4.36e-33, 4.97e-14, 6.02e-23, 8.44e-1, 8.44e-52, 8.44e-53)
  ) |>
    remove_highest_p_value() |>
    expect_error()
})

test_that("remove_highest_p_value fails if independent_variable_column_name is set incorrectly", {
  tibble::tibble(
    independent_variables = c('(Intercept)', 'x1', 'x2', 'x3', 'x4'),
    p_value = c(1.18e-5, 4.36e-33, 4.97e-14, 6.02e-23, 8.44e-1)) |>
    remove_highest_p_value(independent_variable_column_name = 'foo') |>
    expect_error()
})
