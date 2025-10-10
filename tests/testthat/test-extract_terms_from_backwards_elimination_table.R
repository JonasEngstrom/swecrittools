test_that("extract_terms_from_backwards_elimination_table returns expected value", {
  tibble::tibble(
    independent_variables = c('(Intercept)', 'abc', 'def'),
    p_values = c(.5, .5, .5)
  ) |>
    extract_terms_from_backwards_elimination_table() |>
    expect_equal(c('abc', 'def'))
})

test_that("extract_terms_from_backwards_elimination_table throws error if independent_variables_column_name is set incorrectly", {
  tibble::tibble(
    independent_variables = c('(Intercept)', 'abc', 'def'),
    p_values = c(.5, .5, .5)
  ) |>
    extract_terms_from_backwards_elimination_table(independent_variables_column_name = 'foo') |>
    expect_error()
})
