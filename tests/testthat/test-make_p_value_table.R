test_that("make_p_value_table runs as expected", {
  tibble::tibble(
    a = c(1, 2, 3),
    b = c(1, 2, 3),
    c = c(1, 2, 3)
  ) |>
    make_p_value_table('c', c('a', 'b')) |>
    expect_equal(
      tibble::tibble(
        independent_variables = c('(Intercept)', 'a'),
        p_value = c(NaN, 0)
      )
    )
})

test_that("make_p_value_table stops if list of independent variables is too short", {
  tibble::tibble(
    a = c(1, 2, 3),
    b = c(1, 2, 3),
    c = c(1, 2, 3)
  ) |>
    make_p_value_table('c', c()) |>
    expect_error()
})

test_that("Renaming p-value column works as expected in make_p_value_table", {
  tibble::tibble(
    a = c(1, 2, 3),
    b = c(1, 2, 3),
    c = c(1, 2, 3)
  ) |>
    make_p_value_table('c', c('a', 'b'), p_value_column_name = 'test_name') |>
    expect_equal(
      tibble::tibble(
        independent_variables = c('(Intercept)', 'a'),
        test_name = c(NaN, 0)
      )
    )
})

test_that("make_p_value_table throws error if initial_p_value_column_name is not the same as that returned from summary of glm", {
  tibble::tibble(
    a = c(1, 2, 3),
    b = c(1, 2, 3),
    c = c(1, 2, 3)
  ) |>
    make_p_value_table('c', c('a', 'b'), initial_p_value_column_name = 'foo') |>
    expect_equal(
      tibble::tibble(
        independent_variables = c('(Intercept)', 'a'),
        p_value = c(NaN, 0)
      )
    ) |>
    expect_error()
})
