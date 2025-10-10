test_that("make_backwards_elimination_table returns expected values", {
  set.seed(123)
  tibble::tibble(
    x1 = rnorm(50, 5, 2),
    x2 = rnorm(50, 3, 1.5),
    x3 = rnorm(50, 10, 5),
    x4 = rnorm(50, 0, 1),
    y = 3 + 2 * x1 - 1 * x2 + 0.5 * x3 + rnorm(50, 0, 1)
  ) |>
    make_backwards_elimination_table('y', c('x1', 'x2', 'x3', 'x4')) |>
    expect_equal(
      tibble::tibble(
        independent_variables = c('(Intercept)', 'x1', 'x3', 'x2', 'x4'),
        p_values_run_1 = c(6.876735e-08, 1.200655e-28, 5.696304e-22, 7.301994e-14, 2.920481e-01),
        p_values_run_2 = c(2.063649e-08, 4.476376e-29, 3.474373e-22, 2.141082e-14, NA)
      ),
      tolerance = 1e-7
    )
})

test_that("Changing p_value_colum_prefix parameter in make_backwards_elimination_table works as expected", {
  set.seed(123)
  tibble::tibble(
    x1 = rnorm(50, 5, 2),
    x2 = rnorm(50, 3, 1.5),
    x3 = rnorm(50, 10, 5),
    x4 = rnorm(50, 0, 1),
    y = 3 + 2 * x1 - 1 * x2 + 0.5 * x3 + rnorm(50, 0, 1)
  ) |>
    make_backwards_elimination_table('y', c('x1', 'x2', 'x3', 'x4'), p_value_column_prefix = 'foo_') |>
    expect_equal(
      tibble::tibble(
        independent_variables = c('(Intercept)', 'x1', 'x3', 'x2', 'x4'),
        foo_1 = c(6.876735e-08, 1.200655e-28, 5.696304e-22, 7.301994e-14, 2.920481e-01),
        foo_2 = c(2.063649e-08, 4.476376e-29, 3.474373e-22, 2.141082e-14, NA)
      ),
      tolerance = 1e-7
    )
})

test_that("Changing p_value_limit parameter in make_backwards_elimination_table works", {
  set.seed(123)
  tibble::tibble(
    x1 = rnorm(50, 5, 2),
    x2 = rnorm(50, 3, 1.5),
    x3 = rnorm(50, 10, 5),
    x4 = rnorm(50, 0, 1),
    y = 3 + 2 * x1 - 1 * x2 + 0.5 * x3 + rnorm(50, 0, 1)
  ) |>
    make_backwards_elimination_table('y', c('x1', 'x2', 'x3', 'x4'), p_value_limit = .06) |>
    expect_equal(
      tibble::tibble(
        independent_variables = c('(Intercept)', 'x1', 'x3', 'x2', 'x4'),
        p_values_run_1 = c(6.876735e-08, 1.200655e-28, 5.696304e-22, 7.301994e-14, 2.920481e-01),
        p_values_run_2 = c(2.063649e-08, 4.476376e-29, 3.474373e-22, 2.141082e-14, NA)
      ),
      tolerance = 1e-7
    )
})
