test_that("count_and_format formats correctly", {
  tibble::tibble(a = c(1,2,3)) |>
    count_and_format() |>
    expect_equal('3')

  tibble::tibble(a = rep(1, 1000)) |>
    count_and_format() |>
    expect_equal('1,000')

  tibble::tibble(a = rep(1, 10000)) |>
    count_and_format() |>
    expect_equal('10,000')
})
