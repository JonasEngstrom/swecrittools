test_that("average_impute imputes expected value", {
  tibble::tibble(a=c(1,2,3), b=c(1,NA,3), c=c(1,2,3)) |>
    average_impute('a', list('b' = 'pmm'), c('a', 'c'), printFlag = FALSE, seed = 1993) |>
    suppressWarnings() |>
    dplyr::slice(2) |>
    dplyr::pull(b_mean) |>
    expect_equal(2.6)
})
