test_that("egfr_capa yields same result as efr.se", {
  egfr_capa(.92, 25) |>
    round() |>
    expect_equal(91)
})
