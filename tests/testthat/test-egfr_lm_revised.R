test_that("egfr_lm_revised yields same result as efr.se", {
  egfr_lm_revised(45, 25, TRUE) |>
    round() |>
    expect_equal(120)

  egfr_lm_revised(160, 25, TRUE) |>
    round() |>
    expect_equal(32)

  egfr_lm_revised(75, 25, FALSE) |>
    round() |>
    expect_equal(99)

  egfr_lm_revised(200, 25, FALSE) |>
    round() |>
    expect_equal(32)

  egfr_lm_revised(45, 75, TRUE) |>
    round() |>
    expect_equal(88)

  egfr_lm_revised(160, 75, TRUE) |>
    round() |>
    expect_equal(23)

  egfr_lm_revised(75, 75, FALSE) |>
    round() |>
    expect_equal(72)

  egfr_lm_revised(200, 75, FALSE) |>
    round() |>
    expect_equal(24)
})
