test_that("print_iterated_list returns expected value with default settings", {
  c('Athos', 'Porthos', 'Aramis', 'D’Artagnan') |>
    print_iterated_list() |>
    expect_equal('Athos, Porthos, Aramis, and D’Artagnan')
})

test_that("print_iterated_list returns expected value with short list", {
  c('Athos', 'D’Artagnan') |>
    print_iterated_list() |>
    expect_equal('Athos and D’Artagnan')
})

test_that("print_iterated_list returns value without Oxford comma", {
  c('Athos', 'Porthos', 'Aramis', 'D’Artagnan') |>
    print_iterated_list(oxford_comma = FALSE) |>
    expect_equal('Athos, Porthos, Aramis and D’Artagnan')
})

test_that("print_iterated_list returns expected value with non-default conjunction", {
  c('Athos', 'Porthos', 'Aramis', 'D’Artagnan') |>
    print_iterated_list(conjunction = 'or') |>
    expect_equal('Athos, Porthos, Aramis, or D’Artagnan')
})

test_that("print_iterated_list returns expected value without Oxford comma and with non-default conjunction", {
  c('Athos', 'Porthos', 'Aramis', 'D’Artagnan') |>
    print_iterated_list(conjunction = 'or', oxford_comma = FALSE) |>
    expect_equal('Athos, Porthos, Aramis or D’Artagnan')
})

test_that("print_iterated_list returns expected value with short list and non-default conjunction", {
  c('Athos', 'D’Artagnan') |>
    print_iterated_list(conjunction = 'or') |>
    expect_equal('Athos or D’Artagnan')
})

test_that("print_iterated_list returns expected value with single value", {
  c('Athos') |>
    print_iterated_list() |>
    expect_equal('Athos')
})

test_that("print_iterated_list returns expected value with single value wihtout Oxford comma", {
  c('Athos') |>
    print_iterated_list(oxford_comma = FALSE) |>
    expect_equal('Athos')
})

test_that("print_iterated_list returns expected value with single value with non-default conjunction", {
  c('Athos') |>
    print_iterated_list(conjunction = 'or') |>
    expect_equal('Athos')
})

test_that("print_iterated_list returns expected value with single value wihtout Oxford comma and with non-default conjunction", {
  c('Athos') |>
    print_iterated_list(conjunction = 'or', oxford_comma = FALSE) |>
    expect_equal('Athos')
})
