#' Count and Format
#'
#' Takes a tibble, counts its rows, and formats the value with thousand dividers.
#'
#' @param input_tibble A tibble.
#'
#' @returns A string.
#' @export
#'
#' @examples
#' data.frame(a = c(1,2,3)) |> count_and_format()
count_and_format <- function(input_tibble) {
  input_tibble |>
    dplyr::count() |>
    dplyr::pull() |>
    format(big.mark = ',') |>
    (\(x) return(x))()
}
