#' Print Iterated List
#'
#' Collapses and prints a list with a conjunction at the end. Gives the user the option to use an Oxford comma or not. E.g. prints `c('Athos', 'Porthos', 'Aramis', 'D’Artagnan')` as *Athos, Porthos, Aramis, and D’Artagnan*.
#'
#' @param iterated_list The list to collapse.
#' @param conjunction What conjunction to use. Defaults to *and*.
#' @param oxford_comma Whether to use an Oxford comma. Defaults to `TRUE`.
#'
#' @returns A string.
#' @export
#'
#' @md
#'
#' @examples
#' c('Athos', 'Porthos', 'Aramis', 'D’Artagnan') |> print_iterated_list()
print_iterated_list <- function(
    iterated_list,
    conjunction = 'and',
    oxford_comma = TRUE
) {

  # Remove the Oxford comma if the list is shorter than three items.
  if (length(iterated_list) < 3) {
    oxford_comma = FALSE
  }

  # Prepare pattern to replace last comma in list.
  conjunction_pattern <-
    c(ifelse(oxford_comma, ',', ''), ' ', conjunction, ' ') |>
    paste(collapse = '')

  # Join list and replace last comma with conjunction.
  iterated_list |>
    paste(collapse = ', ') |>
    stringr::str_replace(', (?=[^, ]*$)', conjunction_pattern) |>
    (\(x) return(x))()
}
