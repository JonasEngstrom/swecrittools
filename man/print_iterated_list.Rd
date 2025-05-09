% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print_iterated_list.R
\name{print_iterated_list}
\alias{print_iterated_list}
\title{Print Iterated List}
\usage{
print_iterated_list(iterated_list, conjunction = "and", oxford_comma = TRUE)
}
\arguments{
\item{iterated_list}{The list to collapse.}

\item{conjunction}{What conjunction to use. Defaults to \emph{and}.}

\item{oxford_comma}{Whether to use an Oxford comma. Defaults to \code{TRUE}.}
}
\value{
A string.
}
\description{
Collapses and prints a list with a conjunction at the end. Gives the user the option to use an Oxford comma or not. E.g. prints \code{c('Athos', 'Porthos', 'Aramis', 'D’Artagnan')} as \emph{Athos, Porthos, Aramis, and D’Artagnan}.
}
\examples{
c('Athos', 'Porthos', 'Aramis', 'D’Artagnan') |> print_iterated_list()
}
