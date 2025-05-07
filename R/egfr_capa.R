#' Calculate Cystatin C Based eGFR Using the CAPA Equation
#'
#' Calculate estimated glomerular filtration rate (eGFR) using the Caucasian, Asian, Pediatric, and Adult (CAPA) formula described by Grubb et al\eqn{^1}.
#'
#' \eqn{\text{eGFR} = 130 \times \text{cystatin C}^{-1.069} \times \text{age}^{-0,117} - 7}
#'
#' @references 1. Grubb A, Horio M, Hansson LO, Björk J, Nyman U, Flodin M, et al. Generation of a new cystatin C-based estimating equation for glomerular filtration reate by use of 7 assays standardized to the international calibrator. Clinical Chemistry. 2014 Jul 1;60(7):974–86.

#'
#' @param cystatin_c Plasma cystatin C in mg/l.
#' @param age Age in years.
#'
#' @returns eGFR in ml/min/1.73 m\eqn{^2}
#' @export
#'
#' @md
#'
#' @examples
#' egfr_capa(.92, 25)
egfr_capa <- function(cystatin_c, age) {
  130 * cystatin_c ^ -1.069 * age ^ -.117 - 7
}
