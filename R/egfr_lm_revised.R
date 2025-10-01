#' Calculate Creatinine Based eGFR Using the LM Revised Equation
#'
#' Calculate estimated glomerular filtration rate (eGFR) using the Revised Lund-Malmö equation without body weight measure (LM Revised) described by Björk et al\eqn{^1}.
#'
#' \eqn{\text{eGFR} = e^{X - 0.0158 \times \text{age} + 0.438 \times \ln (\text{age})}}
#'
#' |**Sex**|**Plasma Creatinine (µmol/l)**|**X**|
#' |:-:|:-:|:-:|
#' |Female|<150|\eqn{X = 2.50 + 0.0121 \times (150 - \text{creatinine})}|
#' |Female|≥150|\eqn{X = 2.50 - 0.926 \times \ln (\frac{\text{creatinine}}{150})}|
#' |Male|<180|\eqn{X = 2.56 + 0.00968 \times (180 - \text{creatinine})}|
#' |Male|≥180|\eqn{X = 2.56 - 0.926 \times \ln (\frac{\text{creatinine}}{180})}|
#'
#' @references 1. Björk J, Grubb A, Sterner G, Nyman U. Revised equations for estimating glomerular filtration rate based on the Lund-Malmö Study cohort. Scandinavian Journal of Clinical and Laboratory Investigation. 2011 May;71(3):232–9.
#'
#' @param creatinine Plasma creatinine in µmol/l.
#' @param age Age in years.
#' @param female Sex. `TRUE` if female, `FALSE` if male.
#'
#' @returns eGFR in ml/min/1.73 m\eqn{^2}
#' @export
#'
#' @md
#'
#' @examples
#' egfr_lm_revised(50, 23, TRUE)
egfr_lm_revised <- function(creatinine, age, female) {
  x <-
    ifelse(
      female,
      ifelse(
        creatinine < 150,
        2.50 + 0.0121 * (150 - creatinine),
        2.50 - 0.926 * log(creatinine / 150)
      ),
      ifelse(
        creatinine < 180,
        2.56 + 0.00968 * (180 - creatinine),
        2.56 - 0.926 * log(creatinine / 180)
      )
    )

  egfr <- exp(x - 0.0158 * age + 0.438 * log(age))

  return(egfr)
}
