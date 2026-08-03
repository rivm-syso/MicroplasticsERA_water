#' Correction factor for powerlaw using numerical integration
#'
#' Computes the correction factor between two ranges of particle sizes
#' assuming a powerlaw distribution, using the PDF from \code{poweRlaw::dplcon}
#' and numerical integration.
#'
#' @param ULm Upper limit of measured range
#' @param LLm Lower limit of measured range
#' @param ULd Upper limit of desired range
#' @param LLd Lower limit of desired range
#' @param alpha Powerlaw exponent
#'
#' @return Numeric correction factor
#' @examples
#' F.Correctionfactor2(ULm = 10, LLm = 1, ULd = 20, LLd = 2, alpha = 2.5)
#' @export
## Correction factor for powerlaw
F.Correctionfactor2 <- function(ULm,LLm,ULd,LLd,alpha){
  D = integrate(f = poweRlaw::dplcon, lower = LLd, upper = ULd, 
                alpha = alpha, xmin = LLd)
  M = integrate(f = poweRlaw::dplcon, lower = LLm, upper = ULm, 
                alpha = alpha, xmin = LLm)
  result = M$value/D$value
  return(result)
}

#' Correction factor for powerlaw using analytical solution
#'
#' Computes the correction factor between two ranges of particle sizes
#' assuming a powerlaw distribution
#'
#' Formula from Koelmans et al 2020
#' 
#' @param ULm Upper limit of measured range
#' @param LLm Lower limit of measured range
#' @param ULd Upper limit of desired range
#' @param LLd Lower limit of desired range
#' @param alpha Powerlaw exponent
#'
#' @return Analytical correction factor
#' @examples
#' F.Correctionfactor(ULm = 10, LLm = 1, ULd = 20, LLd = 2, alpha = 2.5)
#' @export
## Correction factor for powerlaw
F.Correctionfactor <- function(ULm,LLm,ULd,LLd,alpha){
  (ULd^(1-alpha)-LLd^(1-alpha))/(ULm^(1-alpha)-LLm^(1-alpha))
}
