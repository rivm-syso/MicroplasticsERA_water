#' Calculate the Mean of a Power-law Distribution within Limits
#'
#' Computes the mean value (ERM) of a power-law distribution between an upper and lower limit,
#' using analytical solutions for special cases \eqn{\alpha = 1} and \eqn{\alpha = 2}.
#' For general alpha, a generic formula is used. See SI of Mehinto et al. for details.
#'
#' @param Xul Numeric. Upper limit of the interval.
#' @param Xll Numeric. Lower limit of the interval.
#' @param alpha Numeric. Power-law slope.
#'
#' @return Numeric. The mean value within the interval \code{[Xll, Xul]} for the given alpha.
#' @examples
#' F.Umeanx(100, 10, 1.5)
#' F.Umeanx(100, 10, 1)
#' F.Umeanx(100, 10, 2)
#' @export

F.Umeanx <- function(Xul,Xll,alpha){
  ## First find out if alpha is 1 or 2
  result <- case_when(alpha == 1 ~ as.numeric((Xul-Xll)/(log(Xul/Xll))),
                      alpha == 2 ~  as.numeric((log(Xul/Xll)/(Xll^-1-Xul^-1))),
                      Xul == Xll ~ Xul,
                      TRUE ~ as.numeric((1-alpha)/(2-alpha) * (Xul^(2-alpha)-Xll^(2-alpha))/(Xul^(1-alpha)-Xll^(1-alpha)))
  )
  
  return(result)
}