#' Power calculations for superiority, non-inferiority and equivalence trials
#'
#' @param r Numeric with the ratio between groups A and B.
#' @param alpha Numeric with the alpha.
#' @param na Numeric with the sample size for group A.
#' @param mdiff Numeric with the differences between means.
#' @param sd Numeric with the standard deviation.
#' @param lim Numeric with the limit to be considered.
#' @param known Boolean if calculation should supose known variance.
#' @param type Character with the type of calculation. 'sup', 'equi' or 'ninf'.
#'
#' @return Array with power.
#' @export
#'
#' @details Flight L, Julious SA. Practical guide to sample size calculations: superiority trials. Pharmaceutical Statistics 2015
#' @details Flight L, Julious SA. Practical guide to sample size calculations: non-inferiority and equivalence trials. Pharmaceutical Statistics 2015
#'
#' @examples
#' x <- n_calc('ninf', 2, 0.05, 0.1, 0.5, 0, 1, 0, FALSE)
poder_calc <- function(r, alpha, na, mdiff, sd, lim, known, type) {
 if (type %in% c('ninf', 'sup')) {
  if (known) {
   beta = 1 - pnorm(sqrt((((mdiff - lim)^2) * r * na) / ((r + 1) * sd^2)) - qnorm(1 - alpha))
  } else {
   tau <- abs(((mdiff - lim) * sqrt(r * na)) / sqrt((r + 1) * (sd^2)))
   beta = pt(qt(1 - alpha, na * (r + 1) - 2), na * (r + 1) - 2, ncp = tau)
  }
 } else if (type %in% c('equi')) {
  if (known) {
   beta = 2 - pnorm(sqrt((((mdiff - lim)^2) * r * na) / ((r + 1) * sd^2)) - qnorm(1 - alpha)) - pnorm(sqrt((((mdiff + lim)^2) * r * na) / ((r + 1) * sd^2)) - qnorm(1 - alpha))
  } else {
   tau1 <- ((mdiff + lim) * sqrt(r * na)) / sqrt((r + 1) * (sd^2))
   tau2 <- ((mdiff - lim) * sqrt(r * na)) / sqrt((r + 1) * (sd^2))
   beta = 1 - pt(-qt(1 - alpha, na * (r + 1) - 2), na * (r + 1) - 2, ncp = tau2) + pt(qt(1 - alpha, na * (r + 1) - 2), na * (r + 1) - 2, ncp = tau1)
  }
 }

 return(invisible(1 - beta))
}
