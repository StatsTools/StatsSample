#' Sample size calculations for superiority, non-inferiority and equivalence trials
#'
#' @param type Character with the type of calculation. 'sup', 'equi' or 'ninf'.
#' @param r Numeric with the ratio between groups A and B.
#' @param alpha Numeric with the alpha.
#' @param beta Numeric with the beta.
#' @param sd Numeric with the standard deviation.
#' @param mdiff Numeric with the differences between means.
#' @param lim Numeric with the limit to be considered.
#' @param drop Numeric with the dropout rate.
#' @param known Boolean if calculation should supose known variance.
#'
#' @return Array with sample size.
#' @export
#'
#' @details Flight L, Julious SA. Practical guide to sample size calculations: superiority trials. Pharmaceutical Statistics 2015
#' @details Flight L, Julious SA. Practical guide to sample size calculations: non-inferiority and equivalence trials. Pharmaceutical Statistics 2015
#'
#' @examples
#' x <- n_calc('ninf', 2, 0.05, 0.1, 0.5, 0, 1, 0, FALSE)
n_calc <- function(type, r, alpha, beta, sd, mdiff, lim, drop = 0, known = TRUE) {
alpha_orig <- alpha
beta_orig <- beta

if (type == 'sup') alpha <- alpha / 2
if (type == 'equi') beta <- beta / 2

na <- ((r + 1) * (qnorm(1 - beta) + qnorm(1 - alpha))^2 * sd^2) / (r * (mdiff + lim)^2)

na1 <- ceiling(na)
na <- ceiling(na1 / (1 - drop))
nb <- r * na
n_total <- na + nb

poder <- poder_calc(r, alpha, na1, mdiff, sd, lim, known, type)

cat('Tipo de Teste:', if (type == 'sup') 'Superioridade' else if (type == 'ninf') 'Não Inferioridade' else 'Equivalência')
cat('\n-----------------------------------------')
cat('\nParâmetros:\n\n', '- Razão:', r, '\n - Alpha:', alpha_orig, '\n - Beta:', beta_orig,
    '\n - Desvio Padrão:', sd, '\n - Diferença de Médias:', mdiff, '\n - Efeito:',
    lim, '\n - Taxa de Perda:', round(100 * drop, 2), '%')
cat('\n\nN Controle:', na, '\nN Tratamento:', nb, '\nN Total:', n_total)
cat('\n\nPoder do teste:', round(100 * poder, 2), '%\n')
cat('-----------------------------------------\n')

return(invisible(c(na = na, nb = nb, poder = poder)))
}
