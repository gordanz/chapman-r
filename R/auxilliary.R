#' Create a univariate distribution table
#'
#' Create a table using kableExtra
#'
#' @param x vector of possible values
#' @param p vector of their probabilities
#'
#' @return html?
#'
#'
#' @export
#' @examples
#' univariate_table(c(1,2,3), c(0.1, 0.3, 0.6))
univariate_table <- function(x, p) {
  df = t(data.frame(prob = p))
  colnames(df) = as.character(x)
  kableExtra::kable(df, row.names = FALSE) %>%
    kableExtra::kable_styling(full_width = FALSE)
}



#' Checks, graphically, if two vectors are independent
#'
#' @param x vector
#' @param y vector
#'
#' @return a plot
#'
#' @export
#' @examples
#' permutation_test( c(1,2,3,4,5), c(2,3,4,5,6))
permutation_test = function(x, y) {
  graphics::par(mfrow = c(2, 2))
  y_perm_1 = sample(y)
  y_perm_2 = sample(y)
  y_perm_3 = sample(y)
  plot(x, y)
  plot(x, y_perm_1)
  plot(x, y_perm_2)
  plot(x, y_perm_3)
}
