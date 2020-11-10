# Transition matrix -------------------------------------------------------

#' Returns the transition matrix
#'
#' @param m a markov_chain object
#'
#' @return a matrix
#' @export
transition_matrix <- function(m) {
  P = matrix( nrow=length(m), ncol=length(m))
  for (i in 1:length(m))
    for (j in 1:length(m)) {
      edge = e(m,i,j)
      if (edge) {
        P[i,j] = as.double(m$edges[edge,]$prob)
      } else {
        P[i,j] = 0
      }
    }
  dimnames(P) = list(m$states$label, m$states$label)
  return(P)
}

