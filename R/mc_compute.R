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


#' Check whether the states of the chain have already been classified
#'
#' @param m
#'
#' @return logical
#' @export
is_classified = function(m) {
  return("class" %in% colnames(m$states))
}


# load_all(); m = deck22(); g = graph_from_data_frame(m$edges, directed=T, vertices=m$states)
