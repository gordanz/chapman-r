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

#' Classifies states of the chain
#'
#' @param m a markov_chain object
#'
#' @return a markov_chain object
#' @export
classify = function(m) {

  P = sign(transition_matrix(m))
  n = length(m)
  S = diag(n)*0
  Q = diag(n)
  for (i in 1:n) {
    S = sign(S + Q)
    Q = sign(Q %*% P)
  }
  rel = (S+t(S)) == 2

  class_of = rep(0,n)
  cl = 0
  for (i in 1:n) {
    if (class_of[i] == 0) {
      cl = cl + 1
      class_of[i]=cl
      for (j in i:n) {
        if (rel[i,j]) {
          class_of[j] = cl
        }
      }
    }
  }

  if (! "class" %in% colnames(m$states)){
    m$states = m$states %>%
      tibble::add_column(class = class_of)
  }

  if (! "recurrent" %in% colnames(m$states)){
    m$states = m$states %>%
      tibble::add_column(recurrent = TRUE)
  }

  class_rec = rep(TRUE, cl)

  for (k in 1:nedges(m)) {
    i = m$edges$from[k]
    j = m$edges$to[k]
    if ( m$states$class[i] != m$states$class[j] ) {
      class_rec[m$states$class[i]] = FALSE
    }
  }
  for (i in 1:length(m)) {
    m$states$recurrent[i] = class_rec[m$states$class[i]]
  }

  return(m)
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

#' Returns the vector of transient states
#'
#' @param m a markov_chain object
#'
#' @return vector is indices
#' @export
transient_states = function(m) {
  if (!is_classified(m)) {
    m = classify(m)
  }
  return(m$states$id[m$states$recurrent == FALSE])
}

#' Returns the vector of transient states
#'
#' @param m a markov_chain object
#'
#' @return vector is indices
#' @export

recurrent_states = function(m) {
  if (!is_classified(m)) {
    m = classify(m)
  }
  return(m$states$id[m$states$recurrent == TRUE])
}




