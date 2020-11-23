# Layout functions for for markov_chain --------------------------------------------------

#' Rotate the layout of a markov_chain
#'
#' @param m a markov_chain object
#' @param alpha angle
#' @param pca whether to set angle using principal components
#'
#' @return a markov_chain object
#' @export
rotate <- function(m, alpha, pca = FALSE) {

  if (!pca & missing(alpha)){
    stop("Need an angle or pca=TRUE")
  }

  if (pca) {
    rot_mat = 0.5 * stats::prcomp(m$layout)$rotation
  } else {
    rot_mat = matrix(
      c(cos(alpha), - sin(alpha), sin(alpha), cos(alpha)),
      ncol = 2
    )
  }
  m$layout = m$layout %*% rot_mat
  return(m)
}

#' Scales the layout to mean=0, var=1
#'
#' @param m a markov_chain object
#' @param ... additional arguments to builtin "scale"
#'
#' @return a markov_chain object
#' @export
autoscale <- function(m, ...) {
  m$layout = scale(m$layout,...)
  return(m)
}

#' Stretch (scale) the layout of a markov_chain
#'
#' @param m a markov_chain object
#' @param ax stretch factor in x
#' @param ay stretch factor in y
#'
#' @return a markov_chain object
#' @export
stretch <- function(m, ax,ay=ax){
  m$layout = m$layout %*%
    matrix(c(ax,0,0,ay),nrow=2)
  return(m)
}

#' Shift the layout of a markov_chain
#'
#' @param m a markov_chain object
#' @param ax shift amount in x
#' @param ay shift amount in y
#'
#' @return a markov_chain object
#' @export
shift <- function(m, ax,ay=0){
  m$layout = m$layout +
    matrix(c(ax,ay),byrow = TRUE, ncol=2, nrow=nrow(m$layout))
  return(m)
}

#' Sets an automatic layout using the specified algorithm
#'
#' @param m a markov_chain object
#' @param algorithm an igraph layout algorithm
#'
#' @return a markov_chain object
#' @export
set_auto_layout = function(m, algorithm = default$layout.algorithm) {
  g = graph_of(m)
  m$layout = scale(algorithm(g))
  return(m)
}

# Other functions -----------------
#' Make overlapping edges avoid each other
#'
#' @param m a markov_chain object
#' @param curve the curvature
#'
#' @return a markov_chain object
#' @export
curve_overlapping_edges = function(m, curve = default$edge.curve) {
  if (nedges(m) == 0) return(m)

  for (k in 1:nedges(m)) {
    i = m$edges$from[k]
    j = m$edges$to[k]

    if (edge_of(m,j,i))
    {
      m = m %>%
        set_edge(i, j, curve=curve) %>%
        set_edge(j, i, curve=curve)
    }
  }
  return(m)
}


