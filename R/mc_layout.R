# Layout functions for for markov_chain --------------------------------------------------

#' Rotate the layout of a markov_chain
#'
#' @param m a markov_chain object
#' @param alpha angle
#' @param pca whether to set angle using PCA
#'
#' @return
#' @export
#'
#' @examples
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
#' @param m
#' @param ... additional arguments to builtin "scale"
#'
#' @return
#' @export
#'
#' @examples
autoscale <- function(m, ...) {
  m$layout = scale(m$layout,...)
  return(m)
}

#' Stretch (scale) the layout of a markov_chain
#'
#' @param m
#' @param ax stretch factor in x
#' @param ay stretch factor in y
#'
#' @return
#' @export
#'
#' @examples
stretch <- function(m, ax,ay=ax){
  points = m$layout
  A = matrix(c(ax,0,0,ay),nrow=2)
  m$layout = (points %*% A)
  return(m)
}

#' Shift the layout of a markov_chain
#'
#' @param m
#' @param ax shift amount in x
#' @param ay shift amount in y
#'
#' @return
#' @export
#'
#' @examples
shift <- function(m, ax,ay=0){
  points = m$layout
  A = matrix(c(ax,ay),byrow = TRUE, ncol=2, nrow=nrow(points))
  m$layout = points + A
  return(m)
}



#' Sets an automatic layout using the specified algorithm
#'
#' @param m
#' @param algorithm an igraph layout algorithm
#'
#' @return
#' @export

set_auto_layout = function(m, algorithm = default$layout.algorithm) {
  g = igraph::graph_from_data_frame(m$edges, directed = TRUE, vertices = m$states)
  m$layout = scale(algorithm(g))
  return(m)
}

# Other functions -----------------
#' Make overlapping edges avoid each other
#'
#' @param m
#' @param curve the curvature
#'
#' @return
#' @export
#'
#' @examples
#' P = matrix( rep(1/3,9), ncol=3)
#' m = new_markov_chain_from_matrix(P)
curve_overlapping_edges = function(m, curve = pkg.env$default.curve) {
  if (nedges(m) == 0) return(m)

  for (k in 1:nedges(m)) {
    i = m$edges$from[k]
    j = m$edges$to[k]
    if (index_pair_to_edge(m,j,i))
    {
      m = set_edge_properties_by_indices(m, i, j, curve=curve)
      m = set_edge_properties_by_indices(m, j, i, curve=curve)
    }
  }
  return(m)
}


