# markov_chain class constructors ----------------------------------------------------------------

#' Construct a markov_chain object
#'
#' @param x The object from which the chain is to be constructed.
#' Can be missing, a single integer, a vector, a matrix or a data frame.
#' If x is:
#' - missing; an empty markov chain is produced.
#' - an integer; the produced chain will have $x$ states labeled
#' 1,2,...,x with no edges
#' - a vector; the elements of x will be cast into strings to serve
#' as labels of the output chain. No edges.
#' - a matrix; x will be the transition matrix of a chain. States
#' will be labels 1,2,...,n, where n is the number of rows/columns of x.
#' - a data frame; it needs to have string columns "to" and "from" and a numeric column "prob". It can also have
#' other columns (like color or loop_angle) for additional properties.
#' Each row will become an edge from "from" to "to" with probability "prob".
#'
#' @return a markov_chain object
#' @export
#'
markov_chain <- function(x=0, ...) {

  if ("matrix" %in% class(x))
    return(markov_chain_matrix(x,...))

  if ("data.frame" %in% class(x))
    return(markov_chain_data_frame(x,...))

  if ("character" %in% class(x))
    return(markov_chain_vector(x, ...))

  if ("integer" %in% class(x) |
      "numeric" %in% class(x)){
    if (length(x)>1) {
      return(markov_chain_vector(x, ...))
    } else {
      if (x==0)
        return(markov_chain_empty())
      else
        return(markov_chain_integer(x, ...))
    }
  }
  stop("Don't know how to make a Markov chain from the input.")
}


markov_chain_empty <- function() {
  value <- list(
    edges = tibble::tibble(
      from = integer(0),
      to = integer(0),
      prob = numeric(0),
      label = character(0),
      color = character(0),
      curve = numeric(0),
      loop_angle = double(0)
    ),
    states = tibble::tibble(
      index = integer(0),
      label = character(0),
      x = double(0),
      y = double(0),
      color = character(0),
      shape = character(0)
    ),
    layout = matrix(ncol = 2, nrow = 0),
    graphics_parameters = list()
  )
  class(value) <- "markov_chain"

  return(value)
}

markov_chain_integer <- function(n, ...) {
  m <- markov_chain_empty()
  if (n >= 1) {
      m = m %>% add_state(n, ...)
  } else {
    stop("number_of_states need to be a positive integer")
  }
  return(m)
}

markov_chain_vector <- function(x, ...){
  x = as.character(x)
  m <- markov_chain_empty() %>%
    add_state(x, ...)
  return(m)
}

markov_chain_matrix <- function(P, ...) {
  if (length(dim(P))!=2) stop("P needs to be a matrix")
  if (dim(P)[1] != dim(P)[2]) stop("P is not square")
  n = dim(P)[1]

  m=markov_chain_integer(n, ...)
  for (i in 1:n)
    for (j in 1:n)
      if (P[i,j] != 0)
        m = m %>% add_edge( i, j, prob = P[i,j])

  return(m)
}

markov_chain_data_frame = function(d, ...)
{
  if ( !("to" %in% names(d)) )
    stop("\"to\" needs to be a column label in the input data_frame")

  if (!("from" %in% names(d)))
    stop("\"from\" needs to be a column label in the input data_frame")

  d$to = as.character(d$to)
  d$from = as.character(d$from)

  states = sort(unique(c(d$to, d$from)))
  m = markov_chain_vector(states, ...)
  for (i in 1:nrow(d)) {
    m = do.call(add_edge, c(list(m), d[i,]))
  }
  return(m)
}



#' Builds a markov chain with random transitions
#'
#' @param n number of states
#' @param p a parameter governing the total number of edges
#'
#' @return a markov_chain object
#' @export
markov_chain_random <- function(n, p = 0.5) {
  P = t(replicate(n, random_row(n, p)))
  m = P %>%
    markov_chain_matrix %>%
    set_auto_layout %>%
    rotate(pca=TRUE) %>%
    set_graphics_parameters(edge.label = NA) %>%
    curve_overlapping_edges()
  m$edges$loop_angle = -pi / 2
  return(m)
}


random_row <- function(n, p) {
  num = stats::rbinom(1, n - 1, p) + 1
  non_zeros = stats::runif(num) %>%
    scale(center = F)
  row = c(non_zeros, rep(0, n - num)) %>%
    sample

  return(row / sum(row))

}
