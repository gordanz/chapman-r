# markov_chain class constructors ----------------------------------------------------------------

#' Construct a markov_chain object
#'
#' @param x The object from which the chain is to be constructed. Can be a single integer, a vector, a matrix or a data frame.
#'
#' @return
#' @export
#'
markov_chain <- function(x, ...) {

  if (x==0)
    return(markov_chain_empty(...))

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
      return(markov_chain_integer(x, ...))
    }
  }
  stop("Don't know how to make a Markov chain from the input.")
}


markov_chain_empty <- function(...) {
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
  m <- markov_chain_empty(...)
  if (n >= 1) {
      m = m %>% add_state(n)
  } else {
    stop("number_of_states need to be a positive integer")
  }
  return(m)
}

markov_chain_vector <- function(x, ...){
  x = as.character(x)
  m <- markov_chain_empty(...) %>%
    add_state(x)
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
  m = markov_chain_vector(states)
  for (i in 1:nrow(d)) {
    m = do.call(add_edge_by_labels, c(list(m), d[i,]))
  }
  return(m)
}
