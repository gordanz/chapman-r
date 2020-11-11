# Indices <-> labels -----------------------------------------------------------

#' Returns the number of states
#'
#' @param m markov_chain object
#'
#' @return integer
#' @export
length.markov_chain = function(x) {
  m = x # x is for consistency with length
  nrow(m$states)
}

#' Return the number of edges
#'
#' @param m a markov_chain object
#'
#' @return integer
#' @export
nedges = function(m) {
  nrow(m$edges)
}

check_index = function(m, index) {
  if (index < 1 | (index > length(m)))
    stop(paste(index, "is not a valid index."))
}


index_of <- function(m, x) {
  if (class(x) == "integer" | class(x) == "numeric") {
    check_index(m,x)
    return(x)
  } else {
    index = match(x, m$states$label, nomatch = 0)
    if (index)
      return(index)
    else
      stop(paste("No label", x))
  }
}

label_of <- function(m, x) {
  if (class(x)!="integer" & class(x)!="numeric") {
    return(x)
  } else {
    check_index(m,x)
    return(m$states$label[x])
  }
}



edge_of = function(m, from, to) {
  from = index_of(m,from)
  to = index_of(m,to)

  edge_indices = which((m$edges$from == from &
                          m$edges$to == to))
  if (length(edge_indices) > 1)
    stop("Something wrong with your chain. Too many matches")
  if (length(edge_indices) == 0){
    return(0)
  } else {
    return(edge_indices)
  }
}

# shortcuts
l = label_of
i = index_of
e = edge_of


# New states and edges ----------------------------------------------------


#' Adds a state to a markov_chain
#'
#' @param m a markov_chain object
#' @param l a vector. States with labels in l (cast to strings) are added. If length(l)==1
#'   and l is numeric, l states with labels "1", ..., "l" are added.
#' @param ... Extra arguments like color, x, y, etc.
#'
#' @return markov_chain object
#' @export
#'
add_state <- function(m, l=NA, ...) {

  if (anyNA(l))
    l = default$state.label.function(m)

  single_int = length(l) == 1 & ("integer" %in% class(l) | "numeric" %in% class(l))
  if (single_int) {
        l = as.character(seq(length(m)+1, length(m)+l))
      } else {
        l = as.character(l)
      }
  for (label in l){
    m = add_state_by_label(m,label=label,...)
  }

  return(m)

}


add_state_by_label <- function(m,...) {
  defaults = list(
    label = default$state.label.function(m),
    color = default$state.color,
    shape = default$state.shape,
    x = default$state.x,
    y = default$state.y,
    index = length(m)+1
  )
  args = utils::modifyList(defaults, list(...))
  m$states = do.call(tibble::add_row, c(list(m$states),args))
  m$layout = rbind(m$layout, c(args$x,args$y))
  return(m)
}

#' Adds an edge to a markov_chain
#'
#' @param m a markov_chain object
#' @param from a state. If string then interpreted as a label, otherwise index.
#' @param to a state. If string then interpreted as a label, otherwise index.
#' @param prob the probability (defaults fo 1)
#' @param ... Extra arguments like color, loop_angle, curve, etc.
#'
#' @return markov_chain object
#' @export
add_edge <- function(m, from, to, prob=1, ...) {
  defaults = list(
    color = ifelse(prob<1, default$edge.color, default$edge.one.color),
    loop_angle = default$edge.loop_angle,
    label = default$edge.label.function(prob),
    curve = 0,
    from = index_of(m,from),
    to = index_of(m,to),
    prob = prob
  )
  args = utils::modifyList(defaults, list(...))
  m$edges = do.call(tibble::add_row, c(list(m$edges),args))
  return(m)
}

# Property setters -----------------------------------------------------

#' Sets properties of a state
#'
#' @param m a markov_chain object
#' @param x if integer, state index, otherwise state label
#' @param ... named properties to be set
#'
#' @return a markov_chain object
#' @export
set_state = function(m, x, ...) {
  properties <- list(...)
  if (!all(names(properties) %in% names(m$states)))
    stop("Trying to set a non-existing property.")
  for (s in x) {
    m$states[index_of(m,s), names(properties)] = properties
  }
  return(m)
}

#' Sets edge properties
#'
#' @param m a markov_chain object
#' @param from if integer index, otherwise state label
#' @param to if integer index, otherwise state label
#' @param ... a list of named properties to be set
#'
#' @return markov_chain
#' @export
set_edge= function(m, from, to, ...) {
  from = index_of(m,from)
  to = index_of(m,to)
  properties <- list(...)
  if (!all(names(properties) %in% names(m$edges)))
    stop("Trying to set a non-existing property.")
  m$edges[edge_of(m, from, to), names(properties)] = properties
  return(m)
}



