# markov_chain class constructor ----------------------------------------------------------------

markov_chain <- function(x, ...) {
  switch(typeof(x))
}

markov_chain_empty <- function(name = "Unnamed") {
  value <- list(
    name = name,
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

markov_chain_integer <- function(number_of_states, ...) {
  m <- markov_chain_empty(...)
  if (!is.na(number_of_states) & number_of_states >= 1) {
    for (i in 1:number_of_states) {
      m = m %>% add_state()
    }
  } else {
    stop("number_of_states need to be a positive integer")
  }
  return(m)
}

markov_chain_vector <- function(x, ...){
  x = as.character(x)
  m <- markov_chain_empty(...)
  for (i in 1:length(x)) {
    m = m %>% add_state(label=x[i])
  }
  return(m)
}

markov_chain_matrix <- function(P, ...) {
  if (length(dim(P))!=2) stop("P needs to be a matrix")
  if (dim(P)[1] != dim(P)[2]) stop("P is not square")
  n = dim(P)[1]

  m=markov_chain_integer(number_of_states = n, ...)
  for (i in 1:n)
    for (j in 1:n)
      if (P[i,j] != 0)
        m = m %>% add_edge_by_indices( i, j, prob = P[i,j])

  return(m)
}

markov_chain_data_frame = function(df, ...)
{
  if ( !("to" %in% names(df)) )
    stop("\"to\" needs to be a column label in the input data_frame")

  if (!("from" %in% names(df)))
    stop("\"from\" needs to be a column label in the input data_frame")

  df$to = as.character(df$to)
  df$from = as.character(df$from)

  states = sort(unique(c(df$to, df$from)))
  m = markov_chain_vector(states)
  for (i in 1:nrow(df)) {
    m = do.call(add_edge_by_labels, c(list(m), df[i,]))
  }
  return(m)
}


# Indices <-> labels -----------------------------------------------------------

#' Returns the number of states
#'
#' @param m markov_chain object
#'
#' @return integer
#' @export
#'
#' @examples
#' m = new_markov_chain("Chain with 3 states", number_of_states = 3)
#' print(length(m))
length.markov_chain = function(x) {
  m = x # x is for consistency with length
  nrow(m$states)
}

#' Return the number of edges
#'
#' @param m markov_chain object
#'
#' @return integer
#' @export
#'
#' @examples
#' m = new_markov_chain("Chain with 3 states", number_of_states = 3)
#' m = add_edge_by_labels("1","2")
#' m = add_edge_by_labels("1","3")
#' print(nedges(m))
nedges = function(m) {
  nrow(m$edges)
}

check_index = function(m, index) {
  if (index < 1 | (index > length(m)))
    stop(paste(index, "is not a valid index."))
}

#' Index of a state given its label
#'
#' @param m markov_chain
#' @param label
#'
#' @return integer or 0 if no match
#' @export
#'
#' @examples
#' m = new_markov_chain("Empty chain")
#' m = add_states(m, c("a","b"))
#' print(label_to_index(m,"b"))
#' print(label_to_index(m,"c"))
label_to_index <- function(m, label) {
  index = match(label, m$states$label, nomatch = 0)
  if (index)
    return(index)
  else
    stop(paste("No label", label))
}

#' Label of a state given index
#'
#' @param m markov_chain
#' @param index
#'
#' @return string label
#' @export
#'
#' @examples
#' m = new_markov_chain()
#' m = add_states(m,c("a","b","c"))
#' print(index_to_label(m, 2))
index_to_label <- function(m, index) {
  check_index(m, index)
  return(m$states$label[index])
}

#' The row of the edges data frame from "from" and "to"
#' Returns 0 if there is not such edge
#'
#' @param m
#' @param from
#' @param to
#'
#' @return
#' @export
#'
#' @examples
#' m = random_markov_chain(3)
#' j = index_pair_to_edge(m,1,2)
#' m$edges[j,]
index_pair_to_edge = function(m, from, to) {
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

#' The row of the edges data frame from "from" and "to"
#'
#' @param m
#' @param from
#' @param to
#'
#' @return
#'
#' @examples
label_pair_to_edge = function(m, from, to) {
  index_pair_to_edge(m, i(m, from), i(m, to))
}

# shortcuts
l = index_to_label
i = label_to_index
el = label_pair_to_edge
ei = index_pair_to_edge


# New states and edges ----------------------------------------------------


#' Adds a state to a markov_chain
#'
#' @param m markov_chain
#' @param label state's label
#' @param x x-coordinate (for plotting)
#' @param y x-coordinate (for plotting)
#' @param color (for plotting)
#'
#' @return markov_chain object
#' @export
#'
#' @examples
#' m = new_markov_chain()
#' m = add_state(m,"a", x=1, y=2, color = "reddish")
#' print(m)
add_state <- function(m,
                      label = NA,
                      x = NA,
                      y = NA,
                      color = NA,
                      shape = NA) {
  if (is.na(label))
    label = pkg.env$default.state.label(m)

  if (is.na(color))
    color = pkg.env$default.state.color

  if (is.na(shape))
    shape = pkg.env$default.state.shape

  m$states = tibble::add_row(
    m$states,
    index = length(m) + 1,
    label = label,
    x = x,
    y = y,
    color = color,
    shape = shape
  )
  m$layout = rbind(m$layout, c(x,y))
  return(m)
}

#' Adds a vector of states to a chain
#'
#' @param m markov_chain
#' @param states a vector of state labels
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' m = new_markov_chain()
#' m = add_states(m, c("a","b","c"), color = "yellow")
#' print(m)
add_states = function(m, states, ...) {
  for (s in states) {
    m = m %>%
      add_state(s, ...)
  }
  return(m)
}



#' Add an edge to the chain
#'
#' @param m markov_chain
#' @param from string label
#' @param to string label
#' @param prob numeric
#' @param label string
#' @param color string
#' @param curve numeric
#' @param loop_angle numeric
#'
#' @return markov_chain
#' @export
#'
#' @examples
#' m = new_markov_chain(number_of_states = 3)
#' m = add_edge_by_indices(m, 1, 2, prob=0.2)
#' print(m)
add_edge_by_indices <-
  function(m, from, to, prob = 1,
           label = NA, color = NA, curve = 0, loop_angle = 0) {
    check_index(m, from)
    check_index(m, to)
    if (is.na(label))
      label = pkg.env$default.edge.label(prob)

    if (is.na(color) & prob == 1)
      color = pkg.env$default.one.color

    if (is.na(color) & prob != 1)
      color = pkg.env$default.edge.color

    m$edges = add_row(
      m$edges,
      from = from,
      to = to,
      prob = prob,
      label = label,
      color = color,
      curve = curve,
      loop_angle = loop_angle
    )
    return(m)
  }

#' add edge, but from and to are string labels
#'
#' @param m markov_chain
#' @param from label
#' @param to label
#' @param ...
#'
#' @return markov_chain
#' @export
#'
#' @examples
#' m = new_markov_chain()
#' m = add_states(m, c("a", "b", "c"))
#' m = add_edge_by_labels(m, "b", "c", prob=0.4)
#' print(m)
add_edge_by_labels <- function(m, from, to, ...) {
   add_edge_by_indices(m, i(m,from), i(m,to), ...)
}

# Property Setters -----------------------------------------------------

#' Sets a property of a state by its index
#'
#' @param m markov_chain
#' @param index integer
#' @param ... named properties to be set
#'
#' @return markov_chain
#' @export
#'
#' @examples
#' m = new_markov_chain("Chain",2)
#' m = set_state_properties_by_index(m,1, color="Aquamarine")
#' print(m$states)
set_state_properties_by_index = function(m, index, ...) {
  properties <- list(...)
  if (!all(names(properties) %in% names(m$states)))
    stop("Trying to set a non-existing property.")
  m$states[index, names(properties)] = properties
  return(m)
}


#' Sets a property of a state by its label
#'
#' @param m markov_chain
#' @param label string
#' @param ... named properties to be set
#'
#' @return markov_chain
#' @export
#'
#' @examples
#' m = new_markov_chain("Chain")
#' m = add_states(m,c("a","b"))
#' m = set_state_properties_by_index(m,"a", color="Aquamarine")
#' print(m$states)
set_state_properties_by_label = function(m, label, ...) {
  set_state_properties_by_index(m, i(m,label),...)
}

#' Sets a property of an edge by its indices
#'
#' @param m
#' @param from index
#' @param to index
#' @param ... named properties to be set
#'
#' @return markov_chain
#' @export
#'
#' @examples
#' m = new_markov_chain("Chain",2)
#' m = add_edge_by_indices(m,1,2,prob=0.2)
#' m = set_edge_properties_by_indices(m,1,2, prob=0.3)
#' print(m$edges)
set_edge_properties_by_indices = function(m, from, to, ...) {
  properties <- list(...)
  if (!all(names(properties) %in% names(m$edges)))
    stop("Trying to set a non-existing property.")
  m$edges[ei(m, from, to), names(properties)] = properties
  return(m)
}

#' Sets a property of an edge by its labels
#'
#' @param m
#' @param from label
#' @param to label
#' @param ... named properties to be set
#'
#' @return markov_chain
#' @export
#'
#' @examples
#' m = new_markov_chain("Chain")
#' m = add_states(m, c("b", "a"))
#' m = add_edge_by_labels(m, "a", "b", prob=0.2)
#' m = set_edge_properties_by_indices(m, "a", "b", prob=0.3)
#' print(m$edges)
set_edge_properties_by_labels = function(m, from, to, ...) {
  properties <- list(...)
  if (!all(names(properties) %in% names(m$edges)))
    stop("Trying to set a non-existing property.")
  m$edges[el(m, from, to), names(properties)] = properties
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

