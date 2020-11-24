# The plot method for markov_chain --------------------------------------------------

#' Sets graphics parameters of a markov_chain
#'
#' @param m a markov_chain object
#' @param ... the parameters to be set
#'
#' @export
set_graphics_parameters = function(m, ...) {
  m$graphics_parameters = utils::modifyList(m$graphics_parameters, list(...))
  return(m)
}


#' Colors edges according to their probabilities
#'
#' @param m a markov_chain object
#' @param discrete logical, distinguishable vs. in order
#' @param nbins if discrete, how many categories
#' @param ... additional arguments (palette)
#'
#' @export
set_auto_edge_colors = function (m, discrete = T, nbins = 3, ...) {

  nbins = min(12, max(1, as.integer(nbins)))

  # compute codes
  if (discrete) {
    if (nbins == 1) {
      codes = rep(1, nedges(m))
    } else  {
      codes = m$edges$prob %>%
        as.factor %>%
        as.integer
    }
  }

  if (!discrete)  {
    nbins = 1000
    codes = round(m$edges$prob * nbins)

  }

  # palette settings
  args = list(...)
  if ("palette" %in% names(args)) {
    the_palette_vector = args$palette(nbins)
  } else {
    if (discrete) {
      the_palette = default$discrete.edge.palette(nbins)
    } else {
      the_palette = default$continuous.edge.palette(1000)
    }
  }

  # set colors
  m$edges =
    m$edges %>%
    dplyr::mutate(color = ifelse(prob == 1, "black",
                          the_palette[codes]))
  m = m %>% set_graphics_parameters(edge.label = NA)

  return(m)

}

#' Colors states according to their classes
#'
#' @param m a markov_chain object
#' @param by_recurrence logical
#' @param by_class logical
#' @param ... additional arguments (palette)
#'
#' @export
set_auto_state_colors = function (m, by_recurrence = T, by_class=F, ...){
  if (!is_classified(m))
    m = m %>% classify
  print("classified")

# load_all(); m = tennis(); m = set_auto_state_colors(m); plot(m)
  nclasses = max(m$states$class)

  if (missing(by_class)){
    by_class = !by_recurrence
  } else {
    by_recurrence = !by_class
  }
  if (by_recurrence) {
    # palette settings
    args = list(...)

    transient_color = default$transient.color

    if ("recurrent" %in% names(args)) {
      recurrent_color = args$recurrent
    } else {
      recurrent_color = default$recurrent.color
    }

    if ("transient" %in% names(args)) {
      transient_color = args$transient
    } else {
      transient_color = default$transient.color
    }

    m$states =
      m$states %>%
      dplyr::mutate(color = ifelse(recurrent == TRUE,
                                   recurrent_color, transient_color))
  } else { # by class
    # palette settings
    args = list(...)
    if ("palette" %in% names(args)) {
      the_palette_vector = args$palette(nclasses)
    } else {
      the_palette_vector = default$class.palette(nclasses)
    }
    # set colors
    m$states =
      m$states %>%
      dplyr::mutate(color = the_palette_vector[class])
  }

  return(m)
}

#' Colors all absorbing states
#'
#' @param m a markov_chain object
#' @param color a color
#'
#' @return a markov_chain object
#' @export
set_absorbing_state_color <- function(m,
              color = default$absorbing.state.color) {

  if (!nedges(m))
    return(m)

  for (k in 1:nedges(m)) {
    prob = m$edges$prob[k]
    from = m$edges$from[k]
    to =   m$edges$to[k]
    if ( (from == to) & prob == 1)
      m$states$color[from] = color
  }
  return(m)

}

#' Plot method for a markov_chain object
#'
#' @param x the markov_chain object to be plotted
#' @param ... additional graphics parameters
#'
#' @export
plot.markov_chain <- function(x, ... ) {
  m=x # x is for consistency with plot
  from_function_call = list(...)
  from_graphics_parameters = m$graphics_parameters

  if (any(is.na(m$layout))) {
    m = m %>% set_auto_layout
  }
  from_graphics_parameters$layout = m$layout
  from_here  =
    list( edge.curved = m$edges$curve,
          vertex.color = m$states$color,
          vertex.label = m$states$label,
          vertex.shape = m$states$shape,
          edge.loop.angle = m$edges$loop_angle,
          vertex.size = default$vertex.size,
          vertex.label.cex = default$vertex.label.cex,
          edge.arrow.size = default$edge.arrow.size,
          edge.color = m$edges$color,
          rescale = FALSE,
          add.vertex.names = FALSE
    )

  g = igraph::graph_from_data_frame(m$edges,
                                    directed = TRUE,
                                    vertices = m$states)
  #limits
  mrg = 0.08
  x_range = range(m$layout[,1])
  dx = x_range[2]-x_range[1]
  x_range[2] = x_range[2] + mrg* dx
  x_range[1] = x_range[1] - mrg* dx

  y_range = range(m$layout[,2])
  dy = y_range[2]-y_range[1]
  y_range[2] = y_range[2] + mrg* dy
  y_range[1] = y_range[1] - mrg* dy

  from_here_lim = list(
    xlim = c(x_range[1], x_range[2]),
    ylim = c(y_range[1], y_range[2])
  )

  arg = list(x=g) %>%
    utils::modifyList(from_here) %>%
    utils::modifyList(from_here_lim) %>%
    utils::modifyList(from_graphics_parameters) %>%
    utils::modifyList(from_function_call)

  igraph::igraph_options(verbose = TRUE)

  do.call(igraph::plot.igraph, args=arg)
}


