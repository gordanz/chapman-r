# The print method for markov_chain objects -------------------------------------------------

output_states = function(m) {
  out = paste0("States(", length(m), ")")
  if (length(m)>0) {
    out = paste0(out, ": ")
    for (i in 1:length(m)) {
      out = paste0(out, m$states$label[i])
      if (i < length(m))
        out = paste0(out, ", ")
    }
  }
  return(out)
}

output_edges = function(m) {
  out = paste0("Edges(", nedges(m), ")")
  if (nedges(m)>0) {
    out = paste0(out, ": ")
    for (i in 1:nedges(m)) {
      out = paste0(out,
                   l(m, m$edges$from[i]),
                   "->",
                   l(m, m$edges$to[i]),
                   "(",
                   format(m$edges$prob[i], digits = 2),
                   ")"
                   )
      if (i < nedges(m))
        out = paste0(out, ", ")
    }
  }
  return(out)
}

#' The print method for the markov_chain class
#'
#' @param m a markov_chain object
#' @export
#'
#' @examples
print.markov_chain <- function(x) {
  m=x # x is for consistency with print.
  print(paste("Name:", m$name))
  print(output_states(m))
  print(output_edges(m))
}

# The plot method for markov_chain --------------------------------------------------

#' Rotate the layout of a markov_chain
#'
#' @param m
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
#' @param algorithm
#'
#' @return
#' @export
#'
#' @examples
set_fancy_layout = function(m, algorithm = pkg.env$default.layout.algorithm) {
  g = igraph::graph_from_data_frame(m$edges, directed = TRUE, vertices = m$states)
  m$layout = algorithm(g) %>%
    scale
  return(m)
}

#' Sets graphics parameters of a markov_chain
#'
#' @param m
#' @param ... the parameters to be set
#'
#' @return
#' @export
#'
#' @examples
#' m = random_markov_chain(3)
#' plot(m)
#' m = set_graphics_parameters(edge.label = NA)
#' plot(m)
set_graphics_parameters = function(m, ...) {
  m$graphics_parameters = c(m$graphics_parameters, ...)
  return(m)
}


#' Colors edges according to their probabilities
#'
#' @param m
#' @param discrete logical, distonguishable vs. in order
#' @param nbins if discrete, how many categories
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' m = color_tester_markov_chain(15)
#' m = set_fancy_edge_colors(m)
#' plot(m)
set_fancy_edge_colors = function (m,
                                  discrete = T,
                                  nbins = 3,
                                  ...) {

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
  # m$edges$codes = codes

  # palette settings
  args = list(...)
  if ("palette" %in% names(args)) {
    the_palette_vector = args$palette(nbins)
  } else {
    if (discrete) {
      the_palette = pkg.env$default.discrete.edge.palette(nbins)
    } else {
      the_palette = pkg.env$default.continuous.edge.palette(1000)
    }
  }

  # set colors
  m$edges =
    m$edges %>%
    mutate(color = ifelse(prob == 1, "black",
                          the_palette[codes]))
  m = m %>% set_graphics_parameters(edge.label = NA)

  return(m)

}

set_absorbing_state_color <- function(m,
              color = pkg.env$default.absorbing.color) {

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
#' @param m
#'
#' @return
#' @export
#'
#' @examples
plot.markov_chain <- function(x, ... ) {
  m=x # x is for consistency with plot
  from_function_call = list(...)
  from_graphics_parameters = m$graphics_parameters

  if (any(is.na(m$layout))) {
    m = m %>% set_fancy_layout
  }
  from_graphics_parameters$layout = m$layout
  from_here  =
    list( edge.curved = m$edges$curve,
          vertex.color = m$states$color,
          vertex.label = m$states$label,
          vertex.shape = m$states$shape,
          edge.loop.angle = m$edges$loop_angle,
          vertex.size = pkg.env$default.vertex.size,
          vertex.label.cex = pkg.env$default.vertex.label.cex,
          edge.arrow.size = pkg.env$default.edge.arrow.size,
          edge.color = m$edges$color,
          rescale = FALSE,
          add.vertex.names = FALSE
    )

  g = igraph::graph_from_data_frame(m$edges,
                                    directed = TRUE,
                                    vertices = m$states)
  arguments=c(
    list(x=g),
    from_function_call,
    from_graphics_parameters,
    from_here)
  igraph::igraph_options(verbose = TRUE)

  do.call(igraph::plot.igraph, args=arguments)
}


