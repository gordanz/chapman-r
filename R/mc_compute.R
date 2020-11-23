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

classify = function(m) {
  g = igraph::graph_from_data_frame(m$edges,
                                    directed = TRUE,
                                    vertices = m$states)

  comps = igraph::components(g, mode="strong")
  m$states = m$states %>%
    add_column(class = comps$membership,
               recurrent = TRUE)
  class_rec = rep(TRUE, comps$no)

  for (k in 1:nedges(m)) {
    i = m$edges$from[k]
    j = m$edges$to[k]
    if ( m$states$class[i] != m$states$class[j] ) {
      print(paste(i," -> ", j))
      print(m$states$class[i])
      class_rec[m$states$class[i]] = FALSE
    }
  }
  for (i in 1:length(m)) {
    m$states$recurrent[i] = class_rec[m$states$class[i]]
  }

  return(m)
}


# load_all(); m = deck22(); g = graph_from_data_frame(m$edges, directed=T, vertices=m$states)
