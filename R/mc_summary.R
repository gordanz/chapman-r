# The summary method for markov_chain objects -------------------------------------------------

#' The summary method for the markov_chain class
#'
#' @param m a markov_chain object
#' @export
summary.markov_chain <- function(object, ...) {
  m = object
  print(output_states(m))
  print(output_edges(m))
}

#' The print method for the markov_chain class. For now the same as summary.
#'
#' @param m a markov_chain object
#' @export
print.markov_chain <- function(x, ...) {
  m=x
  print(output_states(m))
  print(output_edges(m))
}

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

#' Output the transition matrix in a copy-paste form
#'
#' @param m a markov_chain object
#'
#' @return a string
#' @export
P_dput = function (m) {
  n = length(m)
  P = transition_matrix(m)
  out = "P = matrix(c("
  for (i in 1:n) {
    out = paste(out,"\n  ")
    for (j in 1:n) {
      out=paste0(out, P[i,j])
      if (i<n || j < n)
        out = paste0(out,", ")
    }
  }
  out = paste(out,"),\n byrow=T, ncol =",
              n,")\n\n S=")

  a= paste(utils::capture.output(dput(m$states$label)),
           collapse="")
  out = paste(out,a)



}
