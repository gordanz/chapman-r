# Transition matrix -------------------------------------------------------

#' Returns the transition matrix
#'
#' @param m
#' @import dplyr
#' @import tidyr
#'
#' @return
#' @export
#'
#' @examples
#' df = data_frame(
#'   from = c(1,2,2,3),
#'   to = c(2,1,3,1),
#'   prob = c(1, 0.3, 0.7, 1)
#' )
#' m = new_mc_from_data_frame(df)
#' print(transition_matrix(m))
transition_matrix <- function(m) {
  P = matrix( nrow=length(m), ncol=length(m))
  for (i in 1:length(m))
    for (j in 1:length(m)) {
      edge = ei(m,i,j)
      if (edge) {
        P[i,j] = as.double(m$edges[ei(m,i,j),]$prob)
      } else {
        P[i,j] = 0
      }
    }
  dimnames(P) = list(m$states$label, m$states$label)
  return(P)
}

