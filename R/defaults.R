# Defaults ----------------------------------------------------------------

pkg.env <- new.env()

pkg.env$default.vertex.size <- 25
pkg.env$default.vertex.label.cex <- 1

pkg.env$default.layout.algorithm <- igraph::layout_with_kk

pkg.env$default.state.color <- "beige"
pkg.env$default.absorbing.color <- "orange"
pkg.env$default.state.shape = "circle"

pkg.env$default.edge.arrow.size = 0.5

pkg.env$default.edge.color <- "gray"
pkg.env$default.one.color <- "black"
pkg.env$default.continuous.edge.palette <- colorRampPalette(c("yellow", "darkorange4"))
pkg.env$default.discrete.edge.palette <- function(n) {
  if (n < 3)
    n = 3
  RColorBrewer::brewer.pal(n, "Set2")
}
pkg.env$default.curve <- 0.5

pkg.env$default.state.label = function(m) paste(length(m)+1)
pkg.env$default.edge.label = function(prob) {
  if (prob == 1)
    return("")
  else
    return(paste(prob))
}
