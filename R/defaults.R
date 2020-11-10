# Defaults ----------------------------------------------------------------

default = list(
  # states/vertices
  state.color = "beige",
  state.shape = "circle",
  state.x = NA,
  state.y = NA,
  state.label.function = function(m) paste(length(m)+1),
  vertex.size = 25,
  vertex.label.cex = 1,
  absorbing.state.color = "orange",
  # edges
  edge.arrow.size = 0.4,
  edge.color = "gray",
  edge.label.function = function(prob) ifelse(prob==1, "", paste(prob)),
  edge.curve = 0.5,
  edge.one.color = "black",
  edge.loop_angle = 0,
  # other
  continuous.edge.palette = colorRampPalette(c("yellow", "darkorange4")),
  discrete.edge.palette = function(n) RColorBrewer::brewer.pal(max(n,3), "Set2"),
  layout.algorithm = igraph::layout_with_kk
)
