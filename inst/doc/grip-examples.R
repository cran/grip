## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8.5,
  fig.height = 5.2
)

## -----------------------------------------------------------------------------
library(grip)

## -----------------------------------------------------------------------------
mesh.edges <- edges.mesh(5, 5)
mesh.coords <- grip(
  mesh.edges,
  n = 25,
  dim = 2,
  preset = "mesh",
  seed = 1
)

mesh.score <- score.layout(mesh.coords, edges = mesh.edges, n = 25)
knitr::kable(mesh.score[, c(
  "sampled.stress",
  "edge.length.cv",
  "sampled.nonedge.sep.ratio"
)], digits = 3)

## ----fig.width=5.2, fig.height=4.6--------------------------------------------
plot.layout(
  mesh.coords,
  mesh.edges,
  main = "grip() on a 5x5 mesh",
  pch = 16,
  cex = 0.65,
  edge.col = "gray82"
)

## -----------------------------------------------------------------------------
mesh.cmp <- compare.layouts(
  edges = mesh.edges,
  n = 25,
  dim = 2,
  candidates = c("default", "mesh", "tree"),
  seeds = 1:2,
  sample.size.stress = 500L,
  sample.size.nonedge = 1000L,
  edge.crossings = "never"
)

knitr::kable(mesh.cmp$summary[, c(
  "candidate",
  "sampled.stress.mean",
  "edge.length.cv.mean",
  "sampled.nonedge.sep.ratio.mean",
  "score.composite"
)], digits = 3)

