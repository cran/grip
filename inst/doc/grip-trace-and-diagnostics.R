## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8.8,
  fig.height = 5.2
)

## -----------------------------------------------------------------------------
library(grip)

## -----------------------------------------------------------------------------
plot.trace.frame <- function(coords, edges, main = "", vertex.col = "black") {
  active <- stats::complete.cases(coords[, 1:2, drop = FALSE])
  xy <- coords[active, 1:2, drop = FALSE]

  xlim <- range(xy[, 1])
  ylim <- range(xy[, 2])
  xpad <- 0.08 * diff(xlim)
  ypad <- 0.08 * diff(ylim)
  if (!is.finite(xpad) || xpad == 0) xpad <- 0.2
  if (!is.finite(ypad) || ypad == 0) ypad <- 0.2

  plot(
    xy[, 1], xy[, 2],
    type = "n",
    asp = 1,
    axes = FALSE,
    xlab = "",
    ylab = "",
    xlim = xlim + c(-xpad, xpad),
    ylim = ylim + c(-ypad, ypad),
    main = main
  )

  active.edges <- edges[active[edges[, 1]] & active[edges[, 2]], , drop = FALSE]
  if (nrow(active.edges) > 0) {
    apply(active.edges, 1, function(e) {
      graphics::segments(
        coords[e[1], 1], coords[e[1], 2],
        coords[e[2], 1], coords[e[2], 2],
        col = "gray82"
      )
    })
  }

  points(xy[, 1], xy[, 2], pch = 16, cex = 0.55, col = vertex.col)
}

pick.trace.frames <- function(trace.obj) {
  n <- length(trace.obj$frames)
  unique(c(1L, max(2L, floor((n + 1L) / 2L)), n))
}

## -----------------------------------------------------------------------------
mesh.edges <- edges.mesh(5, 5)

mesh.trace <- trace.grip(
  mesh.edges,
  n = 25,
  dim = 2,
  preset = "mesh",
  rounds = 12,
  final_rounds = 16,
  trace = "level",
  diagnostics = "light",
  seed = 1
)

knitr::kable(mesh.trace$meta)
knitr::kable(mesh.trace$diagnostics[, c(
  "frame",
  "edge.length.cv",
  "median.edge.length",
  "sampled.nonedge.sep.ratio"
)], digits = 3)

## ----fig.width=10.5, fig.height=3.8-------------------------------------------
sel <- pick.trace.frames(mesh.trace)

op <- par(mfrow = c(1, length(sel)), mar = c(1.2, 1.2, 3, 1.2), bg = "white")
on.exit(par(op), add = TRUE)

for (idx in sel) {
  plot.trace.frame(
    mesh.trace$frames[[idx]],
    mesh.edges,
    main = paste(mesh.trace$meta$phase[[idx]], "(frame", idx, ")")
  )
}

## -----------------------------------------------------------------------------
mesh.trace.round <- trace.grip(
  mesh.edges,
  n = 25,
  dim = 2,
  preset = "mesh",
  rounds = 6,
  final_rounds = 8,
  trace = "round",
  trace.every = 4,
  diagnostics = "light",
  seed = 1
)

data.frame(
  trace_mode = c("level", "round"),
  n.frames = c(length(mesh.trace$frames), length(mesh.trace.round$frames))
)

## -----------------------------------------------------------------------------
weighted.mesh <- mesh.surface.graph(
  4, 4,
  surface = "saddle",
  amplitude = 0.8
)

weighted.trace <- trace.grip(metric = "edge_length",
  weighted.mesh$edges,
  n = weighted.mesh$n,
  edge_weights = weighted.mesh$edge_weights,
  dim = 3,
  preset = "mesh",
  rounds = 8,
  final_rounds = 12,
  trace = "level",
  diagnostics = "light",
  target_coords = weighted.mesh$coords_surface,
  seed = 2
)

knitr::kable(weighted.trace$meta)
knitr::kable(weighted.trace$diagnostics[, c(
  "frame",
  "edge.length.cv",
  "sampled.nonedge.sep.ratio",
  "procrustes.rmse"
)], digits = 3)

## ----fig.width=10.8, fig.height=3.8-------------------------------------------
sel.w <- pick.trace.frames(weighted.trace)

op <- par(mfrow = c(1, length(sel.w)), mar = c(1.2, 1.2, 3, 1.2), bg = "white")
on.exit(par(op), add = TRUE)

for (idx in sel.w) {
  frame.xy <- project.3d(weighted.trace$frames[[idx]], azimuth = 35, elevation = 20)
  plot.trace.frame(
    frame.xy,
    weighted.mesh$edges,
    main = paste(weighted.trace$meta$phase[[idx]], "(frame", idx, ")"),
    vertex.col = "#1F3B73"
  )
}

