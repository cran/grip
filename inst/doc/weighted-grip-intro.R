## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8.8,
  fig.height = 5.3
)

## -----------------------------------------------------------------------------
library(grip)

## -----------------------------------------------------------------------------
plot.layout.triptych <- function(coords.list,
                                 edges,
                                 titles,
                                 projection = NULL,
                                 vertex.cols = rep("black", length(coords.list)),
                                 edge.col = "gray82") {
  op <- par(
    mfrow = c(1, length(coords.list)),
    mar = c(1.2, 1.2, 3, 1.2),
    bg = "white"
  )
  on.exit(par(op), add = TRUE)

  for (i in seq_along(coords.list)) {
    plot.layout(
      coords.list[[i]], edges,
      projection = projection,
      main = titles[[i]],
      vertex.col = vertex.cols[[i]],
      edge.col = edge.col
    )
  }
}

## -----------------------------------------------------------------------------
surface.mesh <- mesh.surface.graph(
  5, 5,
  surface = "saddle",
  amplitude = 0.9
)

coords.unweighted <- grip(
  surface.mesh$edges,
  n = surface.mesh$n,
  dim = 3,
  preset = "mesh",
  seed = 1
)

coords.weighted <- weighted.grip(
  surface.mesh$edges,
  n = surface.mesh$n,
  edge_weights = surface.mesh$edge_weights,
  dim = 3,
  preset = "mesh",
  seed = 1
)

gkk.prepared <- prepare.geodesic.kk(
  surface.mesh$edges,
  n = surface.mesh$n,
  edge_weights = surface.mesh$edge_weights
)

surface.summary <- do.call(
  rbind,
  list(
    cbind(
      method = "Combinatorial GRIP",
      score.geodesic.kk(
        coords.unweighted,
        prepared = gkk.prepared
      )[, c(
        "gkk.weighted.rmse",
        "gkk.mean.abs.path.error",
        "gkk.mean.rel.path.error"
      )]
    ),
    cbind(
      method = "Weighted GRIP",
      score.geodesic.kk(
        coords.weighted,
        prepared = gkk.prepared
      )[, c(
        "gkk.weighted.rmse",
        "gkk.mean.abs.path.error",
        "gkk.mean.rel.path.error"
      )]
    )
  )
)

knitr::kable(surface.summary, digits = 3)

## ----fig.width=12.8, fig.height=4.2-------------------------------------------
plot.layout.triptych(
  list(
    surface.mesh$coords_surface,
    coords.unweighted,
    coords.weighted
  ),
  edges = surface.mesh$edges,
  titles = c("Target geometry", "Combinatorial GRIP", "Weighted GRIP"),
  projection = "ortho",
  vertex.cols = c("#666666", "black", "#1F3B73")
)

## -----------------------------------------------------------------------------
coords.weighted.2d <- weighted.grip(
  surface.mesh$edges,
  n = surface.mesh$n,
  edge_weights = surface.mesh$edge_weights,
  dim = 2,
  preset = "mesh",
  seed = 2
)

coords.weighted.3d <- weighted.grip(
  surface.mesh$edges,
  n = surface.mesh$n,
  edge_weights = surface.mesh$edge_weights,
  dim = 3,
  preset = "mesh",
  seed = 2
)

dim.summary <- do.call(
  rbind,
  list(
    cbind(
      dim = "2D",
      score.geodesic.kk(
        coords.weighted.2d,
        prepared = gkk.prepared
      )[, c(
        "gkk.weighted.rmse",
        "gkk.mean.abs.path.error",
        "gkk.mean.rel.path.error"
      )]
    ),
    cbind(
      dim = "3D",
      score.geodesic.kk(
        coords.weighted.3d,
        prepared = gkk.prepared
      )[, c(
        "gkk.weighted.rmse",
        "gkk.mean.abs.path.error",
        "gkk.mean.rel.path.error"
      )]
    )
  )
)

knitr::kable(dim.summary, digits = 3)

## ----fig.width=10.2, fig.height=4.3-------------------------------------------
op <- par(mfrow = c(1, 2), mar = c(1.2, 1.2, 3, 1.2), bg = "white")
on.exit(par(op), add = TRUE)

plot.layout(
  coords.weighted.2d,
  surface.mesh$edges,
  main = "Weighted GRIP in 2D",
  vertex.col = "black",
  edge.col = "gray82"
)

plot.layout(
  coords.weighted.3d,
  surface.mesh$edges,
  projection = "ortho",
  main = "Weighted GRIP in 3D",
  vertex.col = "#1F3B73",
  edge.col = "gray82"
)

## -----------------------------------------------------------------------------
tree.graph <- kary.tree.weighted.graph(
  k = 2,
  depth = 4,
  depth_rule = "geometric",
  depth_decay = 0.82,
  branch_rule = "linear",
  branch_spread = 0.25
)

tree.coords <- weighted.grip(
  tree.graph$edges,
  n = tree.graph$n,
  edge_weights = tree.graph$edge_weights,
  dim = 2,
  preset = "tree",
  seed = 3
)

knitr::kable(
  head(tree.graph$edge_table[, c(
    "parent",
    "child",
    "child_depth",
    "branch_index",
    "edge_weight"
  )]),
  digits = 3
)

## ----fig.width=5.4, fig.height=4.6--------------------------------------------
plot.layout(
  tree.coords,
  tree.graph$edges,
  main = "Intrinsic weighted tree",
  vertex.col = "#1F3B73",
  edge.col = "gray80",
  pch = 16,
  cex = 0.55
)

## ----eval=FALSE---------------------------------------------------------------
# surface.trace <- trace.weighted.grip(
#   surface.mesh$edges,
#   n = surface.mesh$n,
#   edge_weights = surface.mesh$edge_weights,
#   dim = 3,
#   preset = "mesh",
#   trace = "level",
#   diagnostics = "light",
#   seed = 1
# )
# 
# head(surface.trace$meta)
# head(surface.trace$diagnostics)

