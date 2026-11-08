## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.retina=1, dpi=72, collapse=TRUE,comment='#>',fig.width=8,fig.height=5,warning=FALSE,message=FALSE)
available <- all(vapply(c('dgraphs','igraph','plotly'),requireNamespace,logical(1),quietly=TRUE))
available <- available && file.exists(system.file('examples','fermat-surfaces.R',package='dgraphs')) &&
  'return.graph' %in% names(formals(dgraphs::fermat.distances)) &&
  'distance.matrix' %in% names(formals(grip::metric.mds))
run.experiment <- identical(Sys.getenv('GRIP_RUN_FERMAT_EXPERIMENT'), 'true') ||
  nzchar(Sys.getenv('FERMAT_SURFACE_RESULTS'))
knitr::opts_chunk$set(eval=available && run.experiment)

## ----dependencies,echo=FALSE,eval=TRUE,results='asis'-------------------------
if(!available) cat('This executable example requires development versions of dgraphs and grip with the direct distance and supporting-graph APIs, plus igraph and plotly. Install these dependencies to render the calculated examples.\n')
if(available && !run.experiment) cat('The reference-geodesic calculations are an explicit reproduction step. Set GRIP_RUN_FERMAT_EXPERIMENT=true to run them, or FERMAT_SURFACE_RESULTS to a directory containing the saved results. Routine package builds show the code without rerunning this experiment.\n')

