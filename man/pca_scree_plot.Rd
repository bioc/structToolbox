% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PCA_plotfcns.R
\name{pca_scree_plot}
\alias{pca_scree_plot}
\title{Scree plot}
\usage{
pca_scree_plot(max_pc = 15, ...)
}
\arguments{
\item{max_pc}{(numeric, integer) The maximum number of components to include in the plot. The default is \code{15}.}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{
pca_scree_plot
} object. This object has no \code{output} slots.
See \code{\link[struct]{chart_plot}} in the \code{struct} package to plot this chart object.

struct object
}
\description{
A plot of the percent variance and cumulative percent variance for the components of a PCA model.
}
\examples{
C = pca_scree_plot()
}
