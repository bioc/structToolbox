% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kfold_xval_charts.R
\name{kfoldxcv_grid}
\alias{kfoldxcv_grid}
\title{k-fold cross validation plot}
\usage{
kfoldxcv_grid(factor_name, level, ...)
}
\arguments{
\item{factor_name}{(character) The name of a sample-meta column to use.}

\item{level}{(character) The level/group to plot.}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{
kfoldxcv_grid
} object. This object has no \code{output} slots.
See \code{\link[struct]{chart_plot}} in the \code{struct} package to plot this chart object.
}
\description{
A graphic for visualising the true class and the predicted class of samples in all groups for all cross-validation folds.
}
\section{Inheritance}{

A \code{kfoldxcv_grid} object inherits the following \code{struct} classes: \cr\cr
\verb{[kfoldxcv_grid]} >> \verb{[chart]} >> \verb{[struct_class]}
}

\examples{
M = kfoldxcv_grid(
      factor_name = "V1",
      level = "level_1")

D = iris_DatasetExperiment()
I = kfold_xval(factor_name='Species') *
    (mean_centre() + PLSDA(factor_name='Species'))
I = run(I,D,balanced_accuracy())

C = kfoldxcv_grid(factor_name='Species',level='setosa')
chart_plot(C,I)

}
