% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataset_chart_classes.R
\name{DatasetExperiment_dist}
\alias{DatasetExperiment_dist}
\title{Feature distribution histogram}
\usage{
DatasetExperiment_dist(factor_name, per_class = TRUE, ...)
}
\arguments{
\item{factor_name}{(character) The name of a sample-meta column to use.}

\item{per_class}{(logical) Plot per class. Allowed values are limited to the following: \itemize{ \item{\code{"TRUE"}: The distributions are plotted for each class.}\item{\code{"FALSE"}: The distribution is plotted for all samples.}} The default is \code{TRUE}.\cr}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{
DatasetExperiment_dist
} object. This object has no \code{output} slots.
See \code{\link[struct]{chart_plot}} in the \code{struct} package to plot this chart object.
}
\description{
A histogram to visualise the distribution of values within features.
}
\section{Inheritance}{

A \code{DatasetExperiment_dist} object inherits the following \code{struct} classes: \cr\cr
\verb{[DatasetExperiment_dist]} >> \verb{[chart]} >> \verb{[struct_class]}
}

\examples{
M = DatasetExperiment_dist(
      factor_name = "V1",
      per_class = FALSE)

D = MTBLS79_DatasetExperiment()
C = DatasetExperiment_dist(factor_name='Class')
chart_plot(C,D)
}
