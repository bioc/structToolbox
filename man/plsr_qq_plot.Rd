% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PLSR_class.R
\name{plsr_qq_plot}
\alias{plsr_qq_plot}
\title{PLSR QQ plot}
\usage{
plsr_qq_plot(ycol = 1, ...)
}
\arguments{
\item{ycol}{(numeric, integer, character) The y-block column to plot. The default is \code{1}.\cr}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{
plsr_qq_plot
} object. This object has no \code{output} slots.
See \code{\link[struct]{chart_plot}} in the \code{struct} package to plot this chart object.
}
\description{
A plot of the quantiles of the residuals from a PLSR model against the quantiles of a normal distribution.
}
\section{Inheritance}{

A \code{plsr_qq_plot} object inherits the following \code{struct} classes: \cr\cr
\verb{[plsr_qq_plot]} >> \verb{[chart]} >> \verb{[struct_class]}
}

\examples{
M = plsr_qq_plot(
      ycol = 1)

C = plsr_qq_plot()
}
