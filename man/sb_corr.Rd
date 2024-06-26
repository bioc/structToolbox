% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sb_corr.R
\name{sb_corr}
\alias{sb_corr}
\title{Signal/batch correction for mass spectrometry data}
\usage{
sb_corr(
  order_col,
  batch_col,
  qc_col,
  smooth = 0,
  use_log = TRUE,
  min_qc = 4,
  qc_label = "QC",
  spar_lim = c(-1.5, 1.5),
  ...
)
}
\arguments{
\item{order_col}{(character) The column name of sample_meta indicating the run order of the samples.}

\item{batch_col}{(character) The column name of sample_meta indicating the batch each sample was measured in.}

\item{qc_col}{(character) The column name of sample_meta indicating the group each sample is a member of.}

\item{smooth}{(numeric) The amount of smoothing applied (0 to 1). If set to 0 the smoothing parameter will be estimated using
leave-one-out cross-validation. The default is \code{0}.\cr}

\item{use_log}{(logical) Log tranformation. Allowed values are limited to the following: \itemize{ \item{\code{"TRUE"}: The data is log transformed prior to performing signal correction.}\item{\code{"FALSE"}: Signal correction is applied to the input data.}} The default is \code{TRUE}.\cr}

\item{min_qc}{(numeric) The minimum number of QC samples required for signal correction. The default is \code{4}.\cr}

\item{qc_label}{(character) The label used to identify QC samples. The default is \code{"QC"}.}

\item{spar_lim}{(numeric) A two element vector specifying the upper and lower limits when \code{spar = 0}. Allows the value of \code{spar} to be constrained within these limits to prevent overfitting. The default is \code{c(-1.5, 1.5)}.}

\item{...}{Additional slots and values passed to \code{struct_class}.}
}
\value{
A  \code{sb_corr} object with the following \code{output} slots:
\tabular{ll}{
\code{corrected} \tab          (DatasetExperiment) The DatasetExperiment after signal/batch correction has been applied. \cr
\code{fitted} \tab          (data.frame) The fitted splines for each feature. \cr
}

struct object
}
\description{
Applies Quality Control Robust Spline (QC-RSC) method to correct for signal drift and batch differences in mass spectrometry data.
}
\details{
This object makes use of functionality from the following packages:\itemize{  \item{\code{pmp}}}
}
\section{Inheritance}{

A \code{sb_corr} object inherits the following \code{struct} classes: \cr\cr
\verb{[sb_corr]} >> \verb{[model]} >> \verb{[struct_class]}
}

\examples{
M = sb_corr(
      order_col = character(0),
      batch_col = character(0),
      qc_col = character(0),
      smooth = 0,
      use_log = FALSE,
      min_qc = 4,
      qc_label = "QC",
      spar_lim = c(-1.5, 1.5))

M = sb_corr(order_col='run_order',batch_col='batch_no',qc_col='class')
}
\references{
Jankevics A, Lloyd GR, Weber RJM (????). \emph{pmp: Peak Matrix Processing
and signal batch correction for metabolomics datasets}. R package
version 1.15.1.

Kirwan JA, Broadhurst DI, Davidson RL, Viant MR (2013). "Characterising
and correcting batch variation in an automated direct infusion mass
spectrometry (DIMS) metabolomics workflow." \emph{Analytical and
Bioanalytical Chemistry}, \emph{405}(15), 5147-5157.
}
