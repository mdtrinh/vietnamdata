#' vietnamdata: Data and Empirical Tools for Quantitative Political
#' Science Research on Vietnam
#'
#' The \code{vietnamdata} package provides several datasets that are useful for
#' political scientists interested in Vietnam, and convenient functions for
#' empirical analysis using these datasets
#'
#' The package contains four key datasets: planned national budget breakdowns by
#' provinces, realized national budget breakdowns by provinces, Provincial
#' Competitiveness Index (PCI) aggregate and component scores by provinces, and
#' Public Administration Performance Index (PAPI) aggregate and component scores
#' by provinces.
#'
#' It also provides convenient functions to perform Randomization Inference
#' implementation for some common empirical methods such as Linear Regression,
#' Weighted Fixed Effects Regression, and Synthetic Control. These functions are
#' often helpful for finite-sample problems that often characterize
#' provincial-level quantitative analyses of Vietnam.
#'
#' @section Datasets:
#' \itemize{
#'
#'   \item \code{VNbudget_plan} is a data frame containing planned national
#'   budget broken down by provinces for all Vietnamese provinces from 2006 to
#'   2016. Planned budgets are issued at the beginning of every year. It
#'   contains 46 variables, which include province and year IDs, total revenues,
#'   total expenditures, central transfers broken down by categories, as well as
#'   log transformations, lags and change values for the latter.
#'
#'   \item \code{VNbudget_final} is a data frame containing realized national
#'   budget broken down by provinces for all Vietnamese provinces from 2006 to
#'   2014. Realized budgets are calculated at the end of every year. It contains
#'   38 variables, which include province and year IDs, total revenues, total
#'   expenditures, central transfers broken down by categories, as well as log
#'   transformations, lags and change values for the latter.
#'
#'   \item \code{pci} is a data frame containing results from the Provincial
#'   Competitiveness Index (PCI) survey, aggregated at the province level for
#'   all Vietnamese provinces from 2005 to 2016. It contains 15 variables, which
#'   include province and year IDs, weighted and unweighted aggregate PCI
#'   scores, and component PCI scores.
#'
#'   \item \code{papi} is a data frame containing results from the Public
#'   Administration Performance Index (PAPI) survey, aggregated at the province
#'   level for all Vietnamese provinces from 2005 to 2016. It contains 15
#'   variables, which include province and year IDs, weighted and unweighted
#'   aggregate PAPI scores, and component PAPI scores.
#' }
#' @section Empirical tools:
#' \itemize{
#'
#'   \item \code{rireg} performs Randomization Inference for Ordinary Least Squares regressions
#'
#'   \item \code{riwfe} performs Randomization Inference for Weighted Fixed Effects regressions
#'
#'   \item \code{riSynth} performs Randomization Inference for the Synthetic Control method,
#'   with option for parallel computation
#'
#'   \item \code{plot.riFit} and \code{plot.riSynth} produces simple visualization for results
#'   from \code{rireg}, \code{riwfe}, and \code{riSynth}
#'
#'   \item \code{genperms} produces permutation matrices for an arbitrary vector with options for
#'   block and clustered randomization. Can permute non-binary vectors
#' }
#' @section Note:
#'   The functions in this package makes heavy use of existing functions from
#'   the \code{wfe}, \code{Synth}, and \code{permute} packages. All errors in
#'   implementation, howerver, are my own.
#'
#' @references
#'   Abadie, Alberto, Alexis Diamond, and Jens Hainmueller. "Synth: An R package
#'   for synthetic control methods in comparative case studies." (2011).
#'
#'   Kim, In Song, Kosuke Imai, and Maintainer In Song Kim. "Package ‘wfe’."
#'   (2014).
#'
#'   Knaus, Jochen. "snowfall: Easier cluster computing (based on snow)." (2010).
#'
#'   Simpson, Gavin L. "Restricted permutations; using the permute package." (2012).
#'
#'   Wickham, Hadley. ggplot2: elegant graphics for data analysis. Springer,
#'   2016.
#'
#' @docType package
#' @name vietnamdata
#' @importFrom wfe wfe
#' @importFrom Synth synth
#' @importFrom Synth dataprep
#' @importFrom vietnamcode vietnamcode
#' @importFrom stringdist amatch
#' @importFrom zoo na.locf
NULL
