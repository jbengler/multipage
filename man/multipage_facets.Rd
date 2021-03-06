% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multipage.R
\name{multipage_facets}
\alias{multipage_facets}
\title{multipage_facets}
\usage{
multipage_facets(
  gg,
  facet_by,
  ncol = NULL,
  nrow = NULL,
  byrow = NULL,
  widths = NULL,
  heights = NULL,
  guides = "collect",
  tag_level = NULL,
  design = NULL
)
}
\arguments{
\item{gg}{A \code{ggplot}}

\item{facet_by}{Variable that should be used for faceting.}

\item{ncol, nrow}{The number of columns and rows per page.}

\item{byrow}{Analogous to \code{byrow} in \link[base:matrix]{matrix()}. If \code{FALSE} the
plots will be filled in in column-major order}

\item{widths}{The relative widths and heights of each column and row
in the grid. Will get repeated to match the dimensions of the grid.}

\item{heights}{The relative widths and heights of each column and row
in the grid. Will get repeated to match the dimensions of the grid.}

\item{guides}{A string specifying how guides should be treated in the layout.
\code{'collect'} will collect guides below to the given nesting level, removing
duplicates. \code{'keep'} will stop collection at this level and let guides be
placed alongside their plot. \code{auto} will allow guides to be collected if a
upper level tries, but place them alongside the plot if not.}

\item{tag_level}{A string (\code{'keep'} or \code{'new'}) to indicate how
auto-tagging should behave. See \code{\link[patchwork:plot_annotation]{plot_annotation()}}.}

\item{design}{Specification of the location of areas in the layout. Can either
be specified as a text string or by concatenating calls to \code{\link[patchwork:area]{area()}} together.
See the examples for further information on use.}
}
\description{
multipage_facets
}
