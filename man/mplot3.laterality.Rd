% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mplot3.laterality.R
\name{mplot3.laterality}
\alias{mplot3.laterality}
\title{Laterality scatter plot}
\usage{
mplot3.laterality(
  x,
  regionnames,
  main = NULL,
  ylab = "Left to Right",
  summary.fn = "median",
  summary.lty = 1,
  summary.lwd = 2.5,
  summary.col = NULL,
  arrowhead.length = 0.075,
  deltas = TRUE,
  line.col = theme$fg,
  line.alpha = 0.25,
  lty = 1,
  lwd = 0.3,
  ylim = NULL,
  theme = getOption("rt.theme", "lightgrid"),
  labelify = TRUE,
  autolabel = letters,
  mar = NULL,
  oma = rep(0, 4),
  pty = "m",
  palette = getOption("rt.palette", "rtCol1"),
  par.reset = TRUE,
  pdf.width = 6,
  pdf.height = 6,
  filename = NULL,
  ...
)
}
\arguments{
\item{x}{data.frame or data.table which includes columns with ROI names ending in "_L" or "_R"}

\item{regionnames}{Character, vector: Regions to plot. For example, if \code{regionnames}
contains "Ant_Insula", \code{x} must contain columns \code{Ant_Insula_L} and \code{Ant_Insula_R}}

\item{summary.fn}{Character: Name of function to summarize left and right values.
Default = "median"}

\item{summary.lty}{Integer: line type for summary arrows}

\item{summary.lwd}{Float: line width for summary arrows}

\item{summary.col}{Color for summary arrows}

\item{arrowhead.length}{Float: arrowhead length in inches. Default = .075}

\item{deltas}{Logical, If TRUE, show summary statistics. Default = TRUE}

\item{line.col}{Color for individual cases' lines}

\item{line.alpha}{Float: transparency for individual lines}

\item{lty}{Integer: Line type for individual lines. Default = 1}

\item{lwd}{Float: Line width for individual lines. Default = .3}

\item{ylim}{Float, vector, length 2: y-axis limits}
}
\description{
Laterality scatter plot
}
\author{
E.D. Gennatas
}
