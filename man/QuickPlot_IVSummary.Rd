% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QuickPlot.R
\name{QuickPlot_IVSummary}
\alias{QuickPlot_IVSummary}
\title{Quick plot an IV Summary table}
\usage{
QuickPlot_IVSummary(df_summary, title = NULL, legend_title = NULL,
  zero_intercept = TRUE, zero_axes = TRUE, line_size = 0.5,
  marker_size = line_size * 4, err_bar_size = line_size/1.5,
  err_bar_width = marker_size * 1.5)
}
\arguments{
\item{df_summary}{an IV summary table from IVSummary()}

\item{title}{OPTIONAL, title of the plot}

\item{legend_title}{OPTIONAL, if multiple IV summaries are to plot, title of the legend.}

\item{zero_intercept}{whether to add zero intercepts to the plot.}

\item{zero_axes}{whether to plot axes at 0.}

\item{line_size}{size of lines}

\item{marker_size}{size of markers}

\item{err_bar_size}{size of error bar}

\item{err_bar_width}{width of error bar}
}
\value{
a ggplot object.
}
\description{
Quick plot an IV Summary table
}
