% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PlotChannel.R
\name{PeekAllChannel}
\alias{PeekAllChannel}
\title{Fastp lot all channels of an abf object.}
\usage{
PeekAllChannel(abf, intv = NULL, cursor = NULL, colour = FALSE,
  time_unit = "tick", auto_zoom = FALSE, title = NULL,
  arrange = "H", ratio = 50L, ...)
}
\arguments{
\item{abf}{an abf object.}

\item{intv}{OPTIONAL, an interval (a vector of c(start, end, len)) to plot on top.}

\item{cursor}{OPTIONAL, cursors (a vector of positions) to plot on top.}

\item{colour}{whether to plot in coloured mode.}

\item{time_unit}{time unit for x axis.}

\item{auto_zoom}{whether to zoom in the plot automatically.}

\item{title}{OPTIONAL, title of the plot.}

\item{arrange}{arrangement of the subplots, can be "H" (horizontal), "V" (vertical)}

\item{ratio}{OPTIONAL, sampling ratio}

\item{...}{other arguments passed to melt, see melt.abf for more details.}
}
\value{
a ggplot object.
}
\description{
Fastp lot all channels of an abf object.
}
