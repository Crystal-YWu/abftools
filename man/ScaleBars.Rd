% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_components.R
\name{ScaleBars}
\alias{ScaleBars}
\alias{ApplyScaleBars}
\title{Draw scale bars}
\usage{
ScaleBars(position = c("bl", "br", "tl", "tr"), xrange, yrange,
  xscale = NULL, yscale = NULL, xunit = "", yunit = "",
  label_format = "\%g \%s", basesize = 5, linesize = basesize/4)

ApplyScaleBars(p, position = c("bl", "br", "tl", "tr"), xscale = NULL,
  yscale = NULL, xunit = "", yunit = "", label_format = "\%g \%s",
  basesize = 5, linesize = basesize/4)
}
\arguments{
\item{position}{Position to draw scale bars.}

\item{xrange}{Range of x axis.}

\item{yrange}{Range of y axis.}

\item{xscale}{Size of x scale. If is NULL, 1/20 of xrange.}

\item{yscale}{Size of y scale. If is NULL, 1/20 of yrange.}

\item{xunit}{Label of x scale bar, if xscale is NULL, used as unit of x instead.}

\item{yunit}{Label of y scale bar, if yscale is NULL, used as unit of x instead.}

\item{label_format}{Format string for labeling.}

\item{basesize}{Base textsize.}

\item{linesize}{Line size.}

\item{p}{a ggplot object.}
}
\value{
a list of ggplot objects.
}
\description{
Draw scale bars
}
