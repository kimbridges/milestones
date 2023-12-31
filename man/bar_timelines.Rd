% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{bar_timelines}
\alias{bar_timelines}
\title{bar_timelines
Builds charts using "ggplot2" with bars that span time
periods.}
\usage{
bar_timelines(datatable, styles = column, key_color_table = NULL)
}
\arguments{
\item{datatable}{A table with event, start, and end columns}

\item{styles}{A data frame with the style parameters}

\item{key_color_table}{A table linking events to colors}
}
\value{
A "ggplot2" object ready for plotting
}
\description{
bar_timelines
Builds charts using "ggplot2" with bars that span time
periods.
}
\examples{
bar_timelines(data=data,styles=column)
}
