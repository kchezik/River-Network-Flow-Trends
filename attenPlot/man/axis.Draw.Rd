% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/axis_Draw.R
\name{axis.Draw}
\alias{axis.Draw}
\title{Draw x and y axis.}
\usage{
axis.Draw(LW, ALS, ALY, LL, YLim, YLab, xAxis = F, ALabS = ALS)
}
\arguments{
\item{LW}{Requires a numeric that defines the line width.}

\item{ALS}{Requires a numeric that defines the axis line size.}

\item{ALY}{Requires a character object that defines the y-axis label.}

\item{LL}{Requires a numeric that defines the margin line where the x-axis label will be drawn.}

\item{YLim}{Requires a numeric vector for the location of tick mark labels. These values are output from axis.Fun().}

\item{YLab}{Require a character vector fo the labels of tick mark labels. These values are output from axis.Fun().}

\item{xAxis}{Requires a boolean T/F and determines whether the x-axis is drawn. xAxis defaults to False.}
}
\description{
This function allows you to draw the x and y axis'.
}
\examples{
axis.Draw(LW = 1, ALS = 1, ALY = "Y-Axis", LL = 1, YLim = ylimits, YLab = labels, xAxis = T)
}
\keyword{axis'}
\keyword{draw}
