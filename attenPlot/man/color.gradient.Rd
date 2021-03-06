% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/color_gradient.R
\name{color.gradient}
\alias{color.gradient}
\title{Color ramp function.}
\usage{
color.gradient(vec, pal = "GnBu", shade = "99", monthly = F, min = NA,
  max = NA)
}
\arguments{
\item{pal}{Requires a character object designating the color pallet to apply. Here we require palletes provided by RColorBrewer and default ot a Green Blue gradient pallet.}

\item{shade}{Requires a character object between 0 and 100. The lower the value the greater the transparency. Default is "99".}

\item{df}{Requires a vector of data.}
}
\description{
This function allows you to create a scaled color ramp for a vector of data.
}
\examples{
color.gradient(vec, pal = "GnBu", shade = "99")
}
\keyword{color}
