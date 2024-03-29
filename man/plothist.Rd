% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beginr.R
\name{plothist}
\alias{plothist}
\title{Plot a user-customized hist}
\usage{
plothist(data = rnorm(1000), mybreaks = "Sturges", myxlim = NULL,
  myylim = NULL, eightlines = TRUE, eightdigit = 0, eightcex = 0.8,
  eightcolors = c("red", "darkgreen", "blue", "black", "purple",
  "gold")[c(1, 2, 3, 2, 1, 6, 6, 5, 4, 5)], mylegend = "", myxlab = "",
  return_df = FALSE, show_n = TRUE, show_skewness = TRUE,
  show_density = FALSE, show_normline = FALSE, x)
}
\arguments{
\item{data}{a numeric vector}

\item{mybreaks}{character}

\item{myxlim}{numeric}

\item{myylim}{numeric}

\item{eightlines}{logical}

\item{eightdigit}{numeric}

\item{eightcex}{numeric}

\item{eightcolors}{colors}

\item{mylegend}{character}

\item{myxlab}{character}

\item{return_df}{logic}

\item{show_n}{logical}

\item{show_skewness}{logical}

\item{show_density}{logcial}

\item{show_normline}{logical}

\item{x}{a vector for plotting the curve}
}
\value{
a hist plot
}
\description{
Plot a user-customized hist
}
\examples{
plothist(rnorm(10000))
}
