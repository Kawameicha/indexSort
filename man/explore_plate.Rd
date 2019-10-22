\name{explore_plate}
\alias{explore_plate}
\title{Display median fluorescence and number of cells}
\description{
This function plots a plate visual from index sorted data.
}
\usage{
explore_plate(data, vars = c("IdxRow", "IdxCol"), cell = TRUE, dim = c(8, 12), ...)
}
\arguments{
\item{data}{A data frame of retrieved index sorted data}

\item{vars}{A list of quoted symbols matching columns in data.}

\item{cell}{Should cell number be displayed.}

\item{dim}{A list of two dimensions for the plate.}

\item{legend}{Should fluorescence legend be displayed.}

\item{circle}{Allows to change the wells' size}

\item{...}{Extra arguments, not used}
}
\examples{
explore_plate()
}
