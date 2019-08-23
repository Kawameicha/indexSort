\name{retrieve_index}
\alias{retrieve_index}
\title{Retrieve Index Sorted Data for a Given Cell Sorter}
\description{
This function retrieves index sorted data from various cell sorters.
}
\usage{
retrieve_index(data, sorter = "aria", ...)
}
\arguments{
\item{data}{A Formal class flowFram. The input index sorted .fcs file should already be compensated.}

\item{sorter}{A character string that defines the cell sorter used.
Valid values are \code{aria} (default), \code{symphony}, \code{influx}, and j\code{azz}.}

\item{...}{Extra arguments, not used}
}
\examples{
retrieve_index()
}
