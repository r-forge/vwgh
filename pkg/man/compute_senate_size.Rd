\name{compute_senate_size}

\alias{compute_senate_size}


\title{ Compute Senate Size }

\description{
 Extracts the senate size and writes it back to table senate\_size.
}

\usage{
compute_senate_size(con, limit = 0)
}

\arguments{
  \item{con}{ DBI connection object. }
  \item{limit}{ Limit the number of processed records (0 means no limit). }
}

\value{
 No value is returned.
}

\seealso{
 \code{\link{senate_size}}
}

\references{ Austrian law information system \url{http://www.ris.bka.gv.at/} }

\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }


\keyword{misc}