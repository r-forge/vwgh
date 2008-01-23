\name{compute_duration_of_proc}

\alias{compute_duration_of_proc}


\title{ Compute Duration of Proceedings }
\description{
Extracts the time of the proceeding and writes it back to the table duration\_of\_proc.
}

\usage{
compute_duration_of_proc(con, limit = 0)
}


\arguments{
  \item{con}{ DBI connection object. }
  \item{limit}{ Limit the number of processed records (0 means no limit). }
}

\value{
 No value is returned.
}

\seealso{
 \code{\link{duration_of_proc}}
}

\references{ Austrian law information system \url{http://www.ris.bka.gv.at/} }

\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }


\keyword{misc}
