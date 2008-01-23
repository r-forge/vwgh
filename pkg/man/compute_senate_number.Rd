\name{compute_senate_number}

\alias{compute_senate_number}

\title{ Computer Senate Number }

\description{
 Extracts the identifiction number of the senate the ruled over the case and writes it back in the table senate\_number.
}

\usage{
compute_senate_number(con, limit = 0)
}


\arguments{
  \item{con}{ DBI connection object. }
  \item{limit}{ Limit the number of processed records (0 means no limit). }
}

\value{
 No value is returned.
}

\seealso{
 \code{\link{senate_number}}
}

\references{ Austrian law information system \url{http://www.ris.bka.gv.at/} }

\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }


\keyword{misc}