
\name{compute_result}

\alias{compute_result}


\title{ Compute Result }

\description{
Extracts the result of the ruling rulings and stores it in the table result.
}

\usage{
compute_result(con, limit = 0)
}



\arguments{
  \item{con}{ DBI connection object. }
  \item{limit}{ Limit the number of processed records (0 means no limit). }
}

\value{
 No value is returned.
}

\seealso{
 \code{\link{result}}
}

\references{ Austrian law information system \url{http://www.ris.bka.gv.at/} }

\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }


\keyword{misc}