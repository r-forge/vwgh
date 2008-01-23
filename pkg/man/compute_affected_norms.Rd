\name{compute_affected_norms}

\alias{compute_affected_norms}


\title{ Compute Affected Norms }

\description{
Extracts the norms used in rulings and stores it in the table affected\_norms.
}

\usage{
compute_affected_norms(con, limit = 0)
}


\arguments{
  \item{con}{ DBI connection object. }
  \item{limit}{ Limit the number of processed records (0 means no limit). }
}

\value{
 No value is returned.
}

\seealso{
 \code{\link{affected_norms}}
}

\references{ Austrian law information system \url{http://www.ris.bka.gv.at/} }

\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }


\keyword{misc}

