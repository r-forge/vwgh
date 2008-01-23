\name{result}

\alias{result}

\title{ Result }

\description{
   Analyzes ruling of the court and writes it into table result.
}

\usage{
result(data)
}

\arguments{
  \item{data}{ Single-row dataframe with the column "Spruch" }
}


\value{
  Returns an numerical value:
 \item{0}{ negative ruling }
 \item{1}{ positive ruling }
 \item{2}{ otherwise (e.g. ruling deferred to other court) }
 \item{-1}{ no ruling found }
 \item{-2}{ ruling could not be categorized }
}


\references{ Austrian law information system \url{http://www.ris.bka.gv.at/} }

\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }


\keyword{misc}

