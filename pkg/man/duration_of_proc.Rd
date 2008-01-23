\name{duration_of_proc}

\alias{duration_of_proc}


\title{ Duration of a process }

\description{
  It reads out the starting date and the date of the decision, calculates the difference and adds 6 weeks.}

\usage{
duration_of_proc(data)
}


\arguments{
  \item{data}{ Single-row dataframe with the the columns "Entscheidungsdatum" and "Veroeffentlichungsdatum"}
}



\value{
  It returns the process duration as number of days.
}

\references{ Austrian law information system \url{http://www.ris.bka.gv.at/} }

\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }

\keyword{misc}

