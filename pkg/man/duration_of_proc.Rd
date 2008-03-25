\name{duration_of_proc}

\alias{duration_of_proc}


\title{ Duration of a process }

\description{
  It reads out the date of the rule of the last instance and the date of the VwGH decision, calculates the difference and subtracts 6 weeks.}

\usage{
duration_of_proc(data)
}


\arguments{
  \item{data}{ Single-row dataframe with the the columns "Entscheidungsdatum" and "Veroeffentlichungsdatum"}
}



\value{
  It returns the process duration as number of days.
}

\details{
	The start date of the process is determined by search for the most recent date in the 
	'Betreff' field, this gives good results for the sample dataset from Nagel and Mamut (2006) (note that the classification in the dataset is inconsistent for cases involving more than one incident, sometimes choosing the first incident as a reference date). 42 days are subtracted from the date difference, as appelants have six weeks to file their case to the VwGH.
}

\references{ Austrian law information system \url{http://www.ris.bka.gv.at/} 

Nagel, H. and Mamut, M. (2006): Rechtsprechung des VwGH in Abgabensachen 2000-
2004.}

\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }

\keyword{misc}

