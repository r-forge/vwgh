\name{read_RIS_jurisdiction}
\alias{read_RIS_jurisdiction}
\title{Internal RIS function, extracts information from web page}
\description{
  Extracts information out of VwGH judgements published on RIS. Used by build\_RIS\_db.}
\usage{
	read_RIS_jurisdiction(number)
}
\arguments{
  \item{      number}{ ID number of the document to load. }
    
}
\details{
  The function downloads the pages from http://www.ris.bka.gv.at and extracts all the 
  data out of it. 
}
\value{
  Returns a named character vector with the extracted fields.
}
\references{
  RIS \url{http://www.ris.bka.gv.at}.
}

\author{ Kurt Hornik <kurt.hornik@wu-wien.ac.at>, Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }

\note{ The fields may be very long, i.e. serveral tens of thousands of characters.
  Do not use \code{dump()} to save these strings as \code{source()} cannot load long lines. Use \code{save}/\code{load} instead.
}
\examples{
\dontrun{
x = read_RIS_jurisdiction(10000)
x[["Geschaeftszahl"]]
}}
\keyword{ misc }
