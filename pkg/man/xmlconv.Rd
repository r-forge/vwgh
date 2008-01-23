
\name{xmlconv}
\alias{xmlconv}
\title{Internal RIS helper function. Converts XML objects to character strings.}
\description{
  Takes an XML object list and converts it to a character vector.}
\usage{
	xmlconv(x)
}
\arguments{
  \item{        x}{ A list of XML objects genreated by the XML package. }
    
}
\value{
  Returns a character vector containing the text values of the XMl objects. xmlconv uses \code{xmlvalue()} internally.
}
\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }
\examples{
\dontrun{
library("XML")
x = url("
}
}
\keyword{ misc }