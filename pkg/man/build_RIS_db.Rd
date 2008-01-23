\name{build_RIS_db}
\alias{build_RIS_db}
\title{Function to create and fill a database}
\description{
  Populates a database with entries from the online RIS (Austria).
}
\usage{
	build_RIS_db(con, from=1, to=80000, new_table=TRUE) 
}
\arguments{
  \item{      con}{ DBI connection object. }
  \item{     from}{ Lower bound for the download process. The number used is the 
			same one as the identifier number on the homepage. }
  \item{       to}{ Upper bound for the download process. }
  \item{new_table}{ When new\_table is set, the script (re-)creates the necessary 
			database table, dropping any previously defined table of the same name. 
			Default is TRUE.}
}
\details{
  This function loads the RIS data from the RIS webpage into a (possibly local) mySQl database. To
  load all data, simply connect to the database and call build\_RIS\_db(con). It is also 
  possible to downlaod smaller parts of the full db by using the to and from parameters.

  The URL used for acccessing the rulings is \url{http://ris.bka.gv.at/taweb-cgi/taweb?x=d&o=d&d=VwGHT&i=\%d&v=vwgh} (replace \%d with the id of the ruling).
}
\value{
  build\_RIS\_db returns nothing on success. 
}

\references{ Austrian law information system \url{http://www.ris.bka.gv.at/} }

\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }

\note{ Make sure to set the encoding of your database connection to the encoding of your R locale, i.e. 
"latin1" or "UTF-8".
}

\examples{
\dontrun{
con = dbConnect(...)
build_RIS_db(con)
}
}
\keyword{ misc }
