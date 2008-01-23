\name{setup_foreign_table}

\alias{setup_foreign_table}


\title{ Setup Foreign Table }

\description{
  Creates table for mining results in db.
}


\usage{
setup_foreign_table(con, table_name, fields_out, field_types="TEXT", include_foreign_key=TRUE)
}


\arguments{
  \item{con}{ Database Connection }
  \item{table_name}{Table Name}
  \item{fields_out}{Fields to be created}
  \item{field_types}{Types}
  \item{include_foreign_key}{SP\_Nr?}  
}

\details{
  This function sets up tables in the db for mining results. To use a custom table setup function, specify a function that can be called in the same way as this function.
}

\references{ The Website of the Austrian Law \url{http://www.ris.bka.gv.at/} }

\author{ Martin Kober <mx.2000@gmail.com>, Georg Zweitler <georgz@inode.at>, Georg M. Zajko <georg\_zajko@gmx.at> }

\keyword{misc}

