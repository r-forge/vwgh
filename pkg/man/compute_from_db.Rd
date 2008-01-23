\name{compute_from_db}

\alias{compute_from_db}

\title{Compute data from Database}

\description{This function applys mining functions to the database, conveniently wrapping all database interactions.}

\usage{compute_from_db(con, func, table_name, fields_in, fields_out, field_types="TEXT",
		table_setup=setup_foreign_table, include_foreign_key=TRUE, limit=0)}

\arguments{
  \item{con}{The database connection.}
  \item{func}{The mining function. It is called with func(df), where df is a one-row data.frame with colums defined in fields\_in;
  should return a list with elements defined in fields\_out, all elements must have the same length (typically 1).}
  \item{table_name}{Name of the table to be set up in the DB.}
  \item{fields_in}{Fields from the db main table (Rechtsinformationssystem) to feed in the mining function (Note: SP\_Nr is always passed along).}
  \item{fields_out}{Fields to to create in the new table.}
  \item{field_types}{Format of the table cells. You may supply a vector of formats. Default = 'Text'}
  \item{table_setup}{Function called to create the table. Use this if you need custom table creation, or supply an empty function to create no table at all (The table table\_name has to be present in the database). See the \code{\link{setup_foreign_table}} on how this function is called.}
  \item{include_foreign_key}{Incude the foreign key (SP\_Nr) in the new table?}
  \item{limit}{Database record limit. 0 means no limit. }
}

\details{
  compute\_from\_db is a wrapper to the database created by build\_RIS\_db. Use this to easily create your own mining functions. 
  The function will call the miner once per database record with a single-row dataframe containing the fields in fields\_out and expects a list with fields\_in. This list is treated as a dateframe and written to the db, together with SP\_Nr unless you specified otherwise (see above). This means you can return more then one row per ruling.
  All database interaction is done by this function.
}
\author{ Martin Kober <martin.kober@wu-wien.ac.at>, Georg M. Zajko <georg.zajko@wu-wien.ac.at> }

\keyword{misc}
