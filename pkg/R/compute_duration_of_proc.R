`compute_duration_of_proc` <-
function(con, limit=0)
	compute_from_db(con, duration_of_proc, "duration_of_proc", c("Entscheidungsdatum", "Betreff", "Begruendung"), 
		c("duration_of_proc", "start", "end"), c("INTEGER", "DATE", "DATE"), limit=limit)

