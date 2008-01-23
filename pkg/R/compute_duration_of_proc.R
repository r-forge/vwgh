`compute_duration_of_proc` <-
function(con, limit=0)
	compute_from_db(con, duration_of_proc, "duration_of_proc", c("Entscheidungsdatum", "Veroeffentlichungsdatum"), 
		"duration_of_proceedings", "INTEGER", limit=limit)

