`compute_senate_number` <-
function(con, limit=0)
	compute_from_db(con, senate_number, "senate_number", "Geschaeftszahl",
		"senate_nr", "INTEGER", limit=limit)

