`compute_affected_norms` <-
function(con, limit=0)
	compute_from_db(con, affected_norms, "affected_norms", "Norm", "Norm", limit=limit)

