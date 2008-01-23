`compute_result` <-
function(con,limit=0)
	compute_from_db(con,result, "result", "Spruch", "Result", field_types="INTEGER", limit=limit)

