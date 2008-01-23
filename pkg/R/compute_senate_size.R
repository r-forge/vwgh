`compute_senate_size` <-
function(con, limit=0) 
	compute_from_db(con, senate_size, table_name="senate_size", 
		fields_in="Betreff", fields_out="size", field_types="INTEGER", limit=limit)

