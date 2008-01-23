`setup_foreign_table` <-
function(con, table_name, fields_out, field_types="TEXT", include_foreign_key=TRUE) {
	rs <- dbGetQuery(con, paste("DROP TABLE IF EXISTS", table_name))
	cols = paste(fields_out, field_types, collapse=", ")
	if (include_foreign_key) cols = paste("SP_Nr INTEGER,", cols)
	rs <- dbGetQuery(con, paste("CREATE TABLE", table_name, "(", cols, ")"))
}

