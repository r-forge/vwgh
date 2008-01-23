`duration_of_proc` <-
function(data) {
	ent = as.character(data[["Entscheidungsdatum"]])
	voe = as.character(data[["Veroeffentlichungsdatum"]])
	dur = as.Date(voe, "%Y%m%d") - as.Date(ent, "%Y%m%d")
	return(list(duration_of_proceedings=(dur + 6*7)))
}

