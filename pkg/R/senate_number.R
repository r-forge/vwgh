`senate_number` <-
function(data) {
	nr = unlist(strsplit(data[["Geschaeftszahl"]],"/"))[2]
	list(senate_nr=nr)
}

