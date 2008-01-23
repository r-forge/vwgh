`xmlconv` <-
function(x) {
	## character set from XML always seems to be UTF-8, convert to latin1
	a = sapply(x, xmlValue)
	Encoding(a) <- "UTF-8"
	return(a)
	#sapply(x, xmlValue)
}

