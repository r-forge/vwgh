`duration_of_proc` <-
function(data) {
	correction_factor = 0 #-100 + 6*7 # ???

	end = as.Date(as.character(data[["Entscheidungsdatum"]]), "%Y%m%d")
	txt = data[["Betreff"]]
	
	month_list = c("Jänner"=1, Januar=1, Feber=2, Februar=2, "März"=3, April=4,
		Mai=5, Juni=6, Juli=7, August=8, September=9, Oktober=10, November=11, Dezember=12)
	month_pattern = paste(names(month_list), collapse="|");
	pattern = paste("\\d{1,2}\\.\\W{1,4}(", month_pattern, ")\\D{1,4}\\d{4}\\D", sep="")
	matches = gregexpr(pattern, txt, perl=TRUE)[[1]]
	
#	if (matches[1] == -1) {  
#		## No date found in 'Betreff', let's try 'Begruendung'
#		txt = data[["Begruendung"]]
#		matches = gregexpr(pattern, txt, perl=TRUE)[[1]]
#	}
	

	dates = c()
	for (i in 1:length(matches)) {
		d = substr(txt, matches[i], matches[i]+attr(matches, "match.length")[i]-1)
		d = sub("(\\d{1,2})\\.\\W{1,4}([A-Za-zäöüßÄÖÜ]+)\\D{1,4}(\\d{4})", "\\3-\\2-\\1", d, perl=TRUE)
		dd = strsplit(d, "-")[[1]]
		dd[2] = month_list[dd[2]]
		dates[i] = as.Date(paste(dd, collapse="-"), "%Y-%m-%d")
	}
	class(dates) = "Date"
	start = dates[1]
	dur = end - start
	
	return(list(duration_of_proc=(as.integer(dur) + correction_factor), 
		start=as.character(start), end=as.character(end)))
}

