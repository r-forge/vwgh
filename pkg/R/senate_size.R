`senate_size` <-
function(data) {
	txt = data[["Betreff"]]
	pos <- regexpr(paste("^.{20,45} (Vorsitzende|Vorsitzenden)[[:space:]]*"),
			## es gibt mittlerweile auch eine Senatspraesidentin
			txt)
	if (pos == -1) {
		if (regexpr("pr..?sident", txt) != -1) {
			## warning(paste("Unknown senate format:", data[["SP_Nr"]]))
			return(list(size=-1))
		}
		txt = ""
	}
	else {
		txt <- substring(txt, pos + attr(pos, "match.length"))
		txt <- sub("im[[:space:]]+Beisein.*", "", txt)
		txt <- sub("[[:space:]]*als[[:space:]]+Richter.*", "", txt)
	}
	x = gregexpr("(Dr|Mag)[. ]", txt)[[1]]
	if (x[1] == -1) r = list(0)   ## gregexpr workaround
	else r = list(length(x))
	names(r) = "size"
	return(r)
}

