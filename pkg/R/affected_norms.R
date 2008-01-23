`affected_norms` <-
function(data) {
	norm <- strsplit(data[["Norm"]],"; *", perl=TRUE)[[1]]
	return(list(Norm=norm))
}

