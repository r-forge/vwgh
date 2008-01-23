`put_RIS_jurisdictions_into_data_frame` <-
function(out)
{
	dbcols = c('Norm', 'Gerichtstyp', 'Geschaeftszahl', 'Entscheidungsdatum', 'Veroeffentlichungsdatum', 
	'Index', 'Betreff', 'Spruch', 'Begruendung', 'Dokumentnummer', 'Schlagworte', 'Sammlungsnummer',
	'Beachte', 'Gerichtsentscheidung', 'RisID')
	names(out) <- NULL
    all_names <- unique(unlist(lapply(out, names)))
	if (!all(sapply(all_names, function(x) any(x==dbcols)))) 
		warning(sprintf("Found unknown colum name(s): %s\n",  all_names))
    db <- vector("list", length(dbcols))
    for(i in seq_along(dbcols)) {
        tmp <- sapply(out, "[", dbcols[i])
	#	print(dim(tmp))
        ind <- as.logical(sapply(tmp, function(x) {is.null(x) || is.na(x)}))
        tmp[ind] <- rep.int("", sum(ind))
        db[[i]] <- tmp
    }
    #ind <- which(sapply(db, function(s) all(sapply(s, length) == 1)))
    #db[ind] <- lapply(db[ind], unlist)
    #print(sapply(db, length))
	
	ret = data.frame(db)
	names(ret) = dbcols
	#print(ret)
	ret
}

