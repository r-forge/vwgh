`build_RIS_db` <-
function(con, from=1, to=80000, new_table=TRUE) 
{
	if (new_table) {
		rs <- dbGetQuery(con,"drop table if exists Rechtsinformationssystem;")
		rs <- dbGetQuery(con, "CREATE TABLE `Rechtsinformationssystem` (`Norm` text not null ,`Gerichtstyp` text not null ,`Geschaeftszahl` char(10) not null,`Entscheidungsdatum` date not null,`Veroeffentlichungsdatum` date not null,`Index` text,`Betreff` text,`Spruch` text,`Begruendung` text,`Dokumentnummer` text,`Schlagworte` text,`Sammlungsnummer` text,`Beachte` text,`Gerichtsentscheidung` text, `RisID` int(6), `SP_Nr` integer not null primary key autoincrement); ")
	}
	
	maxnum =  dbGetQuery(con, "select max(SP_Nr) from Rechtsinformationssystem")[1,1]
	if (is.na(maxnum)) maxnum = 0
	
    chunk_size = 100
    lseq <-  c(seq(from-1,to-1,by=chunk_size), to)
    rnd  <- length(lseq)-1
    for ( i in 1:rnd )
	{
    	nums = (lseq[i]+1):lseq[i+1]
		#cat("Last Filename: ",ffile[lseq[i+1]],"\n")
		
		## Read in all jurisdictions.
		out <- lapply(nums, read_RIS_jurisdiction)
		#print(sapply(out, strtrim, width=70))
		print("Jurisdiction read.")
		out = out[!is.na(out)]
		if (length(out)==0) next
		
     	## And convert into a data frame.
    	data <- put_RIS_jurisdictions_into_data_frame(out)
		print("Jurisdiction in DF.")
		#print(lapply(data, strtrim, width=70))
		#print(sapply(length
		data["SP_Nr"] = (maxnum+1):(maxnum+dim(data)[1])
		maxnum = maxnum+dim(data)[1]
		flush.console()
		dbWriteTable(con, "Rechtsinformationssystem", data, append=TRUE, row.names=FALSE)
		
		cat(" *************** END of ROUND ",i," ***********\n")
	}
	
}

