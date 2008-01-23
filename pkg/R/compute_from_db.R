`compute_from_db` <-
function(con, func, table_name, fields_in, fields_out, field_types="TEXT", 
		table_setup=setup_foreign_table, include_foreign_key=TRUE, limit=0) {
	table_setup(con, table_name, fields_out, field_types, include_foreign_key)
	
	chunk_size=100 ## Number of entries fetched per iteration
	if (limit == 0) 
		linecount<-dbGetQuery(con,"select max(SP_Nr) from Rechtsinformationssystem;")[1,1]
	else linecount = limit	
	lseq <- c(seq(0,linecount-1,by=chunk_size),linecount)
	#print(lseq)
	len <- length(lseq)
	i <- 1
	if (include_foreign_key)
		fields_out = c("SP_Nr", fields_out)
	while ( i < len ) {
		#print(paste(i,len))
		start <- lseq[i]+1
		stop <- lseq[i+1]
		cat("Working through from ",start," to ",stop,"...\n")
		fields = paste(c("SP_Nr", fields_in), collapse=", ")
		rs <- dbGetQuery(con,paste("select ", fields, " from Rechtsinformationssystem where SP_Nr>=",start," and SP_Nr<=",stop,";"))
		
		j <- 1
		len2 <- length(rs[,1])

		result = vector("list", length(fields_out))
		names(result) = fields_out

		while ( j <= len2 )
		{
			## rs[...] returns a data frame with one row
			try({
			res = func(rs[j,]) ## rs[j,...] returns a data frame with one row
			if (include_foreign_key) res = c(list(SP_Nr=rep(rs[j,"SP_Nr"],length(res[[1]]))), res)
			reslen = sapply(res, length)

			if (any(reslen[1] != reslen[-1])) stop("Uneven length in result: ", rs[j,"SP_Nr"] )
			for (nm in names(result)) result[[nm]] = c(result[[nm]], res[[nm]])
			})
			j <- j+1
		}
		result = data.frame(result)
		#print(result)
		if (length(result[[1]]) > 0)
			dbWriteTable(con,table_name, result, append=TRUE, row.names=FALSE)
		#cat("\r")
		i <- i+1
		#cat(i,":",len,"\n")
	}
}

