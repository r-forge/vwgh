`read_RIS_jurisdiction` <-
function(number) 
{
	ristemplate = "http://ris.bka.gv.at/taweb-cgi/taweb?x=d&o=d&d=VwGHT&i=%d&v=vwgh"

	url = sprintf(ristemplate, number)
	x = suppressWarnings(htmlTreeParse(url, isURL=TRUE, useInternalNodes=TRUE, encoding="Latin1"))
	cat("Fetching ", url, "\n")
	flush.console()
	if (xmlconv(getNodeSet(x, "//title")) == "Error") {
		warning(sprintf("%s does not exist!\n", number))
		return(NA)
	}
	norm = xmlconv(getNodeSet(x, "//font[@face='Courier New']"))
	#cat(norm, "\n")
	if (norm[1] != "Norm\n") 
		warning(sprintf("%s: Found '%s' were Norm expected!\n",  number, norm[1]))
	
	headers = c(xmlconv(getNodeSet(x, "//pre/b[1]")))
	content = c(xmlconv(getNodeSet(x, "//pre[b]")))
	if (length(headers) != length(content)) 
		warning(sprintf("%s: Headers and contents don't match!\n", number))
	if (any(substr(content, 1, nchar(headers)) != headers)) 
		warning(sprintf("%s: Improper header found!\n", number))
	content = substr(content, nchar(headers)+1, 1000000)

	## For some weird reason, 'Norm' is different
	norm = xmlconv(getNodeSet(x, "//font[@face='Courier New']"))
	if (norm[1] != "Norm\n") 
		warning(sprintf("%s: Found '%s' were Norm expected!\n",  number, norm[1]))
	headers = c(headers, "Norm", "RisID"); content = c(content, norm[2], number) 

	## remove leading and trailing whitespace
	content = gsub(pattern="^[[:space:]]*|[[:space:]]*$", 
		replacement="", content)
	content = gsub(pattern="\n", replacement=" ", content)
	chars =  intToUtf8(as.integer(c(228, 246, 252, 223)), multiple=TRUE)
	Encoding(chars) <- "UTF-8"
	dbnames = headers
	dbnames = gsub(chars[1], "ae", ignore.case=TRUE, dbnames)
	dbnames = gsub(chars[2], "oe", ignore.case=TRUE, dbnames)
	dbnames = gsub(chars[3], "ue", ignore.case=TRUE, dbnames)
	dbnames = gsub(chars[4], "ss", dbnames)
	dbnames = gsub("[^A-Za-z]", "", dbnames)
	names(content) = dbnames
	content
}

