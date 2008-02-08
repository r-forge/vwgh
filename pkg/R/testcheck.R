testcheck <- function(x,y)
{
	cat("Performing catagorising check\n")
	db_query <- dbGetQuery(x,"select spruch,result from Rechtsinformationssystem,Result where Rechtsinformationssystem.SP_Nr == Result.SP_Nr")
	sample_query<- dbGetQuery(y,"select SP_Nr,Entscheidung from resultcheck")
	ll <- length(sample_query[,2])
	i <- 1
	rr <- 0
	ff <- 0
	ff_sp <- c()
	xx <- 0
	xx_sp <- c()
	proz <- round((i/ll)*100,2)
	err1 <- c()
	err2 <- c()
	err3 <- c()
	cat ("Starting ... ")
	while (i <= ll) 
	{	
		cat("Status: Richtig: ",rr,"  Falsch: ",ff,"  Unclassed: ",xx," Aufarbeitung: (",proz,")")
		proz <- round((i/ll)*100,2)	
		c_sp_nr <- sample_query[i,1]
		res <- db_query[c_sp_nr,2]
		res_sp <- sample_query[i,2]
		if ( res == res_sp ) 
		{
			rr <- rr + 1
		}
		else	
		{
			if ((res == -10 ) || (res >= 10)) 
			{
				xx <- xx + 1
				xx_sp <- c(xx_sp,c_sp_nr)
				cat("\n",xx_sp,"\n")
			}
			else 
			{
				ff <- ff + 1
				ff_sp <- c(ff_sp,c_sp_nr)			
			}
			err1 <- c(err1,db_query[c_sp_nr,1])
			err2 <-	c(err2,db_query[c_sp_nr,2])
			err3 <- c(err3,sample_query[i,2])		
		} 
		i <- i +1
		cat("\r")
	}	
	cat("Starting ... DONE                                                                                      \n")
	err <- cbind(err1,err2,err3)
	cat ("Final: Richtig: ",rr," (",round(rr/ll*100,2),")    Falsch: ",ff, "(",round(ff/ll*100,2),")      Unclassed: ",xx, "(",round(xx/ll*100,2),")\n\n")
	xx_sp <<- xx_sp
	ff_sp <<- ff_sp
	write.csv(file="false.csv",err)	
}