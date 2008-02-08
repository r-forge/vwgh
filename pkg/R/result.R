`result` <-
function(data)
	{	
		x <- data[["Spruch"]]
		#x <- data
		x <- tolower(x)
		sp <- c(0,0,0,0,0,0)
		mx1 <- 0
		mx2 <- 0
		mx3 <- 0

		## Ignoring `Mehrbegehren`
		x <- unlist(strsplit(x,"mehrbegehren|(kosten)ersatzbegehren|kosten(mehr)begehren"))[1]

		## 6 Grundkategorien

		## Zurueckweisung
		pat1 <- "zur.ckgewiesen"
		
		## Abweisung 
		pat2 <- "abgewiesen|abewiesen|abgegewiesen|abgelehnt"

		#pat2X <- "n i c h t   s t a t t g e g e b e n|n i c h t s t a t t g e g e b e n"


		## Aufhebung
		pat3="aufgehoben|erteilt|folge gegeben|rechtswidrig|berichtigt|geb.hrt dem Beschwerdef.hrer|erlassen|angeordnet"

		#pat3X <- "stattgegeben"

		## Einstellung
		pat4 <- "eingestellt"

		## Beschluss
		pat5="beschlu.|stattgegeben|bewilligt"

		## Vorabentscheidung
		pat6="vorabentscheidung"

		## specific definitons	

		## 2 -> 7
		pat2X1 <- ".brigen"

		## 2/3 -> 5
		pat2X2 <- "antr.g"
		
		##  3  -> 9
		pat3X1 <- "sowie|spruchpunkt"


		## ungebr.ndet
		## best.tigt
		## antrag nicht
		## nachzahlung auferlegt
		## abgetreten
		## antrag der beschwerdef.hrerin
		## in folgender hinsicht zu h.ren
		## ausgesetzt
		## (beschwerdef.hrerin|beschwerdef.hrenden partei) aufwendungen
		## aufschiebende Wirkung zuerkannt
		## bescheid dahingehend abge.ndert	


		if (length(grep(pat1,x)) >= 1 ) sp[1] <- 1
		if (length(grep(pat2,x)) >= 1 ) sp[2] <- 1
		if (length(grep(pat3,x)) >= 1 ) sp[3] <- 1
		if (length(grep(pat4,x)) >= 1 ) sp[4] <- 1
		if (length(grep(pat5,x)) >= 1 ) sp[5] <- 1
		if (length(grep(pat6,x)) >= 1 ) sp[6] <- 1
		
		## Corrections
		if (length(grep(pat2X1,x)) >= 1 ) mx1 <- 1
		if (length(grep(pat2X2,x)) >= 1 ) mx2 <- 1
		if (length(grep(pat3X1,x)) >= 1 ) mx3 <- 1
			
		#else
		#{	
		#	if (length(grep(pat3X,x)) >= 1 )
		#	{
		#		sp[3] <- 1
		#	}
		#}

                if (sum(sp) == 2 && ( sp[5] == 1 || sp[6] == 1 ))
                  {
                    if (sp[5] == 1)
                      {
                        sp[5] <- 0
                      }
                    else
                      {
                        if (sp[6] == 1)
                          {
                            sp[6] <- 0
                          }
                      }
                 }

                 if (sum(sp) >= 3 && ( sp[5] == 1 || sp[6] == 1 ))
                   {
                     sp[5] <- 0
                     sp[6] <- 0
                   }
                
		#cat(res,"\n")
		if (sum(sp) == 1 )  
		{
			if ( mx1 >= 1 ) 
			{
				if ( sp[2] == 1 ) 
				{
					res <- 7
				}
				else res <- grep("1",sp)
			}
			else
			{	
				if ( mx2 >= 1 ) 
				{
					if ( sp[2] == 1 ) res <- 5
					else if ( sp[3] == 1 ) res <- 5
					else res <- grep("1",sp)
				}
				else 
				{
					if (mx3 >= 1 )
					{
						if ( sp[3] == 1 ) res <- 9
						else res <- grep("1",sp)
					}
					else	res <- grep("1",sp)						
				}
			}
		}
		else
		{
			if (sum(sp) == 0 )
			{
				res <- -10
			}
			else 
			{
				sp2 <- sp[1]*10^5+sp[2]*10^4+sp[3]*10^3+sp[4]*10^2+sp[5]*10^1+sp[6]*10^0
				if (sum(sp) == 2 )
				{
					## Mixture about:
					## teilweise Abweisung-Zurueckweisung
					if ( sp2 == 110000 ) 
					{
						res <- 7
					}
					else
					{
						## teilweise Einstellung-Abweisung
						if ( sp2 == 10100 )  
						{
							res <- 8
						}
						else
						{
							## teilweise Aufhebung
							if ( sp[3] == 1 )    
							{
								res <- 9
							}
							else
							{
								res <- sp[1]*10^5+sp[2]*2*10^4+sp[3]*3*10^3+sp[4]*4*10^2+sp[5]*5*10^1+sp[6]*6*10^0
							}
						}
					}
                                }
				else 
				{
                                  #if (sp[3] == 1 ) res <- 9
                                  #else 
				  res <- sp[1]*10^5+sp[2]*2*10^4+sp[3]*3*10^3+sp[4]*4*10^2+sp[5]*5*10^1+sp[6]*6*10^0
				}
			}

		}
	list(Result=res)
}

