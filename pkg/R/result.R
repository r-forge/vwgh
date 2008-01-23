`result` <-
function(data)
	{	
		x = data[["Spruch"]]
		x <- tolower(x)
		res = 0
		if (x != "" ) x <- unlist(strsplit(x,"mehrbegehren"))[1]
		#if (x != "" ) x <- unlist(strsplit(x,"?brigen"))[1]
		pat1 <- "unbegr.ndet|abgewiesen|eingestellt|nicht (statt|stattgegeben|folge)|zur.ckgewiesen|abgelehnt|best.tigt|antrag nicht|nachzahlung auferlegt|n i c h t   s t a t t g e g e b e n|n i c h t s t a t t g e g e b e n|zur.ckwiesen|Vollstreckbarkeit hemmenden Rechtszug mehr unterliegt"
		pat2 <- "aufgehoben|erteilt|stattgegeben|folge gegeben|(beschwerdef.hrerin|beschwerdef.hrenden Partei) Aufwendungen |wird bewilligt|bewillig \t|rechtswidrig|berichtigt|geb.hrt dem Beschwerdef.hrer|aufschiebende Wirkung zuerkannt|Bescheid dahin gehend abge?ndert"
		pat3 <- "Vorabentscheidung|abgetreten|antrag der Beschwerdef.hrerin|in folgender Hinsicht zu h.ren|ausgesetzt"
		if ( length(grep(pat1,x)) == 1 ) res <- 0 
		else
		{	
			if ( length(grep(pat2,x)) == 1 ) res <- 1
			else
			{
				if ( length(grep(pat3,x)) == 1 ) res <- 2
				else
				{
					if (x == "" ) res <- -1
					else res <- -2
				}
			}	
		}	
	list(Result=res)
}

