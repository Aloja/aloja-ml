
# Josep Ll. Berral-García
# ALOJA-BSC-MSR hadoop.bsc.es
# 2015-09-07
# Pattern mining functions library for ALOJA-ML

suppressMessages(library(arules));
suppressMessages(library(methods));
options(max.print=10000000);

###############################################################################
# Pattern mining tools                                                        #
###############################################################################

aloja_bestrules_single_select <- function (ds, vin, bench, cluster, percent = "20%", minval = 50, saveall = NULL, savedata = NULL, quiet = 1, suppval = 0.1, confval = 0.1)
{
	dsaux <- ds[ds$Exe.Time > minval & ds$Benchmark %in% c(bench) & ds$Cl.Name %in% c(cluster),];
	aloja_bestrules_single(dsaux, vin, percent, saveall, savedata, quiet, suppval, confvall);
}

aloja_bestrules_single <- function (ds, vin, percent = "20%", saveall = NULL, savedata = NULL, quiet = 1, suppval = 0.1, confval = 0.1)
{
	if (!is.numeric(quiet)) quiet <- as.numeric(quiet);

	# Selected "Best" Executions
	q1 <- as.numeric(quantile(ds$Exe.Time,probs=seq(0,1,0.05))[percent]);
	dsauxq1 <- ds[ds$Exe.Time <= q1,vin];
	for (fck in names(dsauxq1)) if (!is.factor(dsauxq1[[fck]])) dsauxq1[[fck]] <- as.factor(dsauxq1[[fck]]);

	# Most Frequent Patterns for Single Attributes
	if (quiet == 1) sink("/dev/null");
	trans1 <- as(dsauxq1, "transactions");
	rules1 <- apriori(trans1, parameter= list(supp=suppval, conf=confval));
	if (quiet == 1) sink();

	dfaux1 <- as(rules1, "data.frame");
	dfaux2 <- do.call(rbind,strsplit(as.character(dfaux1$rules)," => "));
	colnames(dfaux2) <- c("precedent","consequent");
	retaux <- cbind(dfaux1,dfaux2);

	# Add extra information
	auxnump <- nchar(as.character(retaux$precedent)) - nchar(as.character(gsub("=","",retaux$precedent)));
	retaux <- cbind(auxnump,retaux);
	colnames(retaux)[1] <- "numprecs";

	retval <- retaux[order(retaux$numprecs,-retaux$support,-retaux$confidence),c("numprecs","precedent","consequent","support","confidence","lift")];

	# Dump data to file
	if (!is.null(savedata))
	{
		sink(paste("rules-",saveall,".data",sep=""));
		inspect(sort(rules1, by = "support"));
		sink();

		sink(paste("rules-",saveall,"-ns.data",sep=""));
		inspect(rules1);
		sink();
	}

	if (!is.null(saveall))
	{
		write.table(retval, file = paste(saveall,"-brs.csv",sep=""), sep = " ");
	}

	retval;
}

aloja_bestrules_pairs_select <- function (ds, vin, bench, cluster, percent = "20%", minval = 50, saveall = NULL, savedata = NULL, singles = FALSE, simplified = FALSE, quiet = 1, suppval = 0.2, confval = 0.2)
{
	dsaux <- ds[ds$Exe.Time > minval & ds$Benchmark %in% c(bench) & ds$Cl.Name %in% c(cluster),];
	aloja_bestrules_pairs(dsaux, vin, percent, saveall, savedata, singles, simplified, quiet, suppval, confval);
}

aloja_bestrules_pairs <- function (ds, vin, percent = "20%", saveall = NULL, savedata = NULL, singles = FALSE, simplified = FALSE, quiet = 1, suppval = 0.2, confval = 0.2)
{
	if (!is.numeric(quiet)) quiet <- as.numeric(quiet);

	# Selected "Best" Executions
	q1 <- as.numeric(quantile(ds$Exe.Time,probs=seq(0,1,0.05))[percent]);
	dsauxq1 <- ds[ds$Exe.Time <= q1,vin];
	for (fck in names(dsauxq1)) if (!is.factor(dsauxq1[[fck]])) dsauxq1[[fck]] <- as.factor(dsauxq1[[fck]]);

	# Generation of Paired Attributes
	translist <- list();
	for (k in 1:nrow(dsauxq1))
	{
		a <- dsauxq1[k,];
		patternlist <- list();

		# Basic Occurrences (add single attributes)
		if (singles)
		{
			for (j in 1:length(vin))
			{
				auxvj <- levels(a[,j])[a[,j]];
				patternlist <- c(patternlist,paste(vin[j],auxvj,sep=":"));
			}
		}
		
		# Pairing Occurrences
		for (i in 1:(length(vin)-1))
		{
			auxvi <- levels(a[,i])[a[,i]];
			auxvii <- suppressWarnings(!is.na(as.numeric(auxvi)));

			for (j in (i+1):length(vin))
			{
				auxvj <- levels(a[,j])[a[,j]];
				auxvji <- suppressWarnings(!is.na(as.numeric(auxvj)));
				patternlist <- c(patternlist,paste(vin[i],auxvi,vin[j],auxvj,sep=":"));

				# Adding simplified paired attributes
				if (simplified)
				{
					if (auxvii && auxvji)
					{
						inti <- as.numeric(auxvi);
						intj <- as.numeric(auxvj);
						if (inti != 1 && intj != 1)
						{
							if (inti < intj) patternlist <- c(patternlist,paste(vin[i],1,vin[j],round((intj / inti),digits=2),sep=":"));
							if (inti >= intj) patternlist <- c(patternlist,paste(vin[i],round((inti / intj),digits=2),vin[j],1,sep=":"));
						}
					}
				}
			}
		}
		translist[[k]] <- unlist(patternlist);
	}

	# Most Frequent Patterns for Paired Attributes
	if (quiet == 1) sink("/dev/null");
	trans2 <- as(translist, "transactions");
	rules2 <- apriori(trans2, parameter= list(supp=suppval, conf=confval));
	if (quiet == 1) sink();

	dfaux1 <- as(rules2, "data.frame");
	dfaux2 <- do.call(rbind,strsplit(as.character(dfaux1$rules)," => "));
	colnames(dfaux2) <- c("precedent","consequent");
	retaux <- cbind(dfaux1,dfaux2);

	# Add extra information
	auxnump <- nchar(as.character(retaux$precedent)) - nchar(as.character(gsub(":","",retaux$precedent)))/3;
	retaux <- cbind(auxnump,retaux);
	colnames(retaux)[1] <- "numprecs";

	retval <- retaux[order(retaux$numprecs,-retaux$support,-retaux$confidence),c("numprecs","precedent","consequent","support","confidence","lift")];

	# Dump to file
	if (!is.null(saveall))
	{
		sink(paste("rulespair-",saveall,".data",sep=""));
		inspect(sort(rules2, by = "support"));
		sink();

		sink(paste("rulespair-",saveall,"-ns.data",sep=""));
		inspect(rules2);
		sink();
	}

	if (!is.null(saveall))
	{
		write.table(retval, file = paste(saveall,"-brp.csv",sep=""), sep = " ");
	}

	retval;
}

aloja_bestrules_relations_select <- function (ds, vin, bench, cluster, percent = "20%", minval = 50, saveall = NULL, savedata = NULL, quiet = 1, suppval = 0.5, confval = 0.5)
{
	dsaux <- ds[ds$Exe.Time > minval & ds$Benchmark %in% c(bench) & ds$Cl.Name %in% c(cluster),];
	aloja_bestrules_relations(dsaux, vin, percent, saveall, quiet, suppval, confval);
}

aloja_bestrules_relations <- function (ds, vin, percent = "20%", saveall = NULL, savedata = NULL, quiet = 1, suppval = 0.5, confval = 0.5)
{
	if (!is.numeric(quiet)) quiet <- as.numeric(quiet);

	# Selected "Best" Executions
	q1 <- as.numeric(quantile(ds$Exe.Time,probs=seq(0,1,0.05))[percent]);
	dsauxq1 <- ds[ds$Exe.Time <= q1,vin];
	for (fck in names(dsauxq1)) if (!is.factor(dsauxq1[[fck]])) dsauxq1[[fck]] <- as.factor(dsauxq1[[fck]]);

	# Generation of Paired Attributes
	translist <- list();
	for (k in 1:nrow(dsauxq1))
	{
		a <- dsauxq1[k,];
		patternlist <- list();
		
		# Pairing Occurrences (ignores non-numerical attributes)
		for (i in 1:(length(vin)-1))
		{
			auxvi <- levels(a[,i])[a[,i]];
			auxvii <- suppressWarnings(!is.na(as.numeric(auxvi)));

			for (j in (i+1):length(vin))
			{
				auxvj <- levels(a[,j])[a[,j]];
				auxvji <- suppressWarnings(!is.na(as.numeric(auxvj)));
				
				if (auxvii && auxvji)
				{
					inti <- as.numeric(auxvi);
					intj <- as.numeric(auxvj);
					if (inti != 1 && intj != 1)
					{
						if (inti < intj) patternlist <- c(patternlist,paste(paste(vin[i],1,sep=":"),paste(vin[j],round((intj / inti),digits=2),sep=":"),sep="~"));
						if (inti >= intj) patternlist <- c(patternlist,paste(paste(vin[i],round((inti / intj),digits=2),sep=":"),paste(vin[j],1,sep=":"),sep="~"));
					} else {
						patternlist <- c(patternlist,paste(paste(vin[i],auxvi,sep=":"),paste(vin[j],auxvj,sep=":"),sep="~"));
					}
				}
			}
		}

		translist[[k]] <- unlist(patternlist);
	}

	# Most Frequent Patterns for Paired Attributes
	if (quiet == 1) sink("/dev/null");
	trans2 <- as(translist, "transactions");
	rules2 <- apriori(trans2, parameter= list(supp=suppval, conf=confval));
	if (quiet == 1) sink();

	dfaux1 <- as(rules2, "data.frame");
	dfaux2 <- do.call(rbind,strsplit(as.character(dfaux1$rules)," => "));
	colnames(dfaux2) <- c("precedent","consequent");
	retaux <- cbind(dfaux1,dfaux2);

	# Add extra information
	auxnump <- (nchar(as.character(retaux$precedent)) - nchar(as.character(gsub("~","",retaux$precedent))));
	retaux <- cbind(auxnump,retaux);
	colnames(retaux)[1] <- "numprecs";

	retval <- retaux[order(retaux$numprecs,-retaux$support,-retaux$confidence),c("numprecs","precedent","consequent","support","confidence","lift")];

	# Dump to file
	if (!is.null(saveall))
	{
		sink(paste("relations-",saveall,".data",sep=""));
		inspect(sort(rules2, by = "support"));
		sink();

		sink(paste("relations-",saveall,"-ns.data",sep=""));
		inspect(rules2);
		sink();
	}

	if (!is.null(saveall))
	{
		write.table(retval, file = paste(saveall,"-brp.csv",sep=""), sep = " ");
	}

	retval;
}

