
###############################################################################
# Find best M for M5P according to minimal validation MAE                     #
###############################################################################
bestm5p <- function(vout_1, vin_1, traux_1, tvaux_1, mintervals_1)
{
	trmae_1 <- NULL;
	tvmae_1 <- NULL;
	for (i in mintervals_1)
	{
		ml_1 <- M5P(formula=traux_1[,vout_1] ~ .,data=traux_1[,vin_1], control = Weka_control(M = i));
		mae <- (sum(abs(ml_1$predictions - traux_1[,vout_1])))/length(traux_1[,1]);
		trmae_1 <- c(trmae_1,mae);

		prediction <- predict(ml_1,newdata=tvaux_1);
		mae <- (sum(abs(prediction - tvaux_1[,vout_1])))/length(tvaux_1[,1]);
		tvmae_1 <- c(tvmae_1,mae);
	}

	mmin_1 <- mintervals_1[which.min(tvmae_1)];
	ml_1 <- M5P(formula=traux_1[,vout_1] ~ .,data=traux_1[,vin_1], control = Weka_control(M = mmin));

	print (mean(abs(ml_1$predictions - traux_1[,vout_1])));

	retval <- list();
	retval$ml <- ml_1;
	retval$trmae <- trmae_1;
	retval$tvmae <- tvmae_1;
	retval$mmin <- mmin_1;
	
	retval;
}


###############################################################################
# Save the datasets and created models                                        #
###############################################################################

saveloads <- function (traux_0, tvaux_0, ttaux_0, name_0, algor_0)
{
	write.table(tvaux_0, file = paste(algor_0,"-tv",name_0,".csv",sep=""), sep = ",");
	write.table(traux_0, file = paste(algor_0,"-tr",name_0,".csv",sep=""), sep = ",");
	write.table(ttaux_0, file = paste(algor_0,"-tt",name_0,".csv",sep=""), sep = ",");
}

savemodel <- function (traux_2, tvaux_2, ttaux_2, model_2, name_2, algor_2)
{
	saveloads(traux_2, tvaux_2, ttaux_2, name_2, algor_2);

	rJava::.jcache(model_2$classifier);
	save(model_2,file=paste(algor_2,"-",name_2,"-model.dat",sep=""));
}

loadmodel <- function (name,algor_0)
{
	load(paste(algor_0,"-",name,"-model.dat",sep=""));
	##rJava::.jstrVal(ml1$classifier);
}

savestatus <- function ()
{
	save.session("RData");
	savehistory("Rhistory");
	system(paste("tar cvzf rsession-",Sys.Date(),".tar.gz RData Rhistory",sep=""));
	system("rm RData Rhistory");
}

###############################################################################
# Operations and transformation functions                                     #
###############################################################################

bindataset <- function (table_1)
{
	numaux <- sapply(table_1, is.numeric);

	binaux <- table_1[,numaux];
	classaux <- table_1[,!numaux];

	for (k in 1:length(classaux))
	{
		v <- vector();
		for (i in 1:length(levels(classaux[,k]))) v[levels(classaux[,k])[i]] <- i;

		m <- matrix(0,nrow=length(classaux[,k]),ncol=length(levels(classaux[,k])));
		for (i in 1:length(classaux[,k])) m[i,v[classaux[i,k]]] <- 1;
		colnames(m) <- levels(classaux[,k]);

		binaux <- cbind(binaux,m);
	}
	binaux;
}


