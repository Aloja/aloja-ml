
# Josep Ll. Berral-García
# ALOJA-BSC-MSR hadoop.bsc.es
# 2014-12-11
# Function library for ALOJA-ML

library(stringr);
library(RWeka);
library(devtools);
library(scales);
library(reshape);
library(nnet);
library(session);

set.seed(1234567890);

#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

###############################################################################
# Read datasets and prepare them for usage                                    #
###############################################################################

aloja_get_data <- function (fread, cds = FALSE, hds = FALSE, fproc = NULL)
{
	ds <- read.table(fread,header=T,sep=",");

	if ("End.time" %in% colnames(ds))
	{
		aux <- strptime(ds[,"End.time"],format="%Y%m%d%H%M%S");
		ds[,"End.time"] <- NULL;
	} else {
		aux <- rep(0,nrow(ds));
	}
	names_temp <- colnames(ds);
	ds <- cbind(ds,aux);
	colnames(ds) <- c(names_temp,"End.time");

	if (!("Running.Cost.." %in% colnames(ds)))
	{
		names_temp <- colnames(ds);
		ds <- cbind(ds,rep(0,nrow(ds)));
		colnames(ds) <- c(names_temp,"Running.Cost..");
	}
	
	retval <- ds[,!(colnames(ds) %in% c("X","Exec.Conf","Histogram","PARAVER"))];

	if (!is.null(fproc))
	{
		ds_sub <- ds[,c("Exe.Time","Running.Cost..","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster","End.time")];

		ds_ext <- ds;
		if (cds)
		{
			split <- str_split_fixed(ds[,"Exec.Conf"], "/", 2);
			exec_conf <- str_split_fixed(split[,1], "_", 13);
			aux <- strptime(paste(exec_conf[,1],exec_conf[,2],sep=""),format="%Y%m%d%H%M%S");
			exec_conf <- exec_conf[,-c(1,2)];
			exec_conf <- cbind(matrix(as.character(aux)),exec_conf);
			colnames(exec_conf) <- paste("Conf.",c("Time","Conf","Net","Disk","B","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster"),sep="")
			bench_conf <- str_split_fixed(split[,2], "_", 2);
			colnames(bench_conf) <- c("Conf.Benchmark","Conf.Options");
			ds_ext <- cbind(ds_ext,exec_conf,bench_conf);
		}

		if (hds)
		{
			split <- str_split_fixed(ds[,"Histogram"], "/", 2);
			histogram <- str_split_fixed(split[,1], "_", 13);
			colnames(histogram) <- paste("Hist.",c("Date","Time","Conf","Net","Disk","B","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster"),sep="")
			bench_hist <- str_split_fixed(split[,2], "_", 2);
			colnames(bench_conf) <- c("Hist.Benchmark","Hist.Options");
			ds_ext <- cbind(ds_ext,histogram,bench_hist);
		}

		write.table(retval,file=paste(fproc,"-pro.csv",sep=""),sep=",",row.names=F);
		write.table(ds_sub,file=paste(fproc,"-sub.csv",sep=""),sep=",",row.names=F);
		if (cds || hds) write.table(ds_ext,file=paste(fproc,"-ext.csv",sep=""),sep=",",row.names=F);
	}

	retval;
}

###############################################################################
# Print summaries for each benchmark                                          #
###############################################################################

aloja_print_summaries <- function (fprint = NULL, ds, ds_sub, fwidth = 1000, ms = 10, sname = NULL)
{
	if (!is.null(fprint))
	{
		sink(file=fprint,append=FALSE,type="output");
		aux_tmp <- getOption("width");
		options(width=fwidth);
	}

	cat("Summary for General Data","\n");
	print(summary(ds,maxsum=ms));

	if (!is.null(sname))
	{
		for (name in levels(ds[,sname]))
		{
			cat("\n","Summary per",sname,name,"\n");
			print(summary(ds_sub[ds[,sname]==name,],maxsum=ms));
		}
	}

	if (!is.null(fprint))
	{
		sink(NULL);
		options(width=aux_tmp);
	}
}

aloja_crossvariables <- function (ds, pnglabel = "cross", jfactor = 0)
{
	numaux <- sapply(data.frame(ds), is.numeric);

	system("mkdir -p temp");
	for (var1 in 1:(ncol(ds)-1))
	{
		if (numaux[var1])
		{
			auxdata1 <- ds[,var1];
		} else {
			auxdata1 <- match(as.factor(ds[,var1]),levels(as.factor(ds[,var1])));
		}
		auxlabel1 <- colnames(ds)[var1];

		for (var2 in (var1 + 1):ncol(ds))
		{
			if (numaux[var2])
			{
				auxdata2 <- ds[,var2];
			} else {
				auxdata2 <- match(as.factor(ds[,var2]),levels(as.factor(ds[,var2])));
			}
			auxlabel2 <- colnames(ds)[var2];

			auxframe <- na.omit(data.frame(auxdata1, auxdata2));
			png(paste("temp/",pnglabel,"-",var1,"-",var2,".png",sep=""),width=1000,height=500);
				if (!numaux[var2]) par(oma=c(0,0,0,2));
				plot(jitter(auxframe[,1],factor=jfactor),jitter(auxframe[,2],factor=jfactor),xlim=c(min(auxframe[,1]),max(auxframe[,1])),ylim=c(min(auxframe[,2]),max(auxframe[,2])), xlab=auxlabel1, ylab=auxlabel2);
				if (!numaux[var1]) axis(3, at=seq(1,length(levels(as.factor(ds[,var1])))), labels=levels(as.factor(ds[,var1])), las=2);
				if (!numaux[var2]) axis(4, at=seq(1,length(levels(as.factor(ds[,var2])))), labels=levels(as.factor(ds[,var2])), las=2);
			dev.off();
		}		
	}
}

###############################################################################
# ANOVA of current variables                                                  #
###############################################################################

aloja_anova <- function (ds)
{
	anova_1 <- list();
	anova_1[["alpha"]] <- 0.05;
	anova_1[["N"]] <- NULL;
	anova_1[["K"]] <- NULL;
	anova_1[["gmean"]] <- NULL;
	anova_1[["ssb"]] <- 0;
	anova_1[["ssw"]] <- 0;
	anova_1[["mse"]] <- NULL;
	anova_1[["f"]] <- NULL;

	bmks <- list();
	anova_1[["means"]] <- list();
	anova_1[["stdevs"]] <- list();

	for (i in levels(ds[,"Benchmark"]))
	{
		bmks[[i]] <- ds[ds[,"Benchmark"]==i,c("Exe.Time","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster")];
		anova_1$means[[i]] <- mean(bmks[[i]][,"Exe.Time"]);
		anova_1$stdevs[[i]] <- sd(bmks[[i]][,"Exe.Time"]);
	}
	anova_1$gmean <- mean(rapply(anova_1$means,function(x) x));

	for (i in levels(ds[,"Benchmark"]))
	{
		anova_1$ssb <- anova_1$ssb + (length(bmks[[i]][,"Exe.Time"]) * (anova_1$means[[i]] - anova_1$gmean)^2);
	}

	anova_1$N <- nrow(ds);
	anova_1$K <- length(levels(ds[,"Benchmark"]));
	for (i in 1:anova_1$N)
	{
		anova_1$ssw <- anova_1$ssw + (ds[i,"Exe.Time"] - anova_1$means[[ds[i,"Benchmark"]]])^2;
	}
	anova_1$mse <- anova_1$ssw / (anova_1$N - anova_1$K);
	anova_1$f <- (anova_1$ssb / (anova_1$K - 1)) / anova_1$mse;
	anova_1$critical <- qf(1-anova_1$alpha, anova_1$K-1, anova_1$N-1);

	print (c("Means are equal: ",anova_1$f < anova_1$critical));

	anova_1;
}

###############################################################################
# Dataset splitting functions                                                 #
###############################################################################

aloja_split_dataset <- function (ds, vin, vout, tsplit, vsplit)
{
	retval <- list();

	aux <- ds[,c("ID",vout,vin)];
	retval[["dataset"]] <- aux;

	selected <- sample(1:nrow(aux),nrow(aux)*tsplit);
	ttaux <- aux[selected,];
	ntaux <- aux[-selected,];

	retval[["tselected"]] <- selected;
	retval[["testset"]] <- ttaux;

	selected <- sample(1:nrow(ntaux),nrow(ntaux)*vsplit);
	traux <- ntaux[selected,];
	tvaux <- ntaux[-selected,];

	retval[["rselected"]] <- selected;
	retval[["trainset"]] <- traux;
	retval[["validset"]] <- tvaux;

	retval;
}

aloja_load_splits <- function (vin, vout, ttaux, traux, tvaux)
{
	retval <- list();

	# Re-Factorization of Data
	retval[["dataset"]] <- aloja_dbind(aloja_dbind(ttaux[,c("ID",vout,vin)],traux[,c("ID",vout,vin)]),tvaux[,c("ID",vout,vin)]);

	retval[["testset"]] <- retval$dataset[retval$dataset[,"ID"] %in% ttaux[,"ID"],];
	retval[["trainset"]] <- retval$dataset[retval$dataset[,"ID"] %in% traux[,"ID"],];
	retval[["validset"]] <- retval$dataset[retval$dataset[,"ID"] %in% tvaux[,"ID"],];

	retval[["tselected"]] <- rownames(retval$testset);
	retval[["rselected"]] <- rownames(retval$trainset);

	retval;
}

aloja_load_testsplit <- function (ds, vin, vout, ttaux, vsplit)
{
	retval <- list();

	aux <- ds[,c("ID",vout,vin)];
	ntaux <- aux[!(aux[,"ID"] %in% ttaux[,"ID"]),];

	selected <- sample(1:nrow(ntaux),nrow(ntaux)*vsplit);
	traux <- ntaux[selected,];
	tvaux <- ntaux[-selected,];

	# Re-Factorization of Data
	retval[["dataset"]] <- aloja_dbind(aloja_dbind(ttaux,traux),tvaux);

	retval[["testset"]] <- retval$dataset[retval$dataset[,"ID"] %in% ttaux[,"ID"],];
	retval[["trainset"]] <- retval$dataset[retval$dataset[,"ID"] %in% traux[,"ID"],];
	retval[["validset"]] <- retval$dataset[retval$dataset[,"ID"] %in% tvaux[,"ID"],];

	retval[["tselected"]] <- rownames(retval$testset);
	retval[["rselected"]] <- rownames(retval$trainset);

	retval;
}

aloja_datafile_load <- function (ds = NULL, vin, vout, ttfile, trfile = NULL, tvfile = NULL, vsplit = 0.66)
{
	retval <- list();

	ttaux <- read.table(ttfile,header=T,sep=",")[,c("ID",vout,vin)];

	if (!is.null(trfile) & !is.null(tvfile))
	{
		traux <- read.table(trfile,header=T,sep=",")[,c("ID",vout,vin)];
		tvaux <- read.table(tvfile,header=T,sep=",")[,c("ID",vout,vin)];
	} else {
		aux <- ds[,c("ID",vout,vin)];
		ntaux <- aux[!(aux[,"ID"] %in% ttaux[,"ID"]),];

		selected <- sample(1:nrow(ntaux),nrow(ntaux)*vsplit);
		traux <- ntaux[selected,];
		tvaux <- ntaux[-selected,];
	}

	# Re-Factorization of Data
	retval[["dataset"]] <- aloja_dbind(aloja_dbind(ttaux,traux),tvaux);

	retval[["testset"]] <- retval$dataset[retval$dataset[,"ID"] %in% ttaux[,"ID"],];
	retval[["trainset"]] <- retval$dataset[retval$dataset[,"ID"] %in% traux[,"ID"],];
	retval[["validset"]] <- retval$dataset[retval$dataset[,"ID"] %in% tvaux[,"ID"],];

	retval[["tselected"]] <- rownames(retval$testset);
	retval[["rselected"]] <- rownames(retval$trainset);

	retval;
}

aloja_load_datasets <- function (ds = NULL, vin, vout, tsplit = NULL, vsplit = NULL, ttaux = NULL, ntaux = NULL, traux = NULL, tvaux = NULL, ttfile = NULL, trfile = NULL, tvfile = NULL)
{
	rt <- NULL;

	if (!is.null(ttfile)) {
		rt <- aloja_datafile_load(ds, vin, vout, ttfile, trfile, tvfile, vsplit);
	} else if (!is.null(ttaux) & !is.null(traux) & !is.null(tvaux)) {
		rt <- aloja_load_splits(vin, vout, ttaux,traux,tvaux);
	} else if (!is.null(ttaux) & is.null(tvaux) & is.null(traux))	{
		rt <- aloja_load_testsplit(ds,vin,vout,ttaux,vsplit);
	} else {
		rt <- aloja_split_dataset(ds,vin,vout,tsplit,vsplit);
	}

	rt;
}

###############################################################################
# Operations and transformation functions                                     #
###############################################################################

aloja_binarize_ds <- function (table_1)
{
	numaux <- sapply(data.frame(table_1), is.numeric);

	binaux <- table_1[,numaux];
	classaux <- table_1[,!numaux];

	if (length(classaux) > 0)
	{
		for (k in 1:length(classaux))
		{
			v <- vector();
			for (i in 1:length(levels(classaux[,k]))) v[levels(classaux[,k])[i]] <- i;

			m <- matrix(0,nrow=length(classaux[,k]),ncol=length(levels(classaux[,k])));
			for (i in 1:length(classaux[,k])) m[i,v[classaux[i,k]]] <- 1;
			colnames(m) <- levels(classaux[,k]);

			binaux <- cbind(binaux,m);
		}
	}
	binaux;
}

aloja_binarize_mixsets <- function (vin, vout, traux = NULL, ntaux = NULL, tvaux = NULL, ttaux = NULL)
{
	retval <- list();

	if (!is.null(traux)) traux <- aloja_binarize_ds(traux[,c("ID",vout,vin)]);
	if (!is.null(ntaux)) ntaux <- aloja_binarize_ds(ntaux[,c("ID",vout,vin)]);
	if (!is.null(tvaux)) tvaux <- aloja_binarize_ds(tvaux[,c("ID",vout,vin)]);
	if (!is.null(ttaux)) ttaux <- aloja_binarize_ds(ttaux[,c("ID",vout,vin)]);

	if (!is.null(traux) & !is.null(tvaux) & !is.null(ttaux))
	{
		for(name in !(c(colnames(traux),colnames(ttaux)) %in% colnames(tvaux)))
		{
			auxnames <- colnames(tvaux);
			tvaux <- cbind(tvaux,rep(0,nrow(tvaux))); 
			colnames(tvaux) <- c(auxnames,name);
		}

		for(name in !(c(colnames(traux),colnames(tvaux)) %in% colnames(ttaux)))
		{
			auxnames <- colnames(ttaux);
			ttaux <- cbind(ttaux,rep(0,nrow(ttaux))); 
			colnames(ttaux) <- c(auxnames,name);
		}

		for(name in !(c(colnames(ttaux),colnames(tvaux)) %in% colnames(traux)))
		{
			auxnames <- colnames(traux);
			traux <- cbind(traux,rep(0,nrow(traux))); 
			colnames(traux) <- c(auxnames,name);
		}
	}

	if (!is.null(ttaux) & !is.null(ntaux))
	{
		for(name in !(colnames(traux) %in% colnames(ntaux)))
		{
			auxnames <- colnames(ntaux);
			ntaux <- cbind(ntaux,rep(0,nrow(ntaux))); 
			colnames(ntaux) <- c(auxnames,name);
		}

		for(name in !(colnames(ntaux) %in% colnames(ttaux)))
		{
			auxnames <- colnames(ttaux);
			ttaux <- cbind(ttaux,rep(0,nrow(ttaux))); 
			colnames(ttaux) <- c(auxnames,name);
		}
	}

	retval[["trset"]] <- traux;
	retval[["ntset"]] <- ntaux;
	retval[["tvset"]] <- tvaux;
	retval[["ttset"]] <- ttaux;
	retval;
}

###############################################################################
# Learning methods                                                            #
###############################################################################

aloja_nnet <-  function (ds, vin, vout, tsplit = 0.25, vsplit = 0.66, rmols = TRUE, pngval = NULL, pngtest = NULL, saveall = NULL, ttaux = NULL, ntaux = NULL, traux = NULL, tvaux = NULL, sigma = 3, ttfile = NULL, trfile = NULL, tvfile = NULL, decay = 5e-4, hlayers = 3, maxit = 1000, prange = NULL)
{
	# Fix parameter class in case of CLI string input
	if (!is.null(prange)) prange <- as.numeric(prange);
	if (!is.numeric(tsplit)) tsplit <- as.numeric(tsplit);
	if (!is.numeric(vsplit)) vsplit <- as.numeric(vsplit);
	if (!is.integer(sigma)) sigma <- as.integer(sigma);
	if (!is.null(decay)) decay <- as.numeric(decay);
	if (!is.integer(hlayers)) hlayers <- as.integer(hlayers);
	if (!is.integer(maxit)) maxit <- as.integer(maxit);

	# Binarization of variables
	dsbaux <- aloja_binarize_ds(ds[,c(vout,vin)]);
	auxset <- aloja_binarize_mixsets(vin,vout,traux=traux,ntaux=ntaux,tvaux=tvaux,ttaux=ttaux);
	vin <- colnames(dsbaux[!(colnames(dsbaux) %in% vout)]);

	# Load and split datasets
	dsid <- cbind(ds[,"ID"],dsbaux);
	colnames(dsid) <- c("ID",vout,vin);
	rt <- aloja_load_datasets (dsid,vin,vout,tsplit,vsplit,auxset$ttset,auxset$ntset,auxset$trset,auxset$tvset,ttfile,trfile,tvfile);

	# Remove outliers (leap of faith, as vout may not be normal
	if (rmols)
	{
		rt[["olstrain"]] <- rt$trainset[rt$trainset[,vout] > mean(rt$trainset[,vout]) + sigma * sd(rt$trainset[,vout]),];
		rt[["olsvalid"]] <- rt$validset[rt$validset[,vout] > mean(rt$validset[,vout]) + sigma * sd(rt$validset[,vout]),];
		
		rt$trainset <- rt$trainset[rt$trainset[,vout] <= mean(rt$trainset[,vout]) + sigma * sd(rt$trainset[,vout]),];
		rt$validset <- rt$validset[rt$validset[,vout] <= mean(rt$validset[,vout]) + sigma * sd(rt$validset[,vout]),];
	}

	# Normalize values
	trauxnorm <- NULL;
	tvauxnorm <- NULL;
	ttauxnorm <- NULL;
	rt[["maxout"]] <- NULL;
	rt[["minout"]] <- NULL;
	for (i in c(vout,vin))
	{
		divisor <- max(c(rt$trainset[,i],rt$validset[,i])); if (divisor == 0) divisor = 1e-15;
		trauxnorm <- cbind(trauxnorm, (rt$trainset[,i]-min(c(rt$trainset[,i],rt$validset[,i])))/divisor);
		tvauxnorm <- cbind(tvauxnorm, (rt$validset[,i]-min(c(rt$trainset[,i],rt$validset[,i])))/divisor);
		ttauxnorm <- cbind(ttauxnorm, (rt$testset[,i]-min(c(rt$trainset[,i],rt$validset[,i])))/divisor); # Same Norm (tr,tv) as not seen before
		rt[["maxout"]] <- c(rt[["maxout"]],divisor);
		rt[["minout"]] <- c(rt[["minout"]],min(c(rt$trainset[,i],rt$validset[,i])));
		trauxnorm[is.na(trauxnorm)] <- 0;
		tvauxnorm[is.na(tvauxnorm)] <- 0;
		ttauxnorm[is.na(ttauxnorm)] <- 0;
	}
	rt[["normtrainset"]] <- trauxnorm;
	rt[["normvalidset"]] <- tvauxnorm;
	rt[["normtestset"]] <- ttauxnorm;
	colnames(rt$normtrainset) <- c(vout,vin);
	colnames(rt$normvalidset) <- c(vout,vin);
	colnames(rt$normtestset) <- c(vout,vin);
	rt[["maxout"]] <- matrix(rt[["maxout"]]);
	rt[["minout"]] <- matrix(rt[["minout"]]);
	rownames(rt[["maxout"]]) <- c(vout,vin);
	rownames(rt[["minout"]]) <- c(vout,vin);

	rt[["varin"]] <- vin;
	rt[["varout"]] <- vout;
	
	# Training and Validation
	rt[["model"]] <- nnet(y=rt$normtrainset[,vout],x=rt$normtrainset[,vin],size=hlayers,decay=decay,maxit=maxit);
	rt[["predtrain"]] <- rt$model$fitted.values;
	rt[["predval"]] <- predict(rt$model,newdata=rt$normvalidset[,vin]);
	if (!is.null(prange))
	{
		rt$predtrain[rt$predtrain < prange[1]] <- prange[1];
		rt$predtrain[rt$predtrain > prange[2]] <- prange[2];
		rt$predval[rt$predval < prange[1]] <- prange[1];
		rt$predval[rt$predval > prange[2]] <- prange[2];
	}
	rt[["maeval"]] <- mean(abs(rt$predval*rt$maxout[vout,1]+rt$minout[vout,1] - rt$validset[,vout]));
	rt[["raeval"]] <- mean(abs((rt$predval*rt$maxout[vout,1]+rt$minout[vout,1] - rt$validset[,vout])/rt$validset[,vout]));

	if (!is.null(pngval))
	{
		png(paste(pngval,".png",sep=""),width=500,height=500);
		plot(rt$predval,rt$normvalidset[,vout],main=paste("NN 32-5-1, decay",decay,"maxit",maxit));
		abline(0,1);
		dev.off();
	}

	# Testing and evaluation
	rt[["predtest"]] <- predict(rt$model,newdata=rt$normtestset[,vin]);
	if (!is.null(prange))
	{
		rt$predtest[rt$predtest < prange[1]] <- prange[1];
		rt$predtest[rt$predtest > prange[2]] <- prange[2];
	}
	rt[["maetest"]] <- mean(abs(rt$predtest*rt$maxout[vout,1]+rt$minout[vout,1] - rt$testset[,vout])) ;
	rt[["raetest"]] <- mean(abs((rt$predtest*rt$maxout[vout,1]+rt$minout[vout,1] - rt$testset[,vout])/rt$testset[,vout]));

	if (!is.null(pngtest))
	{
		png(paste(pngtest,".png",sep=""),width=1000,height=500);
		par(mfrow=c(1,2));
		plot(rt$predval,rt$normvalidset[,vout],main=paste("NN 32-5-1, decay",decay,"maxit",maxit));
		abline(0,1);
		plot(rt$predtest,rt$normtestset[,vout],main=paste("NN 32-5-1, decay",decay,"maxit",maxit));
		abline(0,1);
		dev.off();
	}
	#plot.nnet(rt$model);
	#plot.nnet(rt$model$wts,rt$model$n);

	print(c(rt$maeval,rt$raeval));
	print(c(rt$maetest,rt$raetest));

	if (!is.null(saveall))
	{
		aloja_save_model(rt$model,tagname=saveall);
		aloja_save_object(rt,tagname=saveall);
		aloja_save_predictions(rt$dataset,rt$trainset,rt$predtrain*rt$maxout[vout,1]+rt$minout[vout,1],rt$validset,rt$predval*rt$maxout[vout,1]+rt$minout[vout,1],rt$testset,rt$predtest*rt$maxout[vout,1]+rt$minout[vout,1],testname=saveall);
	}

	rt;
}

aloja_linreg <- function (ds, vin, vout, tsplit = 0.25, vsplit = 0.66, rmols = TRUE, pngval = NULL, pngtest = NULL, saveall = NULL, ttaux = NULL, ntaux = NULL, traux = NULL, tvaux = NULL, sigma = 3, ttfile = NULL, trfile = NULL, tvfile = NULL, ppoly = 1, prange = NULL)
{
	# Fix parameter class in case of CLI string input
	if (!is.null(prange)) prange <- as.numeric(prange);
	if (!is.numeric(tsplit)) tsplit <- as.numeric(tsplit);
	if (!is.numeric(vsplit)) vsplit <- as.numeric(vsplit);
	if (!is.integer(sigma)) sigma <- as.integer(sigma);
	if (!is.integer(ppoly)) ppoly <- as.integer(ppoly);

	# Prevent prediction startle because of singularities
	options(warn=-1);

	# Binarization of variables
	dsbaux <- aloja_binarize_ds(ds[,c(vout,vin)]);
	auxset <- aloja_binarize_mixsets(vin,vout,traux=traux,ntaux=ntaux,tvaux=tvaux,ttaux=ttaux);
	vin <- colnames(dsbaux[!(colnames(dsbaux) %in% vout)]);

	# Load and split datasets
	dsid <- cbind(ds[,"ID"],dsbaux);
	colnames(dsid) <- c("ID",vout,vin);
	rt <- aloja_load_datasets (dsid,vin,vout,tsplit,vsplit,auxset$ttaux,auxset$ntaux,auxset$traux,auxset$tvaux,ttfile,trfile,tvfile);
	rt[["varin"]] <- vin;
	rt[["varout"]] <- vout;

	# Remove outliers (leap of faith, as vout may not be normal)
	if (rmols)
	{
		rt[["olstrain"]] <- rt$trainset[rt$trainset[,vout] > mean(rt$trainset[,vout]) + sigma * sd(rt$trainset[,vout]),];
		rt[["olsvalid"]] <- rt$validset[rt$validset[,vout] > mean(rt$validset[,vout]) + sigma * sd(rt$validset[,vout]),];
		
		rt$trainset <- rt$trainset[rt$trainset[,vout] <= mean(rt$trainset[,vout]) + sigma * sd(rt$trainset[,vout]),];
		rt$validset <- rt$validset[rt$validset[,vout] <= mean(rt$validset[,vout]) + sigma * sd(rt$validset[,vout]),];
	}

	if (ppoly > 3 || ppoly < 1)
	{
		if (ppoly > 3) ppoly <- 3;
		if (ppoly < 1) ppoly <- 1;
		print(paste("[WARNING] Parameter ppoly not in [1,3]. ppoly=",ppoly," will be used instead",sep=""));
	}
	rt[["ppoly"]] <- ppoly;

	# Training and Validation
	if (ppoly == 1) rt[["model"]] <- lm(formula=rt$trainset[,vout] ~ ., data=data.frame(rt$trainset[,vin]));
	if (ppoly == 2) rt[["model"]] <- lm(formula=rt$trainset[,vout] ~ . + (.)^2, data=data.frame(rt$trainset[,vin]));
	if (ppoly == 3) rt[["model"]] <- lm(formula=rt$trainset[,vout] ~ . + (.)^2 + (.)^3, data=data.frame(rt$trainset[,vin]));
	rt[["predtrain"]] <- rt$model$fitted.values;
	rt[["predval"]] <- predict(rt$model,newdata=data.frame(rt$validset));
	if (!is.null(prange))
	{
		rt$predtrain[rt$predtrain < prange[1]] <- prange[1];
		rt$predtrain[rt$predtrain > prange[2]] <- prange[2];
		rt$predval[rt$predval < prange[1]] <- prange[1];
		rt$predval[rt$predval > prange[2]] <- prange[2];
	}
	rt[["maeval"]] <- mean(abs(rt$predval - rt$validset[,vout]));
	rt[["raeval"]] <- mean(abs((rt$predval - rt$validset[,vout])/rt$validset[,vout]));

	if (!is.null(pngval))
	{
		png(paste(pngval,".png",sep=""),width=500,height=500);
		plot(rt$predval,rt$validset[,vout],main=paste("Polynomial Regression power =",ppoly));
		abline(0,1);
		dev.off();
	}

	# Testing and evaluation
	rt[["predtest"]] <- predict(rt$model,newdata=data.frame(rt$testset));
	if (!is.null(prange))
	{
		rt$predtest[rt$predtest < prange[1]] <- prange[1];
		rt$predtest[rt$predtest > prange[2]] <- prange[2];
	}
	rt[["maetest"]] <- mean(abs(rt$predtest - rt$testset[,vout]));
	rt[["raetest"]] <- mean(abs((rt$predtest - rt$testset[,vout])/rt$testset[,vout]));

	if (!is.null(pngtest))
	{
		png(paste(pngtest,".png",sep=""),width=1000,height=500);
		par(mfrow=c(1,2));
		plot(rt$predval,rt$validset[,vout],main=paste("Polynomial Regression power =",ppoly));
		abline(0,1);
		plot(rt$predtest,rt$testset[,vout],main=paste("Test Polynomial Regression power =",ppoly));
		abline(0,1);
		dev.off();
	}

	print(c(rt$maeval,rt$raeval));
	print(c(rt$maetest,rt$raetest));

	if (!is.null(saveall))
	{
		aloja_save_model(rt$model,tagname=saveall);
		aloja_save_object(rt,tagname=saveall);
		aloja_save_predictions(rt$dataset,rt$trainset,rt$predtrain,rt$validset,rt$predval,rt$testset,rt$predtest,testname=saveall);
	}

	rt;
}

aloja_nneighbors <- function (ds, vin, vout, tsplit = 0.25, vsplit = 0.66, rmols = TRUE, pngval = NULL, pngtest = NULL, saveall = NULL, ttaux = NULL, ntaux = NULL, traux = NULL, tvaux = NULL, sigma = 3, ttfile = NULL, trfile = NULL, tvfile = NULL, kparam = NULL, iparam = TRUE)
{
	# Fix parameter class in case of CLI string input
	if (!is.numeric(tsplit)) tsplit <- as.numeric(tsplit);
	if (!is.numeric(vsplit)) vsplit <- as.numeric(vsplit);
	if (!is.integer(sigma)) sigma <- as.integer(sigma);
	if (!is.integer(kparam) && !is.null(kparam)) kparam <- as.integer(kparam);

	# Load and split datasets
	rt <- aloja_load_datasets (ds,vin,vout,tsplit,vsplit,ttaux,ntaux,traux,tvaux,ttfile,trfile,tvfile);
	rt[["varin"]] <- vin;
	rt[["varout"]] <- vout;

	# Remove outliers (leap of faith, as vout may not be normal)
	if (rmols)
	{
		rt[["olstrain"]] <- rt$trainset[rt$trainset[,vout] > mean(rt$trainset[,vout]) + sigma * sd(rt$trainset[,vout]),];
		rt[["olsvalid"]] <- rt$validset[rt$validset[,vout] > mean(rt$validset[,vout]) + sigma * sd(rt$validset[,vout]),];
		
		rt$trainset <- rt$trainset[rt$trainset[,vout] <= mean(rt$trainset[,vout]) + sigma * sd(rt$trainset[,vout]),];
		rt$validset <- rt$validset[rt$validset[,vout] <= mean(rt$validset[,vout]) + sigma * sd(rt$validset[,vout]),];
	}

	rt[["kparam"]] <- kparam;
	rt[["iparam"]] <- iparam;

	# Training and Validation
	if (is.null(kparam))
	{
		rt[["selected_model"]] <- aloja_knn_select(vout, vin, rt$trainset, rt$validset, c("1","2","3","5","10","25","50","100"), iparam);
		kparam <- rt$selected_model$kmin;
	}
	rt[["model"]] <- IBk(formula=rt$trainset[,vout] ~ . , data = rt$trainset[,vin], control = Weka_control(K = kparam, I = iparam));
	#evaluate_Weka_classifier(rt[["model"]], numFolds = 10);
	rt[["predtrain"]] <- rt$model$predictions;
	rt[["predval"]] <- predict(rt$model,newdata=rt$validset);
	rt[["maeval"]] <- mean(abs(rt$predval - rt$validset[,vout]));
	rt[["raeval"]] <- mean(abs((rt$predval - rt$validset[,vout])/rt$validset[,vout]));

	if (!is.null(pngval))
	{
		png(paste(pngval,".png",sep=""),width=500,height=500);
		par(mfrow=c(1,2));
		plot(rt$predval,rt$validset[,vout],main=paste("K-NN K =",kparam,ifelse(iparam,"Weight = Inv.Dist.","")));
		abline(0,1);
		if (!is.null(rt$selected_model))
		{
			plot(rt$selected_model$trmae,ylim=c(min(c(rt$selected_model$trmae,rt$selected_model$tvmae)),max(rt$selected_model$trmae,rt$selected_model$tvmae)),main="Error vs K");
			points(rt$selected_model$tvmae,col="red");
			legend("topleft",pch=1,c("trmae","tvmae"),col=c("black","red"));
		}
		dev.off();
	}

	# Testing and evaluation
	rt[["predtest"]] <- predict(rt$model,newdata=rt$testset);
	rt[["maetest"]] <- mean(abs(rt$predtest - rt$testset[,vout]));
	rt[["raetest"]] <- mean(abs((rt$predtest - rt$testset[,vout])/rt$testset[,vout]));

	if (!is.null(pngtest))
	{
		png(paste(pngtest,".png",sep=""),width=1000,height=500);
		par(mfrow=c(1,2));
		plot(rt$predval,rt$validset[,vout],main=paste("Best Validation k-NN K =",kparam));
		abline(0,1);
		plot(rt$predtest,rt$testset[,vout],main=paste("Test k-NN K =",kparam));
		abline(0,1);
		dev.off();
	}

	print(c(rt$maeval,rt$raeval));
	print(c(rt$maetest,rt$raetest));

	if (!is.null(saveall))
	{
		aloja_save_model(rt$model,tagname=saveall,is.weka=TRUE);
		aloja_save_object(rt,tagname=saveall);
		aloja_save_predictions(rt$dataset,rt$trainset,rt$predtrain,rt$validset,rt$predval,rt$testset,rt$predtest,testname=saveall);
	}

	rt;
}

aloja_regtree <- function (ds, vin, vout, tsplit = 0.25, vsplit = 0.66, rmols = TRUE, pngval = NULL, pngtest = NULL, saveall = NULL, ttaux = NULL, ntaux = NULL, traux = NULL, tvaux = NULL, sigma = 3, ttfile = NULL, trfile = NULL, tvfile = NULL, mparam = NULL, exsel = NULL, prange = NULL)
{
	# Fix parameter class in case of CLI string input
	if (!is.null(prange)) prange <- as.numeric(prange);
	if (!is.numeric(tsplit)) tsplit <- as.numeric(tsplit);
	if (!is.numeric(vsplit)) vsplit <- as.numeric(vsplit);
	if (!is.integer(sigma)) sigma <- as.integer(sigma);
	if (!is.integer(mparam) && !is.null(mparam)) mparam <- as.integer(mparam);

	# Load and split datasets
	rt <- aloja_load_datasets (ds,vin,vout,tsplit,vsplit,ttaux,ntaux,traux,tvaux,ttfile,trfile,tvfile);
	rt[["varin"]] <- vin;
	rt[["varout"]] <- vout;

	# Example selection from a threshold, balancing outputs
	if (!is.null(exsel))
	{
		ntaux <- rbind(rt$trainset,rt$validset);
		upperaux <- ntaux[ntaux[,vout] > exsel,];
		loweraux <- ntaux[ntaux[,vout] <= exsel,];

		uppersel <- sample(1:nrow(upperaux),nrow(upperaux)*vsplit);
		lowersel <- sample(1:nrow(loweraux),nrow(loweraux)*vsplit);

		rt$trainset <- rbind(upperaux[uppersel,],loweraux[lowersel,]);
		rt$validset <- rbind(upperaux[-uppersel,],loweraux[-lowersel,]);
	}

	# Remove outliers (leap of faith, as vout may not be normal
	if (rmols)
	{
		rt[["olstrain"]] <- rt$trainset[rt$trainset[,vout] > mean(rt$trainset[,vout]) + sigma * sd(rt$trainset[,vout]),];
		rt[["olsvalid"]] <- rt$validset[rt$validset[,vout] > mean(rt$validset[,vout]) + sigma * sd(rt$validset[,vout]),];
		
		rt$trainset <- rt$trainset[rt$trainset[,vout] <= mean(rt$trainset[,vout]) + sigma * sd(rt$trainset[,vout]),];
		rt$validset <- rt$validset[rt$validset[,vout] <= mean(rt$validset[,vout]) + sigma * sd(rt$validset[,vout]),];
	}

	# Training and Validation
	if (is.null(mparam))
	{
		rt[["selected_model"]] <- aloja_m5p_select(vout, vin, rt$trainset, rt$validset, c("1","2","5","10","25","50","75","100","150","200"));
		mparam <- rt$selected_model$mmin;
	}
	rt[["model"]] <- M5P(formula=rt$trainset[,vout] ~ .,data=data.frame(rt$trainset[,vin]), control = Weka_control(M = mparam));
	rt[["predtrain"]] <- rt$model$predictions;
	rt[["predval"]] <- predict(rt$model,newdata=data.frame(rt$validset));
	if (!is.null(prange))
	{
		rt$predtrain[rt$predtrain < prange[1]] <- prange[1];
		rt$predtrain[rt$predtrain > prange[2]] <- prange[2];
		rt$predval[rt$predval < prange[1]] <- prange[1];
		rt$predval[rt$predval > prange[2]] <- prange[2];
	}
	rt[["maeval"]] <- mean(abs(rt$predval - rt$validset[,vout]));
	rt[["raeval"]] <- mean(abs((rt$predval - rt$validset[,vout])/rt$validset[,vout]));

	if (!is.null(pngval))
	{
		png(paste(pngval,".png",sep=""),width=1000,height=500);
		par(mfrow=c(1,2));
		plot(rt$predval,rt$validset[,vout],main=paste("Best Validation M5P M = ",mparam));
		abline(0,1);
		if (!is.null(rt$selected_model))
		{
			plot(rt$selected_model$trmae,ylim=c(min(c(rt$selected_model$trmae,rt$selected_model$tvmae)),max(rt$selected_model$trmae,rt$selected_model$tvmae)),main="Error vs M");
			points(rt$selected_model$tvmae,col="red");
			legend("topleft",pch=1,c("trmae","tvmae"),col=c("black","red"));
		}
		dev.off();
	}

	# Testing and evaluation
	rt[["predtest"]] <- predict(rt$model,newdata=data.frame(rt$testset));
	if (!is.null(prange))
	{
		rt$predtest[rt$predtest < prange[1]] <- prange[1];
		rt$predtest[rt$predtest > prange[2]] <- prange[2];
	}
	rt[["maetest"]] <- mean(abs(rt$predtest - rt$testset[,vout]));
	rt[["raetest"]] <- mean(abs((rt$predtest - rt$testset[,vout])/rt$testset[,vout]));

	if (!is.null(pngtest))
	{
		png(paste(pngtest,".png",sep=""),width=1000,height=500);
		par(mfrow=c(1,2));
		plot(rt$predval,rt$validset[,vout],main=paste("Best Validation M5P M = ",rt$selected_model$mmin));
		abline(0,1);
		plot(rt$predtest,rt$testset[,vout],main=paste("Test M5P M = ",rt$selected_model$mmin));
		abline(0,1);
		dev.off();
	}

	print(c(rt$maeval,rt$raeval));
	print(c(rt$maetest,rt$raetest));

	if (!is.null(saveall))
	{
		aloja_save_model(rt$model,tagname=saveall,is.weka=TRUE);
		aloja_save_object(rt,tagname=saveall);
		aloja_save_predictions(rt$dataset,rt$trainset,rt$predtrain,rt$validset,rt$predval,rt$testset,rt$predtest,testname=saveall);
	}

	rt;
}

###############################################################################
# Predicting methods                                                          #
###############################################################################

aloja_predict_dataset <- function (learned_model, vin, ds = NULL, data_file = NULL)
{
	retval <- NULL;
	if (is.null(ds) && is.null(data_file))
	{
		retval;
	}

	ds <- ds[,vin];
	if (!is.null(data_file))
	{
		fileset <- read.table(file=data_file,header=T,sep=",");
		ds <- aloja_dbind(ds,fileset[,vin]);
	}

	for (i in 1:nrow(ds))
	{
		pred_aux <- aloja_predict_individual_instance (learned_model, vin, ds[i,]);
		retval <- c(retval, pred_aux);
	}

	retval;
}

aloja_predict_instance <- function (learned_model, vin, inst_predict, sorted = NULL)
{
	retval <- NULL;

	if (length(grep(pattern="\\||\\*",inst_predict)) > 0)
	{

		expression <- inst_predict;
		plist <- list();
		for (i in 1:length(expression))
		{
			if (expression[i]=="*")
			{
				caux <- learned_model$dataset[,vin[i]];
				if (class(caux)=="factor") plist[[i]] <- levels(caux);
				if (class(caux)=="integer")
				{
					print(paste("[WARNING] * in",i,"is integer. Unique values from learned_model dataset will be used.",sep=" "));
					plist[[i]] <- unique(caux);
				}

			} else if (grepl('[|]',expression[i]) == TRUE)
			{
				plist[[i]] <- strsplit(expression[i],split='\\|')[[1]];
			} else
			{
				plist[[i]] <- expression[i];
			}
		}
		instances <- expand.grid(plist);
		colnames(instances) <- vin;
	
		for(cname in vin)
		{
			if (class(learned_model$dataset[,cname])=="integer") instances[,cname] <- as.integer(as.character(instances[,cname]));
			if (class(learned_model$dataset[,cname])=="factor") instances[,cname] <- factor(instances[,cname],levels=levels(learned_model$dataset[,cname]));
		}

		laux <- list();
		for (i in 1:nrow(instances))
		{
			pred_aux <- aloja_predict_individual_instance (learned_model, vin, instances[i,]);
			laux[[i]] <- c(paste(sapply(instances[i,],function(x) as.character(x)),collapse=","),pred_aux);
		}
		daux <- t(as.data.frame(laux));
		daux <- data.frame(Instance=as.character(daux[,1]),Prediction=as.numeric(daux[,2]),stringsAsFactors=FALSE);
		if (is.null(sorted) || !(sorted %in% c("asc","desc")))
		{
			retval <- daux;
		} else {
			retval <- daux[order(daux[,"Prediction"],decreasing=(sorted=="desc")),];
		}

	} else {
		retval <- aloja_predict_individual_instance (learned_model, vin, inst_predict);
	}
	retval;
}

aloja_predict_individual_instance <- function (learned_model, vin, inst_predict)
{
	ds <- learned_model$dataset;
	model_aux <- learned_model$model;

	inst_aux <- inst_predict;
	if (!is.data.frame(inst_aux))
	{
		inst_aux <- t(as.matrix(inst_aux));
		colnames(inst_aux) <- vin;
	}

	datamodel <- ds[1,learned_model$varin];
	if (class(model_aux)[1]=="lm" || class(model_aux)[1]=="nnet")
	{
		for (name_1 in colnames(datamodel))
		{
			if (name_1 %in% colnames(inst_aux))
			{
				value_aux <- inst_aux[1,name_1];
				class(value_aux) <- class(datamodel[1,name_1]);

				if (class(model_aux)[1]=="nnet")
				{
					value_aux <- (value_aux - learned_model$minout[name_1,]) / learned_model$maxout[name_1,];
				}
				datamodel[1,name_1] <- value_aux;

			} else {
				datamodel[1,name_1] <- 0;
				for (name_2 in colnames(inst_aux))
				{
					if (inst_aux[,name_2] == name_1) datamodel[1,name_1] <- 1;
				}
			}
		}
	} else {
		for (name_1 in colnames(datamodel))
		{
			if (class(datamodel[1,name_1]) == "factor")
			{
				datamodel[1,name_1] <- factor(inst_aux[1,name_1],levels=levels(datamodel[,name_1]));
			} else {
				var_aux <- inst_aux[1,name_1];
				class(var_aux) <- class(datamodel[1,name_1]);
				datamodel[1,name_1] <- var_aux;
			}
		}
	}

	options(warn=-1);

	retval <- predict(model_aux,newdata=data.frame(datamodel));
	if (class(model_aux)[1]=="nnet")
	{
		retval <- (retval * learned_model$maxout[learned_model$varout,]) + learned_model$minout[learned_model$varout,];
	}
	as.vector(retval);
}

###############################################################################
# Fine-tunning parameters for Learning Algorithms                             #
###############################################################################

aloja_m5p_select <- function (vout, vin, traux, tvaux, mintervals)
{
	trmae <- NULL;
	tvmae <- NULL;
	mmin <- 0;
	mminmae <- 9e+15;
	off_threshold <- 1e-4;
	for (i in mintervals)
	{
		ml <- M5P(formula=traux[,vout] ~ .,data=data.frame(traux[,vin]), control = Weka_control(M = i));
		mae <- mean(abs(ml$predictions - traux[,vout]));
		trmae <- c(trmae,mae);

		prediction <- predict(ml,newdata=data.frame(tvaux));
		mae <- mean(abs(prediction - tvaux[,vout]));
		tvmae <- c(tvmae,mae);

		if (mae < mminmae - off_threshold) { mmin <- i; mminmae <- mae;	}
	}
	print (paste("Selected M:",mmin));	

	retval <- list();
	retval[["trmae"]] <- trmae;
	retval[["tvmae"]] <- tvmae;
	retval[["mmin"]] <- mmin;
	retval[["mintervals"]] <- mintervals;
	
	retval;
}

aloja_knn_select <- function (vout, vin, traux, tvaux, kintervals, iparam)
{
	trmae <- NULL;
	tvmae <- NULL;
	kmin <- 0;
	kminmae <- 9e+15;
	off_threshold <- 1e-4;
	for (i in kintervals)
	{
		ml <- IBk(formula=traux[,vout] ~ .,data=data.frame(traux[,vin]), control = Weka_control(K = i, I = iparam));
		mae <- mean(abs(ml$predictions - traux[,vout]));
		trmae <- c(trmae,mae);

		prediction <- predict(ml,newdata=data.frame(tvaux));
		mae <- mean(abs(prediction - tvaux[,vout]));
		tvmae <- c(tvmae,mae);

		if (mae < kminmae - off_threshold) { kmin <- i; kminmae <- mae; }
	}
	print (paste("Selected K:",kmin));	

	retval <- list();
	retval[["trmae"]] <- trmae;
	retval[["tvmae"]] <- tvmae;
	retval[["kmin"]] <- kmin;
	retval[["kintervals"]] <- kintervals;
	retval[["inverse"]] <- iparam;
	
	retval;
}

###############################################################################
# Principal Component Analysis methods                                        #
###############################################################################

aloja_pca <- function (ds, vin, vout, pngpca = NULL, saveall = NULL)
{
	vinaux <- vin;

	dsbin <- aloja_binarize_ds(ds[,c(vout,vin)]);
	vin <- colnames(dsbin[,-1]);

	pc <- princomp(dsbin[,vin]);
	pc[["dataset"]] <- cbind(dataset[,"ID"],dsbin);
	colnames(pc$dataset) <- c("ID",colnames(dsbin));
	pc[["pcaset"]] <- cbind(dataset[,"ID"],dsbin[,vout],pc$scores);
	colnames(pc$pcaset) <- c("ID",vout,colnames(pc$scores));

	pc[["vin_orig"]] <- vinaux;
	pc[["vin"]] <- vin;
	pc[["vout"]] <- vout;

	if (!is.null(pngpca))
	{
		system("mkdir -p temp");
		for (var1 in 1:(length(pc$scores[1,])-1))
		{
			for (var2 in (var1 + 1):ncol(pc$scores))
			{
				png(paste("temp/",pngpca,"-",var1,"-",var2,".png",sep=""),width=1000,height=500);
					plot(1, type="n", xlim=c(min(pc$scores[,var1]),max(pc$scores[,var1])),ylim=c(min(pc$scores[,var2]),max(pc$scores[,var2])), xlab="", ylab="");
					points(jitter(pc$scores[dsbin[,vout]>3000,var1],factor=0.6),jitter(pc$scores[dsbin[,vout]>3000,var2],factor=0.6),col="red");
					points(jitter(pc$scores[dsbin[,vout]>2000 & dsbin[,vout]<3000,var1],factor=0.6),jitter(pc$scores[dsbin[,vout]>2000 & dsbin[,vout]<3000,var2],factor=0.6),col="blue");
					points(jitter(pc$scores[dsbin[,vout]>1000 & dsbin[,vout]<2000,var1],factor=0.6),jitter(pc$scores[dsbin[,vout]>1000 & dsbin[,vout]<2000,var2],factor=0.6),col="green");
					points(jitter(pc$scores[dsbin[,vout]<1000,var1],factor=0.6),jitter(pc$scores[dsbin[,vout]<1000,var2],factor=0.6),col="black");
				dev.off();
			}
		}
	}

	if (!is.null(saveall))
	{
		write.table(pc$dataset, file = paste(saveall,"-dataset.csv",sep=""), sep = ",", row.names=FALSE);
		write.table(pc$pcaset, file = paste(saveall,"-transformed.csv",sep=""), sep = ",", row.names=FALSE);
		aloja_save_object(pc,tagname=saveall);
	}

	pc;
}

aloja_transform_data <- function (ds, pca_obj = NULL, pca_name = NULL, saveall = NULL)
{
	retval <- NULL;
	
	if (is.null(pca_name) && is.null(pca_obj))
	{
		print("[WARNING] No PCA object or file introduced");
		retval;
	}

	if (!is.null(pca_name))	pca_obj <- aloja_load_object(pca_name);

	dsbaux <- aloja_binarize_ds(ds[,pca_obj$vin_orig]);
	vin <- colnames(dsbaux);

	retval[["dataset"]] <- dsbaux;
	retval[["vin"]] <- vin;
	retval[["pca"]] <- pca_obj;

	dspca <- predict(pca_obj,dsbaux);
	retval[["pcaset"]] <- dspca;	

	if (!is.null(saveall))
	{
		write.table(dspca, file = paste(saveall,"-transformed.csv",sep=""), sep = ",", row.names=FALSE);
	}

	retval;
}

aloja_transform_instance <- function (inst_transform, pca_obj = NULL, pca_name = NULL, verbose = 0)
{
	retval <- NULL;
	
	if (is.null(pca_name) && is.null(pca_obj))
	{
		print("[WARNING] No PCA object or file introduced");
		retval;
	}

	if (!is.null(pca_name)) pca_obj <- aloja_load_object(pca_name);

	datamodel <- pca_obj$dataset[1,pca_obj$vin];
	for (name_1 in colnames(datamodel))
	{
		if (name_1 %in% pca_obj$vin_orig)
		{
			var_aux <- inst_transform[which(pca_obj$vin_orig==name_1)];
			class(var_aux) <- class(datamodel[1,name_1]);
			datamodel[1,name_1] <- var_aux;
		} else {
			if (length(which(inst_transform==name_1)) >= 1)
			{
				datamodel[1,name_1] <- 1;
			} else {
				datamodel[1,name_1] <- 0;
			}
		}
	}
	vin <- colnames(datamodel);

	retval[["instance"]] <- datamodel;
	retval[["vin"]] <- vin;
	retval[["pca"]] <- pca_obj;

	dspca <- predict(pca_obj,datamodel);
	retval[["pcainst"]] <- dspca;

	if (verbose == 2)
	{
		retval;
	} else if (verbose == 1) {
		retval$pcainst;
	} else {
		paste(retval$pcainst,collapse=",");
	}
}


aloja_dataset_collapse <- function (ds, vin, vout, dimension1, dimension2, dimname1, dimname2, model_obj = NULL, model_name = NULL, saveall = NULL)
{
	retval <- list();

	dsid <- ds[,"ID"];
	ds <- ds[,c(vout,vin)];
	dsaux <- cbind(ds[,vout],apply(as.matrix(ds[,dimension1]),1,paste,collapse=":"),apply(as.matrix(ds[,dimension2]),1,paste,collapse=":"));
	colnames(dsaux) <- c(vout,dimname1,dimname2);

	maux <- matrix(NA,length(levels(as.factor(dsaux[,dimname1]))),length(levels(as.factor(dsaux[,dimname2]))));
	colnames(maux) <- levels(as.factor(dsaux[,dimname2]));
	rownames(maux) <- levels(as.factor(dsaux[,dimname1]));

	midaux <- matrix(NA,length(levels(as.factor(dsaux[,dimname1]))),length(levels(as.factor(dsaux[,dimname2]))));
	colnames(midaux) <- levels(as.factor(dsaux[,dimname2]));
	rownames(midaux) <- levels(as.factor(dsaux[,dimname1]));

	for (i in 1:nrow(dsaux))
	{
		dim1_aux <- dsaux[i,dimname1];
		dim2_aux <- dsaux[i,dimname2];

		midaux[dim1_aux,dim2_aux] <- ifelse(!is.na(midaux[dim1_aux,dim2_aux]),c(midaux[dim1_aux,dim2_aux], dsid[i]),dsid[i]);
		len_aux <- length(midaux[dim1_aux,dim2_aux]);

		prev_val <- ifelse(!is.na(maux[dim1_aux,dim2_aux]),as.numeric(maux[dim1_aux,dim2_aux]),0);
		maux[dim1_aux,dim2_aux] <- (prev_val * (len_aux-1) + as.numeric(dsaux[i,vout])) / len_aux;
	}

	# IMPORTANT - NA filling is done AFTER aggregation, so estimated values DO NOT affect aggregation
	# also, model filling only works when all represented dimensions are in the original learning dataset
	if (!is.null(model_name)) model_obj <- aloja_load_object(model_name);

	if (!is.null(model_obj))
	{
		for (i in 1:length(maux))
		{
			if (is.na(maux[i]))
			{
				row_aux <- ((i-1) %% nrow(maux)) + 1;
				col_aux <- ((i-1) %/% nrow(maux)) + 1;

				dim1_aux <- rownames(maux)[row_aux];
				dim2_aux <- colnames(maux)[col_aux];

				inst_aux <- c(strsplit(dim1_aux,split=":")[[1]],strsplit(dim2_aux,split=":")[[1]]);

				maux[i] <- aloja_predict_instance (model_obj,model_obj$varin,inst_aux);
			}
		}
	}

	retval[["matrix"]] <- maux;
	retval[["IDs"]] <- midaux;
	retval[["collapsed"]] <- dimension2;

	if (!is.null(saveall))
	{
		write.table(retval$matrix,file=paste(saveall,"-matrix.csv",sep=""),sep=",",row.names=TRUE);
		write.table(retval$IDs,file=paste(saveall,"-ids.csv",sep=""),sep=",",row.names=TRUE);
		aloja_save_object(retval,tagname=saveall);
	}

	retval;
}

aloja_dataset_clustering <- function (datamatrix, k = 3, na.predict = NULL)
{
	if (class(datamatrix) == "character")
	{
		maux <- as.matrix(read.csv(file=datamatrix, header=TRUE, sep=",", check.names=FALSE));
	} else {
		maux <- datamatrix;
	}

	if (!is.null(na.predict))
	{
		for (i in 1:length(maux))
		{
			if (is.na(maux[i]))
			{
				row_aux <- ((i-1) %% nrow(maux)) + 1;
				col_aux <- ((i-1) %/% nrow(maux)) + 1;

				dim1_aux <- rownames(maux)[row_aux];
				dim2_aux <- colnames(maux)[col_aux];

				inst_aux <- c(dim1_aux,strsplit(dim2_aux,split=":")[[1]]);
			
				maux[i] <- aloja_predict_instance (na.predict,na.predict$varin,inst_aux);
			}
		}
	} else {
		maux[is.na(maux)] <- 0;
	}
	retval <- kmeans(maux, k);

	retval;
}

###############################################################################
# Characterization methods                                                    #
###############################################################################

aloja_check_similarity <- function (ds_new, ds_gral, vin, vout, var.base, gmodel = NULL, lmodel = NULL, alpha = 0.05)
{
	retval <- list();

	anova1 <- list();
	for (i in levels(ds_gral[,var.base]))
	{
		if (!is.null(gmodel)) model_aux <- gmodel;
		if (!is.null(lmodel[[i]])) model_aux <- lmodel[[i]];
		if (is.null(model_aux))
		{
			print(paste("[ERROR] No suitable model found"));
			NULL;
		}

		ds_aux <- ds_new;
		ds_aux[,var.base] <- i;
		paux1 <- aloja_predict_dataset(learned_model=model_aux,vin=varin,ds=ds_aux[,vin]);
		paux2 <- aloja_predict_dataset(learned_model=model_aux,vin=varin,ds=ds_gral[ds_gral[,var.base]==i,vin]);

		perr1 <- paux1 - ds_aux[,vout];
		perr2 <- paux2 - ds_gral[ds_gral[,var.base]==i,vout];
	
		perry <- c(perr1,perr2);
		group <- rep(1:2,c(length(perr1),length(perr2)));
		daux = data.frame(y = perry, group = factor(group));
		anova1[[i]] <- anova(lm(perry~group,daux));
	}

	result <- t(sapply(lapply(anova1,function(x) x$"Pr(>F)"[1]),unlist));
	retval[["kind"]] <- "Prediction";
	retval[["anova"]] <- anova1;
	retval[["result"]] <- result;

	if (max(result) > alpha)
	{
		retval[["cluster"]] <- colnames(result)[which.max(result)];
		retval[["significance"]] <- max(result);
	}
	retval;
}

aloja_check_cluster <- function (kcluster, bmk.vector) # FIXME - Problema de distàncies amb 2 bmks només
{
	retval <- list();

	distances <- NULL;
	gen_distances <- NULL;
	for (i in 1:nrow(kcluster$centers))
	{
		distances <- c(distances,(sum((kcluster$centers[i,] - bmk.vector$matrix[1,])^2))^(1/2));

		for (j in i:nrow(kcluster$centers))
		{
			if (j == i) next;
			gen_distances <- c(gen_distances,(sum((kcluster$centers[i,] - kcluster$centers[j,])^2))^(1/2));
		}
	}

	retval[["kind"]] <- "Cluster";
	retval[["full.distances"]] <- gen_distances;
	retval[["distances"]] <- distances;

	if (max(gen_distances) > min(distances))
	{
		retval[["cluster"]] <- colnames(result)[which(kcluster$cluster == which.min(distances))];
		retval[["min.distance"]] <- min(distances);
	}
	retval;
}

aloja_best_configurations <- function (bvectors = NULL, bvec_name = NULL)
{
	if (is.null(bvectors) && is.null(bvec_name))
	{
		print("[WARNING] No Vector Matrix object or file introduced");
		NULL;
	}

	if (!is.null(bvec_name)) bvectors <- aloja_load_object(bvec_name);

	result <- data.frame(Config=character(),Variance=numeric(),Benchmarks=integer(),Missing=character(),stringsAsFactors=FALSE);
	for(i in 1:ncol(bvectors$matrix))
	{
		vaux <- var(bvectors$matrix[,i],na.rm=TRUE);
		raux <- is.na(bvectors$matrix[,i]);
		result <- rbind(result,data.frame(Config=colnames(bvectors$matrix)[i],Variance=vaux,Benchmarks=length(raux[raux==FALSE]),Missing=paste(names(raux[raux==TRUE]),collapse=",")));
	}

	colnames(result)[1] <- paste(bvectors$collapsed,collapse=":");
	result[order(-result[,2], -result[,3]),];
}

###############################################################################
# Save the datasets and created models                                        #
###############################################################################

aloja_save_predictions <- function (ds, trdata, trpred, tvdata, tvpred, ttdata, ttpred, testname = "default")
{
	traux <- cbind(trdata,trpred);
	tvaux <- cbind(tvdata,tvpred);
	ttaux <- cbind(ttdata,ttpred);

	colnames(traux) <- c(colnames(trdata),"Pred.Exe.Time");
	colnames(tvaux) <- c(colnames(tvdata),"Pred.Exe.Time");
	colnames(ttaux) <- c(colnames(ttdata),"Pred.Exe.Time");

	write.table(ds, file = paste(testname,"-ds.csv",sep=""), sep = ",", row.names=FALSE);

	write.table(traux, file = paste(testname,"-tr.csv",sep=""), sep = ",", row.names=FALSE);
	write.table(tvaux, file = paste(testname,"-tv.csv",sep=""), sep = ",", row.names=FALSE);
	write.table(ttaux, file = paste(testname,"-tt.csv",sep=""), sep = ",", row.names=FALSE);
}

aloja_save_datasets <- function (traux_0, tvaux_0, ttaux_0, name_0, algor_0)
{
	write.table(tvaux_0, file = paste(algor_0,"-",name_0,"-tv.csv",sep=""), sep = ",");
	write.table(traux_0, file = paste(algor_0,"-",name_0,"-tr.csv",sep=""), sep = ",");
	write.table(ttaux_0, file = paste(algor_0,"-",name_0,"-tt.csv",sep=""), sep = ",");
}

aloja_save_model <- function (model_0, tagname = "default", is.weka = FALSE)
{
	if (is.weka) rJava::.jcache(model_0$classifier);
	saveRDS(model_0,file=paste(tagname,"-model.dat",sep=""));
}

aloja_save_object <- function (object_1, tagname = "default", is.weka = FALSE)
{
	if (!is.null(object_1$model) && is.weka) rJava::.jcache(object_1$model$classifier);
	saveRDS(object_1,file=paste(tagname,"-object.rds",sep=""));
}

aloja_load_model <- function (tagname = "default", is.weka = FALSE)
{
	model_1 <- readRDS(paste(tagname,"-model.dat",sep=""));
	##if (is.weka) rJava::.jstrVal(model_1$classifier);
	model_1;
}

aloja_load_object <- function (tagname = "default", is.weka = FALSE)
{
	object_1 <- readRDS(paste(tagname,"-object.rds",sep=""));
	##if (!is.null(object_1$model) && is.weka) rJava::.jstrVal(object_1model$classifier);
	object_1;
}

aloja_save_status <- function ()
{
	save.session("RData");
	savehistory("Rhistory");
	system(paste("tar cvzf rsession-",Sys.Date(),".tar.gz RData Rhistory",sep=""));
	system("rm RData Rhistory");
}

###############################################################################
# R hacks and operators                                                       #
###############################################################################

':=' <- function(lhs, rhs)
{
	frame <- parent.frame();
	lhs <- as.list(substitute(lhs));
	if (length(lhs) > 1)
		lhs <- lhs[-1];
	if (length(lhs) == 1)
	{
		do.call(`=`, list(lhs[[1]], rhs), envir=frame);
		return(invisible(NULL));
	}
	if (is.function(rhs) || is(rhs, 'formula'))
		rhs <- list(rhs);
	if (length(lhs) > length(rhs))
		rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)));
	for (i in 1:length(lhs))
		do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame);
	return(invisible(NULL));
}

aloja_dbind <- function (dataframe_1, dataframe_2)
{
	retval <- data.frame(rep(0,nrow(dataframe_1) + nrow(dataframe_2)));
	cnames <- NULL;
	for (name_1 in colnames(dataframe_1))
	{
		vec_aux <- NULL;
		if (!is.null(dataframe_2[,name_1]))
		{
			cnames <- c(cnames,name_1);
			if (class(dataframe_1[,name_1]) == "factor" || class(dataframe_2[,name_1]) == "factor")
			{
				vec_aux <- c(as.character(dataframe_1[,name_1]),as.character(dataframe_2[,name_1]));
				vec_aux <- as.factor(vec_aux);
			} else if (class(dataframe_1[,name_1]) == "integer" || class(dataframe_2[,name_1]) == "integer") {
				vec_aux <- as.integer(c(dataframe_1[,name_1],dataframe_2[,name_1]));
			} else {
				vec_aux <- c(dataframe_1[,name_1],dataframe_2[,name_1]);
			}
			retval <- data.frame(retval,vec_aux);
		}
	}
	retval <- retval[,-1];
	colnames(retval) <- cnames;
	retval;
}

shannon.entropy <- function(p)
{
	if (min(p) < 0 || sum(p) <= 0) return(NA);
	p.norm <- p[p > 0]/sum(p);
	-sum(log2(p.norm)*p.norm);
}

