#!/usr/bin/env Rscript

# Josep Ll. Berral-García
# ALOJA-BSC-MSR hadoop.bsc.es
# 2014-11-24
# Scripts and snippets for ALOJA-ML

source("functions.r");
options(width=as.integer(Sys.getenv("COLUMNS")));

###############################################################################
# Read datasets and prepare them for usage                                    #
###############################################################################

	dataset <- aloja_get_data(fread = "aloja-dataset.csv", cds = FALSE, hds = FALSE, fproc = "aloja-process");

	varout <- "Exe.Time";
	varin <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");

	aloja_print_summaries(fprint="output.txt", ds=dataset, ds=dataset[,c(varout,"Running.Cost..",varin)], fwidth = 1000, ms = 10, sname = "Benchmark");

###############################################################################
# Relation among input ~ output variables                                     #
###############################################################################

	aloja_crossvariables(dataset[,c(varout,varin)], jfactor=0.1);

###############################################################################
# ANOVA of current variables                                                  #
###############################################################################

	anova_1 <- aloja_anova(dataset);

###############################################################################
# Learning from the variables                                                 #
###############################################################################

###############################################################################
# Regression Trees

	#######################################################################
	## Training M5P without example selection

	#m5p1 <- aloja_regtree(dataset,vin=varin,vout=varout);
	m5p1 <- aloja_regtree(dataset,vin=varin,vout=varout,saveall="m5p-simple",pngval="m5p-simple-app",pngtest="m5p-simple-test");

	#######################################################################
	## Training M5P with example selection

	#m5p2 <- aloja_regtree(dataset,vin=varin,vout=varout,ttaux=m5p1$testset,exsel=8000);
	m5p2 <- aloja_regtree(dataset,vin=varin,vout=varout,ttaux=m5p1$testset,exsel=8000,saveall="m5p-simple",pngval="m5p-exsel-app",pngtest="m5p-exsel-test");

	#######################################################################
	## Training M5P with benchmark separation

	m5px <- list();
	for (name in levels(dataset[,"Benchmark"]))
	{
		baux <- dataset[dataset[,"Benchmark"]==name,];
		taux <- m5p1$testset[m5p1$testset[,"Benchmark"]==name,];

		#m5px[[name]] <- aloja_regtree(ds=baux,vin=varin,vout=varout,ttaux=taux);
		m5px[[name]] <- aloja_regtree(ds=baux,vin=varin,vout=varout,ttaux=taux,saveall=paste("m5p-benchmark",name,sep="-"),pngval=paste("m5p-benchmark",name,"val",sep="-"),pngtest=paste("m5p-benchmark",name,"test",sep="-"));
	}
	rm (baux,taux,name);

###############################################################################
# Nearest Neighbors

	#ibk1 <- aloja_nneighbors(dataset,varin,varout,ttaux=m5p1$testset);
	ibk1 <- aloja_nneighbors(dataset,vin=varin,vout=varout,ttaux=m5p1$testset,saveall="ibk-simple",pngval="ibk-simple-app",pngtest="ibk-simple-test");

###############################################################################
# Other Regression Methods

	#######################################################################
	## LinReg (Binarized & Polynomial)

	#pr3 <- aloja_linreg(dataset,vin=varin,vout=varout,ttaux=m5p1$testset,ppoly=3);
	pr3 <- aloja_linreg(dataset,vin=varin,vout=varout,ttaux=m5p1$testset,ppoly=3,saveall="linreg-polynom3",pngval="linreg-polynom3-app",pngtest="linreg-polynom3-test");

	par(mfrow=c(1,2));
	plot(pr3$predval,pr3$validset[,varout],main=paste("Polynomial Regression power =",pr3$ppoly));
	abline(0,1);
	plot(pr3$predtest,pr3$testset[,varout],main=paste("Test Polynomial Regression power =",pr3$ppoly));
	abline(0,1);
	points(pr3$predtest[rownames(pr3$testset) %in% rownames(pr3$testset[pr3$testset[,"dfsioe_read"]==1,])],pr3$testset[rownames(pr3$testset) %in% rownames(pr3$testset[pr3$testset[,"dfsioe_read"]==1,]),1],col="red");


	#######################################################################
	## Neural Networks

	#nn1 <- aloja_nnet(dataset,vin=varin,vout=varout,ttaux=m5p1$testset);
	nn1 <- aloja_nnet(dataset,vin=varin,vout=varout,ttaux=m5p1$testset,hlayers=5,saveall="nnet-32-5-1",pngval="nnet-32-5-1-app",pngtest="nnet-32-5-1-test"); 

###############################################################################
# Dimensional Techniques                                                      #
###############################################################################

###############################################################################
# Principal Components Analysis

	pca1 <- aloja_pca(dataset,vin=varin,vout=varout,pngpca="pca");
	pca1$loadings;

	#######################################################################
	## LinReg (with reduced dimension)

	#pr3dim <- aloja_linreg(pca1$pcaset,vin=colnames(pca1$pcaset)[3:22],vout=varout,ppoly=3,prange=c(1e-4,1e+4));
	pr3dim <- aloja_linreg(pca1$pcaset,vin=colnames(pca1$pcaset)[3:22],vout=varout,ppoly=3,prange=c(1e-4,1e+4),saveall="polynom3 redim",pngval="linreg-polynom3-redim-app",pngtest="linreg-polynom3-redim-test");

	par(mfrow=c(1,2));
	plot(pr3dim$predval,pr3dim$validset[,varout],main=paste("Polynomial Regression power =",pr3dim$ppoly));
	abline(0,1);
	plot(pr3dim$predtest,pr3dim$testset[,varout],main=paste("Test Polynomial Regression power =",pr3dim$ppoly));
	abline(0,1);
	points(pr3dim$predtest[rownames(pr3dim$testset) %in% rownames(pca1$dataset[pca1$dataset[,"dfsioe_read"]==1,])],pr3dim$testset[rownames(pr3dim$testset) %in% rownames(pca1$dataset[pca1$dataset[,"dfsioe_read"]==1,]),1],col="red");


	#######################################################################
	## Training M5P (with reduced dimension)

	#m5p1dim <- aloja_regtree(pca1$pcaset,vin=colnames(pca1$pcaset)[3:22],vout=varout,prange=c(1e-4,1e+4));
	m5p1dim <- aloja_regtree(pca1$pcaset,vin=colnames(pca1$pcaset)[3:22],vout=varout,prange=c(1e-4,1e+4),saveall=c("simple redim","m5p"),pngval="m5p-simple-redim-app",pngtest="m5p-simple-redim-test");

	par(mfrow=c(1,2));
	plot(m5p1dim$predval,m5p1dim$validset[,varout],main=paste("Best Validation M5P (Red.Dim.) M = ",m5p1dim$mmin));
	abline(0,1);
	plot(m5p1dim$predtest,m5p1dim$testset[,varout],main=paste("Test M5P (Red.Dim.) M = ",m5p1dim$mmin));
	abline(0,1);
	points(m5p1dim$predtest[rownames(m5p1dim$testset) %in% rownames(pca1$dataset[pca1$dataset[,"dfsioe_read"]==1,])],m5p1dim$testset[rownames(m5p1dim$testset) %in% rownames(pca1$dataset[pca1$dataset[,"dfsioe_read"]==1,]),1],col="red");

###############################################################################
# Dataset and Benchmark Caracterization                                       #
###############################################################################

###############################################################################
# Benchmark - Configuration Matrix

	dsc1 <- aloja_dataset_collapse (dataset,varin,varout,dimension1="Benchmark",dimension2=c(3:11),dimname1="Benchmark",dimname2="Configuration",saveall="dsc1");

	plot(dsc1$matrix["bayes",],ylim=c(0,10000));
	points(dsc1$matrix["kmeans",],col="red");
	points(dsc1$matrix["terasort",],col="green");
	points(dsc1$matrix["sort",],col="blue");
	points(dsc1$matrix["wordcount",],col="orange");
	points(dsc1$matrix["pagerank",],col="yellow");
	points(dsc1$matrix["dfsioe_write",],col="gray");

###############################################################################
# Clustering

	#######################################################################
	## Clustering with NA <- 0

	#kc1 <- aloja_dataset_clustering(datamatrix=dsc1$matrix,k=3);
	kc1 <- aloja_dataset_clustering(datamatrix="dsc1-matrix.csv",k=8);

	#######################################################################
	## Clustering with NA <- prediction

	m5p3 <- aloja_regtree(dataset,vin=varin,vout=varout); model_aux <- m5p3;
	ibk2 <- aloja_nneighbors(dataset,vin=varin,vout=varout); model_aux <- ibk2;
	pr31 <- aloja_linreg(dataset,vin=varin,vout=varout,ppoly=3); model_aux <- pr31;
	nn2 <- aloja_nnet(dataset,vin=varin,vout=varout); model_aux <- nn2;

	kc2 <- aloja_dataset_clustering(datamatrix=dsc1$matrix,k=3,na.predict=model_aux);

###############################################################################
# Classification of New Benchmarks

# TODO FIXME - Refactor the following code:

	# Preparing dummy scenario
	dsaux_sort <- dataset[dataset[,"Benchmark"]=="sort",];
	dsaux_sort$Benchmark <- dsaux_sort$Benchmark[,drop=TRUE];
	dsaux_other <- dataset[dataset[,"Benchmark"]!="sort",];
	dsaux_other$Benchmark <- dsaux_other$Benchmark[,drop=TRUE];

	dsc2 <- aloja_dataset_collapse (dsaux_other,varin,varout,dimension1="Benchmark",dimension2=c(3:11),dimname1="Benchmark",dimname2="Configuration",saveall="dsc2");
	m5p4 <- aloja_regtree(dsaux_other,vin=varin,vout=varout,saveall="m5p-dummy-cluster");
	kc4 <- aloja_dataset_clustering(datamatrix=dsc2$matrix,k=8,na.predict=m5p4);

	caux <- kc4$centers;
	nb_name <- "sort";
	cluster_pred <- TRUE;

	#######################################################################
	## Recomend experiments to run

	if (!(nb_name %in% rownames(dsc2$matrix)))
	{
		# Select best configurations to compare with existing benchmarks
		result <- data.frame(Config=character(),Variance=numeric(),Benchmarks=integer(),stringsAsFactors=FALSE);
		for(i in 1:ncol(dsc2$matrix))
		{
			vaux <- var(dsc2$matrix[,i],na.rm=TRUE);
			raux <- is.na(dsc2$matrix[,i]);
			result <- rbind(result,data.frame(Config=colnames(dsc2$matrix)[i],Variance=vaux,Benchmarks=length(raux[raux==FALSE])));
		}
		sresult <- result[order(-result[,2], -result[,3]),];

		# Show the best configurations to test the new
		nbests <- 10;
		sresult[1:nbests,];
	}
	# DO THE HADOOP EXPERIMENTS NOW

	#######################################################################
	## Classify new Benchmark

	if (!(nb_name %in% rownames(dsc2$matrix)))
	{
		# Get characterization vector on new benchmark
		dsc21 <- aloja_dataset_collapse (dsaux_sort,varin,varout,dimension1="Benchmark",dimension2=c(3:11),dimname1="Benchmark",dimname2="Configuration",saveall="dsc21");
		
		###############################################################
		## Use clustering/categorization to check which "existing 
		## benchmark" is more alike
		if (cluster_pred == FALSE)
		{
			## For each existing benchmark

				## Match configurations and calculate dstance

				## If lower distance, keep as candidate benchmark

			## Check distance to candidate

				## If higher that maximum distance among clusters -> New cluster

				## Else new benchmark -> belongs to candidate cluster


			## If new benchmark -> Update Model
		}

		###############################################################
		## Test it with prediction model as each "existing benchmark"
		if (cluster_pred == TRUE)
		{
			anova1 <- list();
			for (i in levels(dsaux_other$Benchmark))
			{
				dsaux_aux <- dsaux_sort;
				dsaux_aux$Benchmark <- i;
				paux1 <- aloja_predict_dataset(learned_model=m5p4,vin=varin,ds=dsaux_aux[,varin]);
				paux2 <- aloja_predict_dataset(learned_model=m5p4,vin=varin,ds=dsaux_other[dsaux_other$Benchmark==i,varin]);

				perr1 <- paux1 - dsaux_aux[,varout];
				perr2 <- paux2 - dsaux_other[dsaux_other$Benchmark==i,varout];
			
				perry <- c(perr1,perr2);
				group <- rep(1:2,c(length(perr1),length(perr2)));
				daux = data.frame(y = perry, group = factor(group));
				anova1[[i]] <- anova(lm(perry~group,daux));
			}

			result <- t(sapply(lapply(anova1,function(x) x$"Pr(>F)"[1]),unlist));
			maxclu_aux <- colnames(result)[which.max(result)];
			maxsig_aux <- max(result);

			alpha <- 0.05;
			if (max_clu > alpha)
			{
				# TODO - Join the two benchmarks
			} else {
				# TODO - Create new cluster (and learn about it)
			}
		}
	}

