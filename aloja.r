#!/usr/bin/env Rscript

# Josep Ll. Berral-García
# ALOJA-BSC-MRS hadoop.bsc.es
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
	kc1 <- aloja_dataset_clustering(datamatrix="dsc1-matrix.csv",k=3);

	#######################################################################
	## Clustering with NA <- prediction

	m5p3 <- aloja_regtree(dataset,vin=varin,vout=varout); model_aux <- m5p3;
	ibk2 <- aloja_nneighbors(dataset,vin=varin,vout=varout); model_aux <- ibk2;
	pr31 <- aloja_linreg(dataset,vin=varin,vout=varout,ppoly=3); model_aux <- pr31;
	nn2 <- aloja_nnet(dataset,vin=varin,vout=varout); model_aux <- nn2;

	kc2 <- aloja_dataset_clustering(datamatrix=dsc1$matrix,k=3,na.predict=model_aux);


