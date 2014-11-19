
source("functions.r");
source("nnet_plot_update.r");
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

options(width=as.integer(Sys.getenv("COLUMNS")));

###############################################################################
# Read datasets and prepare them for usage                                    #
###############################################################################

	aux <- aloja_get_data(fread = "aloja-dataset.csv", cds = FALSE, hds = FALSE, fproc = "aloja-process");
	dataset <- aux$ds;
	dataset_sub <- aux$ds_sub;
	rm(aux);

	aloja_print_summaries(fprint="output.txt", ds=dataset, ds_sub=dataset_sub, fwidth = 1000, ms = 10);

###############################################################################
# Relation among input ~ output variables                                     #
###############################################################################

	aloja_crossvariables(dataset_sub, jfactor=0.1);

###############################################################################
# ANOVA of current variables                                                  #
###############################################################################

	anova_1 <- aloja_anova(dataset);
	print (c("Means are equal: ",anova_1$f < anova_1$critical));

###############################################################################
# Learning from the variables                                                 #
###############################################################################

	vout <- "Exe.Time";
	vin <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");

###############################################################################
# Decision/Regression Tree

	#######################################################################
	## Training M5P without example selection

	#m5p1 <- aloja_regtree(dataset,vin,vout);
	m5p1 <- aloja_regtree(dataset,vin,vout,saveall="m5p-simple",pngval="m5p-simple-app",pngtest="m5p-simple-test");

	#######################################################################
	## Training M5P with example selection

	#m5p2 <- aloja_regtree(dataset,vin,vout,ttaux=m5p1$testset,exsel=8000);
	m5p2 <- aloja_regtree(dataset,vin,vout,ttaux=m5p1$testset,exsel=8000,saveall="m5p-simple",pngval="m5p-exsel-app",pngtest="m5p-exsel-test");

	#######################################################################
	## Training M5P with benchmark separation

	m5px <- list();
	for (name in levels(dataset[,"Benchmark"]))
	{
		baux <- dataset[dataset[,"Benchmark"]==name,];
		taux <- m5p1$testset[m5p1$testset[,"Benchmark"]==name,];

		#m5px[[name]] <- aloja_regtree(ds=baux,vin=vin,vout=vout,ttaux=taux);
		m5px[[name]] <- aloja_regtree(ds=baux,vin=vin,vout=vout,ttaux=taux,saveall=paste("m5p-benchmark",name,sep="-"),pngval=paste("m5p-benchmark",name,"val",sep="-"),pngtest=paste("m5p-benchmark",name,"test",sep="-"));
	}
	rm (baux,taux,name);

###############################################################################
# k-Nearest Neighbor

	#ibk1 <- aloja_nneighbors(dataset,vin,vout,ttaux=m5p1$testset);
	ibk1 <- aloja_nneighbor(dataset,vin,vout,ttaux=m5p1$testset,saveall="ibk-simple",pngval="ibk-simple-app",pngtest="ibk-simple-test");

###############################################################################
# Others (Regression)

	#######################################################################
	## LinReg (Binarized & Polynomial)

	#pr3 <- aloja_linreg(dataset,vin,vout,ppoly=3);
	pr3 <- aloja_linreg(dataset,vin,vout,ppoly=3,saveall="linreg-polynom3",pngval="linreg-polynom3-app",pngtest="linreg-polynom3-test");

	par(mfrow=c(1,2));
	plot(pr3$predval,pr3$validset[,vout],main=paste("Polynomial Regression power =",pr3$ppoly));
	abline(0,1);
	plot(pr3$predtest,pr3$testset[,vout],main=paste("Test Polynomial Regression power =",pr3$ppoly));
	abline(0,1);
	points(pr3$predtest[rownames(pr3$testset) %in% rownames(pr3$testset[pr3$testset[,"dfsioe_read"]==1,])],pr3$testset[rownames(pr3$testset) %in% rownames(pr3$testset[pr3$testset[,"dfsioe_read"]==1,]),1],col="red");


	#######################################################################
	## Neural Networks

	#nn1 <- aloja_nnet(dataset,vin,vout);
	nn1 <- aloja_nnet(dataset,vin,vout,hlayers=5,saveall="nnet-32-5-1",pngval="nnet-32-5-1-app",pngtest="nnet-32-5-1-test"); 

###############################################################################
# Clustering and dimensional techniques                                       #
###############################################################################

###############################################################################
# Principal Components Analysis

	pca1 <- aloja_pca(pr3$dataset,colnames(pr3$dataset)[2:21],colnames(pr3$dataset)[1],pngpca="pca");
	pca1$loadings;

	#######################################################################
	## LinReg (with reduced dimension)

	#pr3dim <- aloja_linreg(pca1$dataset,colnames(pca1$dataset)[2:21],colnames(pca1$dataset)[1],ppoly=3,prange=c(1e-4,1e+4));
	pr3dim <- aloja_linreg(pca1$dataset,colnames(pca1$dataset)[2:21],colnames(pca1$dataset)[1],ppoly=3,prange=c(1e-4,1e+4),saveall=c("polynom3 redim","linreg"),pngval="linreg-polynom3-redim-app",pngtest="linreg-polynom3-redim-test");

	par(mfrow=c(1,2));
	plot(pr3dim$predval,pr3dim$validset[,vout],main=paste("Polynomial Regression power =",pr3dim$ppoly));
	abline(0,1);
	plot(pr3dim$predtest,pr3dim$testset[,vout],main=paste("Test Polynomial Regression power =",pr3dim$ppoly));
	abline(0,1);
	points(pr3dim$predtest[rownames(pr3dim$testset) %in% rownames(pca1$dataset[pca1$dataset[,"dfsioe_read"]==1,])],pr3dim$testset[rownames(pr3dim$testset) %in% rownames(pca1$dataset[pca1$dataset[,"dfsioe_read"]==1,]),1],col="red");


	#######################################################################
	## Training M5P (with reduced dimension)

	#m5p1dim <- aloja_regtree(pca1$extended,colnames(pca1$extended)[2:21],colnames(pca1$extended)[1],prange=c(1e-4,1e+4));
	m5p1dim <- aloja_regtree(pca1$extended,colnames(pca1$extended)[2:21],colnames(pca1$extended)[1],prange=c(1e-4,1e+4),saveall=c("simple redim","m5p"),pngval="m5p-simple-redim-app",pngtest="m5p-simple-redim-test");

	par(mfrow=c(1,2));
	plot(m5p1dim$predval,m5p1dim$validset[,vout],main=paste("Best Validation M5P (Red.Dim.) M = ",m5p1dim$mmin));
	abline(0,1);
	plot(m5p1dim$predtest,m5p1dim$testset[,vout],main=paste("Test M5P (Red.Dim.) M = ",m5p1dim$mmin));
	abline(0,1);
	points(m5p1dim$predtest[rownames(m5p1dim$testset) %in% rownames(pr3$dataset[pr3$dataset[,"dfsioe_read"]==1,])],m5p1dim$testset[rownames(m5p1dim$testset) %in% rownames(pr3$dataset[pr3$dataset[,"dfsioe_read"]==1,]),1],col="red");


