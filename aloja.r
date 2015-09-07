#!/usr/bin/env Rscript

# Josep Ll. Berral-García
# ALOJA-BSC-MSR hadoop.bsc.es
# 2014-11-24
# Scripts and snippets for ALOJA-ML

library(devtools);
source_url('https://raw.githubusercontent.com/Aloja/aloja-ml/master/functions.r');
#source("functions.r");
options(width=as.integer(Sys.getenv("COLUMNS")));

###############################################################################
# Read datasets and prepare them for usage                                    #
###############################################################################

	dataset <- aloja_get_data(fread = "aloja-dataset.csv", cds = FALSE, hds = FALSE, fproc = "aloja-process");

	varout <- "Exe.Time";
	#varin <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");
	varin <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster","Cl.Name","Datanodes","Headnodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Type","Bench.Type");

	aloja_print_summaries(fprint="output", ds=dataset, fwidth = 1000, ms = 10, sname = "Benchmark");

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
	m5p1 <- aloja_regtree(dataset,vin=varin,vout=varout,saveall="m5p-1",pngval="m5p-1-app",pngtest="m5p-1-test");

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
# Optimization Techniques                                                     #
###############################################################################

###############################################################################
# Outlier Detection (with paralelization)

	params <- list();
	params[["ds"]] = m5p2$dataset;
	params[["vin"]] = m5p2$varin;
	params[["vout"]] = "Exe.Time";
	params[["learned_model"]] = m5p2;
	params[["sigma"]] = 1;
	params[["hdistance"]] = 0;
	params[["sfCPU"]] = 3;

	system.time(outds5 <- do.call(aloja_outlier_dataset,params));

###############################################################################
# Prediction (with paralelization)

	system.time(a <- aloja_predict_dataset(m5p2,m5p2$varin,ds=m5p2$dataset,sfCPU=3))
	system.time(a <- aloja_predict_instance(m5p2,m5p2$varin,inst_predict=c("prep_terasort","ETH","HDD","8","10","1","65536","Cmp0","64","Cl3","al-03","8","1","linux","8","56","azure","A7","IaaS"),sfCPU=3))

###############################################################################
# Find Attributes against Genetic Search

	vin1 <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");
	vin2 <- c("Cl.Name","Datanodes","Headnodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Type");
	expressionA1 <- c("terasort","*","*","*","*","*","*","*","*","Cl3","al-03","8","1","linux","8","56","azure","A7","IaaS");
	expressionA2 <- c("terasort","*","*","*","*","*","*","*","*","Cl1","m1000-01","3","1","linux","12","128","on-premise","SYS-6027R-72RF","On-premise");
	expressionA3 <- c("terasort","*","*","*","*","*","*","*","*","Cl4","al-04","8","1","linux","8","56","azure","A7","IaaS");

	system.time(a1 <- aloja_predict_instance(m5p2,c(vin1,vin2),inst_predict=expressionA1,sfCPU=1));
	system.time(a2 <- aloja_predict_instance(m5p2,c(vin1,vin2),inst_predict=expressionA2,sfCPU=1));
	system.time(a3 <- aloja_predict_instance(m5p2,c(vin1,vin2),inst_predict=expressionA3,sfCPU=1));
	a <- rbind(a1,a2); a <- rbind(a,a3); a[which(a$Prediction==min(a$Prediction)),];

	expressionB <- c("terasort","*","*","*","*","*","*","*","*","Cl1|Cl3|Cl4");
	system.time(b <- aloja_genalg_search(reference_model=m5p2,vin=vin1,vin_complete=vin2,expression=expressionB))

###############################################################################
# Data Representation                                                         #
###############################################################################

###############################################################################
# Group Data on Find Attributes

	#m5p2 - 2f9e5425835008cebabc6966c65bc626
	vin1 <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");
	vin2 <- c("Cl.Name","Datanodes","Headnodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Type");
	expression <- c("terasort","ETH|IB","HDD|SSD","*","10","1","65536|131072","Cmp0","128","Cl1","m1000-01","3","1","linux","12","128","on-premise","SYS-6027R-72RF","On-premise");
	a <- aloja_predict_instance(m5p2,c(vin1,vin2),inst_predict=expression,sfCPU=3);

	stree <- aloja_representative_tree(a, method = "ordered");
	aloja_repress_tree_string(stree);
	aloja_repress_tree_ascii(stree);

	#########
	b <- sapply(a$Instance,function(x) strsplit(x,","));
	b <- as.data.frame(t(matrix(unlist(b),nrow=length(c(vin1,vin2)))));
	b <- cbind(b,a$Prediction);
	colnames(b) <- c(vin1,vin2,"Prediction");
	bord <- b[order(b$Prediction),];

	hdaux <- sapply(1:nrow(bord),function(x) { sapply(1:nrow(bord),function(y) { sum(bord[x,]!=bord[y,]) }) });
	hc <- hclust(as.dist(hdaux),"ave");
	rhc <- reorder.hclust(x=hc,dis=dist(bord$Prediction))
	plot(rhc,labels=round(bord$Prediction))
	#text(x=rhc$order,y=rhc$height,labels="H")

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
# Example selection for Benchmarks

	m5p4 <- aloja_regtree(ds=dataset[dataset$Benchmark=="terasort",],vin=varin,vout=varout,saveall="m5p4");
	mi1 <- aloja_minimal_instances(learned_model=m5p4,quiet=0,kmax=200,step=10);


###############################################################################
# Example for comparision and statistics per cluster

	#######################################################################
	## Precision

	vin_ord <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster","Cl.Name","Datanodes","Headnodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Type");
	vout_ord <- "Exe.Time";
	aloja_precision(dataset[dataset[,"Cl.Name"]=="rl-06",],vin_ord,vout_ord,noout=1);
	aloja_precision(dataset[dataset[,"Cl.Name"]=="rl-16",],vin_ord,vout_ord,noout=1);
	aloja_precision(dataset[dataset[,"Cl.Name"]=="rl-19",],vin_ord,vout_ord,noout=1);
	aloja_precision(dataset[dataset[,"Cl.Name"]=="rl-30",],vin_ord,vout_ord,noout=1);
	aloja_precision(dataset[dataset[,"Cl.Name"]=="rl-33",],vin_ord,vout_ord,noout=1);

	aloja_precision_split(dataset,vin_ord,vout_ord,"Cl.Name",noout=1);

	#######################################################################
	## Reunion of Executions

	vin_ord2 <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size");
	vout_ord <- "Exe.Time";
	aloja_reunion(dataset,vin_ord2,vout_ord);

	#######################################################################
	## Diversity Analysis

	vdisc <- "Cl.Name";
	aloja_diversity(dataset,vin_ord2,vout_ord,vdisc);
	aloja_diversity_json(dataset,vin_ord2,vout_ord,vdisc);

###############################################################################
# Extraction of Rules

	varin_rr <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster","Cl.Name","Datanodes","Headnodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Type","Bench.Type");

	# Most Frequent Patterns for Single Attributes
	r1 <- aloja_bestrules_single(dataset,varin_rr[2:9],bench="terasort",cluster="al-05",percent="20%",minval=50,filename=NULL);

	# Most Frequent Patterns for Paired Attributes
	r2 <- aloja_bestrules_pairs(dataset,varin_rr[2:9],bench="terasort",cluster="al-05",percent="20%",minval=50,filename=NULL);

	# Most Frequent Patterns for Paired Attributes (Only Numerics)
	r3 <- aloja_bestrules_relations(dataset,varin_rr[2:9],bench="terasort",cluster="al-05",percent="20%",minval=50,filename=NULL);


###############################################################################
# Classification of New Benchmarks

# TODO FIXME - Refactor the following code:

	#######################################################################
	## Rank Configurations

	dsc1 <- aloja_dataset_collapse (dataset,varin,varout,dimension1="Benchmark",dimension2=varin[!(varin %in% "Benchmark")],dimname1="Benchmark",dimname2="Configuration",saveall="dsc1");
	sresult1 <- aloja_best_configurations (dsc1);
	write.table(x=sresult1,file="ranking.data",sep=",",row.names=FALSE);

	dsc11 <- aloja_dataset_collapse (dataset,varin,varout,dimension1="Benchmark",dimension2=varin[!(varin %in% c("Benchmark","Net","Disk","Cluster"))],dimname1="Benchmark",dimname2="Configuration");
	sresult11 <- aloja_best_configurations (dsc11);
	write.table(x=sresult11,file="ranking11.data",sep=",",row.names=FALSE);

	dsc12 <- aloja_dataset_collapse (dataset[dataset[,"Cluster"]=="Local 1",],varin,varout,dimension1="Benchmark",dimension2=varin[!(varin %in% c("Benchmark","Net","Disk","Cluster"))],dimname1="Benchmark",dimname2="Configuration");
	dsc13 <- aloja_dataset_collapse (dataset[dataset[,"Cluster"]=="Azure L",],varin,varout,dimension1="Benchmark",dimension2=varin[!(varin %in% c("Benchmark","Net","Disk","Cluster"))],dimname1="Benchmark",dimname2="Configuration");
	sresult12 <- aloja_best_configurations (dsc12);
	sresult13 <- aloja_best_configurations (dsc13);
	write.table(x=sresult12,file="Ranking of Configurations (SW) Local 1.data",sep=",",row.names=FALSE);
	write.table(x=sresult13,file="Ranking of Configurations (SW) Azure L.data",sep=",",row.names=FALSE);

	#######################################################################
	## Classify new Benchmark

	# Preparing dummy scenario
	dsaux_sort <- dataset[dataset$Benchmark=="sort",];
	dsaux_sort$Benchmark <- dsaux_sort$Benchmark[,drop=TRUE];
	dsaux_other <- dataset[dataset$Benchmark!="sort",];
	dsaux_other$Benchmark <- dsaux_other$Benchmark[,drop=TRUE];

	dsc2 <- aloja_dataset_collapse (dsaux_other,varin,varout,dimension1="Benchmark",dimension2=c(3:11),dimname1="Benchmark",dimname2="Configuration",saveall="dsc2");
	m5p4 <- aloja_regtree(dsaux_other,vin=varin,vout=varout,saveall="m5p-dummy-cluster");
	kc4 <- aloja_dataset_clustering(datamatrix=dsc2$matrix,k=8,na.predict=m5p4);

	nb_name <- "sort";

	# Select best configurations to compare with existing benchmarks
	sresult <- aloja_best_configurations (dsc2);
	sresult[1:10,];

	# DO THE HADOOP EXPERIMENTS NOW

	# Get characterization vector on new benchmark
	dsc21 <- aloja_dataset_collapse (dsaux_sort,varin,varout,dimension1="Benchmark",dimension2=c(3:11),dimname1="Benchmark",dimname2="Configuration",saveall="dsc21");
		
	## Use clustering to check the "existing benchmarks"
	ct1 <- aloja_check_cluster(kcluster=kc4,bmk.vector=dsc21);
	if (is.null(ct1$cluster)) { print("New Cluster"); } else { print(paste("Cluster:",ct1$cluster,"Distance:",ct1$min.distance)); }

	## Test it with prediction model as each "existing benchmark"
	pt1 <- aloja_check_similarity(ds_new=dsaux_sort, ds_gral=dsaux_other, vin=varin, vout=varout, var.base="Benchmark", alpha = 0.05);
	if (is.null(pt1$cluster)) { print("New Cluster"); } else { print(paste("Cluster:",pt1$cluster,"Significance:",pt1$significance)); }

	#######################################################################
	## General method to Classify new Benchmark

	# Preparing dummy scenario
	ds_separate <- list();
	for (i in levels(dataset$Benchmark))
	{
		ds_separate[[i]] <- dataset[dataset$Benchmark==i,];
		ds_separate[[i]]$Benchmark <- ds_separate[[i]]$Benchmark[,drop=TRUE];
	}

	# Method starts here
	benchmark <- list();
	benchmark[["map"]] <- list();
	benchmark[["model"]] <- list();
	benchmark[["vector"]] <- list();
	benchmark[["dataset"]] <- NULL;

	for (nb_name in names(ds_separate))
	{
		if (!(nb_name %in% names(benchmark$map)))
		{
			new_bmk <- ds_separate[[nb_name]];
			if (length(benchmark$map) == 0)
			{
				print("Benchmark:",nb_new,"-> New Cluster");
				benchmark$map[[nb_name]] <- length(benchmark$map) + 1;
				benchmark$model[[benchmark$map[[nb_name]]]] <- aloja_regtree(new_bmk,vin=varin,vout=varout,saveall=paste("m5p-bmk-",new_bmk,sep=""));
				benchmark$vector[[benchmark$map[[nb_name]]]] <- aloja_dataset_collapse (new_bmk,vin=varin,vout=varout,dimension1="Benchmark",dimension2=c(3:11),dimname1="Benchmark",dimname2="Configuration",saveall=paste("vector-",new_bmk,sep=""));
				benchmark$dataset <- new_bmk;
			} else {
				pt_aux <- aloja_check_similarity(new_bmk,ds_gral=benchmark$dataset,varin,varout,"Benchmark",lmodel=benchmark$model,alpha=0.05);
				if (is.null(pt_aux$cluster))
				{
					print("Benchmark:",nb_new,"-> New Cluster");
					benchmark$map[[nb_name]] <- length(benchmark$map) + 1;
					benchmark$model[[benchmark$map[[nb_name]]]] <- aloja_regtree(new_bmk,vin=varin,vout=varout,saveall=paste("m5p-bmk-",new_bmk,sep=""));
					benchmark$vector[[benchmark$map[[nb_name]]]] <- aloja_dataset_collapse (new_bmk,vin=varin,vout=varout,dimension1="Benchmark",dimension2=c(3:11),dimname1="Benchmark",dimname2="Configuration",saveall=paste("vector-",new_bmk,sep=""));

				} else {
					print(paste("Benchmark:",nb_new,"-> Cluster:",pt_aux$cluster,"Significance:",pt_aux$significance));
					benchmark$map[[nb_name]] <- benchmark$map[[pt_aux$cluster]];
					new_bmk[,"Benchmark"] <- pt_aux$cluster;
					#benchmark$model[[benchmark$map[[nb_name]]]]; # TODO - Retrain model with new data
					#benchmark$vector[[benchmark$map[[nb_name]]]]; # TODO - Find new centroid
				}
				benchmark$dataset <- aloja_dbind(benchmark$dataset,new_bmk);
			}

		} else {
			# TODO - Stuff when the bmk exists
		}
	}
