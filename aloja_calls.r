#!/usr/bin/env Rscript

# Josep Ll. Berral-García
# ALOJA-BSC-MSR hadoop.bsc.es
# 2014-12-11
# Example of calling prodecures from ALOJA-ML

source("functions.r");
options(width=as.integer(Sys.getenv("COLUMNS")));

###############################################################################
# Procedures for Remote Calls                                                 #
###############################################################################

	# Call for aloja_get_data
	params <- list();
	params[["fread"]] = "aloja-dataset.csv";
	params[["cds"]] = FALSE;
	params[["hds"]] = FALSE;
	params[["fproc"]] = "aloja-process";

	dataset <- do.call(aloja_get_data,params);
	varout <- "Exe.Time";
	varin <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");

	# Call for aloja_print_summaries
	params <- list();
	params[["fprint"]] = "output.txt"
	params[["ds"]] = dataset;
	params[["ds_sub"]] = dataset[,c(varout,varin)];
	params[["fwidth"]] = 1000;
	params[["ms"]] <- 10;
	params[["sname"]] <- "Benchmark";

	do.call(aloja_print_summaries,params);

	# Relation among input ~ output variables
	params <- list();
	params[["ds"]] = dataset[,c(varout,varin)];
	params[["pnglabel"]] = "cross";
	params[["jfactor"]] <- 0.1;

	do.call(aloja_crossvariables,params);

	# ANOVA of current variables
	params <- list();
	params[["ds"]] = dataset;

	anova_1 <- do.call(aloja_anova,params);

	# Training M5P without example selection
	params <- list();
	params[["ds"]] = dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;
	params[["pngval"]] = "m5p-simple-app";
	params[["pngtest"]] = "m5p-simple-test";
	params[["saveall"]] = "m5p-simple";

	m5p1 <- do.call(aloja_regtree,params);

	common_testset <- m5p1$testset;

	# Training M5P with example selection
	params <- list();
	params[["ds"]] = dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;
	params[["pngval"]] = "m5p-exsel-app";
	params[["pngtest"]] = "m5p-exsel-test";
	params[["saveall"]] = "m5p-exsel";
	params[["ttaux"]] = common_testset;
	params[["exsel"]] = 8000;

	m5p2 <- do.call(aloja_regtree,params);

	# Training M5P with benchmark separation
	m5px <- list();
	for (name in levels(dataset[,"Benchmark"]))
	{
		params <- list();
		params[["ds"]] = dataset[dataset[,"Benchmark"]==name,];
		params[["vin"]] = varin;
		params[["vout"]] = varout;
		params[["pngval"]] = paste("m5p-benchmark",name,"val",sep="-");
		params[["pngtest"]] = paste("m5p-benchmark",name,"test",sep="-");
		params[["saveall"]] = paste("m5p-benchmark",name,sep="-");
		params[["ttaux"]] = common_testset[common_testset[,"Benchmark"]==name,];

		m5px[[name]] <- do.call(aloja_regtree,params);
	}
	rm (name);

	# Nearest Neighbors
	params <- list();
	params[["ds"]] = dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;
	params[["pngval"]] = "ibk-simple-app";
	params[["pngtest"]] = "ibk-simple-test";
	params[["saveall"]] = "ibk-simple";
	params[["ttaux"]] = common_testset;
	params[["kparam"]] = 1;
	params[["iparam"]] = TRUE;
	
	ibk1 <- do.call(aloja_nneighbors,params);

	# LinReg (Binarized & Polynomial)
	params <- list();
	params[["ds"]] = dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;
	params[["pngval"]] = "linreg-polynom3-app";
	params[["pngtest"]] = "linreg-polynom3-test";
	params[["saveall"]] = "linreg-polynom3";
	params[["ttaux"]] = common_testset;
	params[["ppoly"]] = 3;

	pr3 <- do.call(aloja_linreg,params);

	# Neural Networks
	params <- list();
	params[["ds"]] = dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;
	params[["pngval"]] = "nnet-32-5-1-app";
	params[["pngtest"]] = "nnet-32-5-1-test";
	params[["saveall"]] = "nnet-32-5-1";
	params[["ttaux"]] = common_testset;
	params[["hlayers"]] = 5;
	params[["decay"]] = 5e-4;
	params[["maxit"]] = 1000;
	params[["prange"]] = NULL;

	nn1 <- do.call(aloja_nnet,params); 

	# Principal Components Analysis
	params <- list();
	params[["ds"]] = dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;
	params[["pngpca"]] = "pca";
	params[["saveall"]] = "pca1";

	pca1 <- do.call(aloja_pca,params);

	# LinReg (with reduced dimension)
	params <- list();
	params[["fread"]] = "pca1-transformed.csv";
	pca1ds <- do.call(aloja_get_data,params);

	params <- list();
	params[["ds"]] = pca1ds;
	params[["vin"]] = (colnames(pca1ds)[!(colnames(pca1ds) %in% c("ID",varout,"End.tme","Running.Cost.."))])[1:20]
	params[["vout"]] = varout;
	params[["pngval"]] = "linreg-polynom3-redim-app";
	params[["pngtest"]] = "linreg-polynom3-redim-test";
	params[["saveall"]] = "linreg-polynom3-redim";
	params[["ppoly"]] = 3;
	params[["prange"]] = c(1e-4,1e+4);

	pr3dim <- do.call(aloja_linreg,params);

	# Training M5P (with reduced dimension)
	params <- list();
	params[["fread"]] = "pca1-transformed.csv";
	pca1ds <- do.call(aloja_get_data,params);

	params <- list();
	params[["ds"]] = pca1ds;
	params[["vin"]] = (colnames(pca1ds)[!(colnames(pca1ds) %in% c("ID",varout,"End.tme","Running.Cost.."))])[1:20]
	params[["vout"]] = varout;
	params[["pngval"]] = "m5p-simple-redim-app";
	params[["pngtest"]] = "m5p-simple-redim-test";
	params[["saveall"]] = "m5p-simple-redim";
	params[["prange"]] = c(1e-4,1e+4);

	m5p1dim <- do.call(aloja_regtree,params);

	# Training, saving, loading and using a model
	params <- list();
	params[["ds"]] = dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;
	params[["pngval"]] = "m5p-iotest-app";
	params[["pngtest"]] = "m5p-iotest-test";
	params[["saveall"]] = "m5p-iotest";

	m5pio1 <- do.call(aloja_regtree,params);

	params <- list();
	params[["tagname"]] = "m5p-iotest";
	params[["is.weka"]] = TRUE;

	m5pio2 <- do.call(aloja_load_object,params);
	if (!params$is.weka) identical(m5pio1, m5pio2, ignore.environment = TRUE);

	params <- list();
	params[["learned_model"]] = m5pio2;
	params[["vin"]] = varin;
	params[["inst_predict"]] = c("sort","ETH","RR3","8","10","1","65536","None","32","Azure L");

	predict1 <- do.call(aloja_predict_instance,params);

	params <- list();
	params[["learned_model"]] = m5pio2;
	params[["vin"]] = varin;
	params[["ds"]] = dataset;	

	predict2 <- do.call(aloja_predict_dataset,params);

	# PCA, convert new data, load and using a model
	params <- list();
	params[["ds"]] = dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;
	params[["saveall"]] = "pca2";

	pca2 <- do.call(aloja_pca,params);

	params <- list();
	params[["ds"]] = dataset;
	params[["pca_name"]] = "pca2"
	params[["saveall"]] = "newdataset";

	newdata <- do.call(aloja_transform_data,params);

	params <- list();
	params[["inst_transform"]] = c("sort","ETH","RR3","8","10","1","65536","None","32","Azure L");
	params[["pca_name"]] = "pca1"

	newinstance <- do.call(aloja_transform_instance,params);
	
	# Collapsing Datasets
	params <- list();
	params[["ds"]] = dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;
	params[["dimension1"]] = "Benchmark";
	params[["dimension2"]] = c("Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");
	params[["dimname1"]] = "Benchmark";
	params[["dimname2"]] = "Configuration";
	params[["saveall"]] = "dsc1"

	dsc1 <- do.call(aloja_dataset_collapse,params);

	# Selecting most informative configurations
	params <- list();
	params[["bvec_name"]] = "dsc1"

	sresult1 <- do.call(aloja_best_configurations,params);

	# Clustering Datasets
	params <- list();
	params[["datamatrix"]] = dsc1$matrix;
	params[["k"]] = 3;
	params[["na.predict"]] = m5pio2;

	kc1 <- do.call(aloja_dataset_clustering,params);


