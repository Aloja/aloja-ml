#!/usr/bin/env Rscript

# Josep Ll. Berral-García
# ALOJA-BSC-MRS hadoop.bsc.es
# 2014-11-24
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

	pca1 <- do.call(aloja_pca,params);

	# LinReg (with reduced dimension)
	params <- list();
	params[["ds"]] = pca1$dataset;
	params[["vin"]] = colnames(pca1$dataset)[-1];
	params[["vout"]] = colnames(pca1$dataset)[1];
	params[["pngval"]] = "linreg-polynom3-redim-app";
	params[["pngtest"]] = "linreg-polynom3-redim-test";
	params[["saveall"]] = "linreg-polynom3-redim";
	params[["ppoly"]] = 3;
	params[["prange"]] = c(1e-4,1e+4);

	pr3dim <- do.call(aloja_linreg,params);

	# Training M5P (with reduced dimension)
	params <- list();
	params[["ds"]] = pca1$dataset;
	params[["vin"]] = colnames(pca1$dataset)[-1];
	params[["vout"]] = colnames(pca1$dataset)[1];
	params[["pngval"]] = "m5p-simple-redim-app";
	params[["pngtest"]] = "m5p-simple-redim-test";
	params[["saveall"]] = "m5p-simple-redim";
	params[["prange"]] = c(1e-4,1e+4);

	m5p1dim <- do.call(aloja_regtree,params);

