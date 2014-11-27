#!/usr/bin/env Rscript

# Josep Ll. Berral-García
# ALOJA-BSC-MRS hadoop.bsc.es
# 2014-11-24
# Launcher of ALOJA-ML
 
# usage: ./aloja_cli.r -d dataset.csv -m aloja_regtree -p param1=aaaa;param2=bbbb;param3=cccc,...
#	 ./aloja_cli.r --dataset dataset.csv --method aloja_regtree --params param1=aaaa;param2=bbbb;param3=cccc,...
#	 ./aloja_cli.r -m aloja_predict_instance -l m5p-model -p inst_predict="sort,ETH,RR3,8,10,1,65536,None,32,Azure L" -v
#	 ./aloja_cli.r -m aloja_predict_instance -l m5p-model -d dataset-to-predict.csv -v

source("functions.r");

###############################################################################
# Read arguments from CLI

	suppressPackageStartupMessages(require(optparse));

	option_list = list(
		make_option(c("-m", "--method"), action="store", default=NULL, type='character', help="Method to be executed"),
		make_option(c("-p", "--params"), action="store", default=NULL, type='character', help="Generic list of parameters, separated by semicolon and no spaces"),
		make_option(c("-v", "--verbose"), action="store_true", default=FALSE, help="Outputs the result of the method"),
		make_option(c("-d", "--dataset"), action="store", default=NULL, type='character', help="For training methods: Dataset source of data"),
		make_option(c("-a", "--allvars"), action="store_true", default=FALSE, help="For training methods: All vars are input but first one (for reduced dimensions)"),
		make_option(c("-l", "--learned"), action="store", default=NULL, type='character', help="For prediction methods: Learned model for prediction")
	);

	opt = parse_args(OptionParser(option_list=option_list));

###############################################################################
# Error and Warning messages on arguments

	if (is.null(opt$method))
	{
		cat("[ERROR] No method selected. Aborting mission.\n");
		quit(save="no", status=-1);
	}

###############################################################################
# Read datasets

	dataset <- NULL;

	if (!is.null(opt$dataset))
	{
		# Call for aloja_get_data
		params_1 <- list();
		params_1[["fread"]] = opt$dataset;

		dataset <- do.call(aloja_get_data,params_1);
	}

###############################################################################
# Parse parameters

	params <- list();
	params[["ds"]] <- dataset;

	if (opt$method %in% c("aloja_regtree","aloja_nneighbors","aloja_linreg","aloja_nnet","aloja_pca","aloja_dataset_collapse","aloja_predict_instance","aloja_predict_dataset"))
	{
		if (opt$allvars)
		{
			params[["vin"]] = colnames(dataset)[-1];
			params[["vout"]] = colnames(dataset)[1];
		} else {
			params[["vin"]] = c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");
			params[["vout"]] = "Exe.Time";
		}
	}

	if (opt$method  == "aloja_predict_instance" || opt$method  == "aloja_predict_dataset")
	{
		params[["vout"]] <- NULL;

		params_2 <- list();
		params_2[["tagname"]] <- opt$learned;
		params[["learned_model"]] <- do.call(aloja_load_object,params_2);
	}

	if (!is.null(opt$params))
	{
		saux_1 <- strsplit(opt$params, ";");
		saux_2 <- strsplit(saux_1[[1]],"=");

		for (i in 1:length(saux_2))
		{
			params[[saux_2[[i]][1]]] <- strsplit(saux_2[[i]][2],",")[[1]];
		}
		rm(saux_1,saux_2);
	}

###############################################################################
# Execute call

	result <- do.call(opt$method,params);

	if (opt$verbose) result;

###############################################################################
# C'est fini

	quit(save="no", status=0);

