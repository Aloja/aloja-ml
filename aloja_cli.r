#!/usr/bin/env Rscript

# Josep Ll. Berral-García
# ALOJA-BSC-MRS hadoop.bsc.es
# 2014-11-24
# Launcher of ALOJA-ML
 
# usage: ./aloja_cli.r -d dataset.csv -m aloja_m5p -p param1=aaaa,param2=bbbb,param3=cccc,...
#	 ./aloja_cli.r --dataset dataset.csv --method aloja_regtree --params param1=aaaa,param2=bbbb,param3=cccc,...

source("functions.r");

###############################################################################
# Read arguments from CLI

	suppressPackageStartupMessages(require(optparse));

	option_list = list(
		make_option(c("-p", "--params"), action="store", default=NA, type='character', help="list of parameters, separated by coma and no spaces"),
		make_option(c("-m", "--method"), action="store", default=NA, type='character', help="method to be executed"),
		make_option(c("-d", "--dataset"), action="store", default=NA, type='character', help="dataset source of data")
	) 

	opt = parse_args(OptionParser(option_list=option_list));

###############################################################################
# Error and Warning messages on arguments

	if (is.null(opt$dataset))
	{
		cat("Warning! No dataset introduced. Dataset could be incomplete if no Training, Validation and Test files are explicitly introduced");
	}

	if (is.null(opt$method))
	{
		cat("Error! No method selected. Aborting mission.");
		quit(save="no", status=-1);
	}

	if (is.null(opt$params))
	{
		cat("Info! No parameters introduced. Default configuration per method will be selected.");
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

	varout <- "Exe.Time";
	varin <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");

###############################################################################
# Parse parameters

	params <- list();
	params[["ds"]] <- dataset;
	params[["vin"]] = varin;
	params[["vout"]] = varout;

	if (!is.null(opt$params))
	{
		saux_1 <- strsplit(opt$params, ",");
		saux_2 <- strsplit(saux_1[[1]],"=");

		for (i in 1:length(saux_2))
		{
			params[[saux_2[[i]][1]]] <- saux_2[[i]][2];
		}
	}

###############################################################################
# Execute call

	result <- do.call(opt$method,params);

###############################################################################
# C'est fini

	quit(save="no", status=0);

