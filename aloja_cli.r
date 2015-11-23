#!/usr/bin/env Rscript

# Josep Ll. Berral-García
# ALOJA-BSC-MSR aloja.bsc.es
# 2014-12-11
# Launcher of ALOJA-ML
 
# usage: ./aloja_cli.r -m method [-d dataset] [-l learned model] [-p param1=aaaa:param2=bbbb:param3=cccc:...] [-v]
#	 ./aloja_cli.r --method method [--dataset dataset] [--learned learned model] [--params param1=aaaa:param2=bbbb:param3=cccc:...] [--verbose]
#
#	 ./aloja_cli.r -m aloja_regtree -d aloja-dataset.csv -p saveall=m5p1
#	 ./aloja_cli.r -m aloja_regtree -d aloja-dataset.csv -p saveall=m5p1-small:vin="Benchmark,Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size"
#	 ./aloja_cli.r -m aloja_predict_dataset -l m5p1 -d m5p1-tt.csv -v
#	 ./aloja_cli.r -m aloja_predict_instance -l m5p1 -p inst_predict="sort,ETH,RR3,8,10,1,65536,None,32,Azure L" -v
#	 ./aloja_cli.r -m aloja_predict_instance -l m5p1 -p inst_predict="sort,ETH,RR3,8|10,10,1,65536,*,32,Azure L":sorted=asc -v
#	 ./aloja_cli.r -m aloja_predict_instance -l m5p1 -p inst_predict="sort,ETH,RR3,8|10,10,1,65536,*,32,Azure L":vin="Benchmark,Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size,Cluster":sorted=asc:saveall="m5p1-instances" -v
#
#	 ./aloja_cli.r -m aloja_outlier_dataset -d m5p1-tt.csv -l m5p1 -p sigma=3:hdistance=3:saveall=m5p1test
#	 ./aloja_cli.r -m aloja_outlier_instance -l m5p1 -p instance="sort,ETH,RR3,8,10,1,65536,None,32,Azure L":observed=100000:display=1 -v
#
#	 ./aloja_cli.r -m aloja_pca -d aloja-dataset.csv -p saveall=pca1
#	 ./aloja_cli.r -m aloja_regtree -d pca1-transformed.csv -p prange=1e-4,1e+4:saveall=m5p-simple-redim -n 20
#	 ./aloja_cli.r -m aloja_predict_instance -l m5p-simple-redim -p inst_predict="1922.904354752,70.1570440421649,2.9694955079494,-3.64259027685954,-0.748746678239734,0.161321484374316,0.617610510007444,-0.459044093400257,0.251211132013151,0.251937462205716,-0.142007748147355,-0.0324862729758309,0.406308900544488,0.13593705166432,0.397452596451088,-0.731635384355167,-0.318297127484775,-0.0876192175148721,-0.0504762335523307,-0.0146283091875174" -v
#	 ./aloja_cli.r -m aloja_predict_dataset -l m5p-simple-redim -d m5p-simple-redim-tt.csv -v
#	 ./aloja_cli.r -m aloja_transform_data -d newdataset.csv -p pca_name=pca1:saveall=newdataset
#	 ./aloja_cli.r -m aloja_transform_instance -p pca_name=pca1:inst_transform="sort,ETH,RR3,8,10,1,65536,None,32,Azure L" -v
#
#	 ./aloja_cli.r -m aloja_dataset_collapse -d aloja-dataset.csv -p dimension1="Benchmark":dimension2="Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size,Cluster":dimname1="Benchmark":dimname2="Configuration":saveall=dsc1
#	 ./aloja_cli.r -m aloja_dataset_collapse -d aloja-dataset.csv -p dimension1="Benchmark":dimension2="Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size,Cluster":dimname1="Benchmark":dimname2="Configuration":saveall=dsc1:model_name=m5p1
#	 ./aloja_cli.r -m aloja_dataset_collapse_expand -d aloja-dataset.csv -p dimension1="Benchmark":dimension2="Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size,Cluster":dimname1="Benchmark":dimname2="Configuration":saveall=dsc1:model_name=m5p1:inst_general="sort,ETH,RR3,8|10,10,1,65536,*,32,Azure L"
#	 ./aloja_cli.r -m aloja_best_configurations -p bvec_name=dsc1 -v
#
#	 ./aloja_cli.r -m aloja_minimal_instances -l m5p1 -p saveall=mi1
#	 ./aloja_cli.r -m aloja_minimal_instances -l m5p1 -p kmax=200:step=10:saveall=mi1
#
#	 ./aloja_cli.r -m aloja_representative_tree -p method=ordered:pred_file="m5p1-instances":output="string" -v
#
#	 ./aloja_cli.r -m aloja_precision -d aloja-dataset.csv -v
#	 ./aloja_cli.r -m aloja_precision_split -d aloja-dataset.csv -p vdisc="Cl.Name":noout=1:sigma=1:json=0 -v
#	 ./aloja_cli.r -m aloja_reunion -d aloja-dataset.csv -v
#	 ./aloja_cli.r -m aloja_diversity -d aloja-dataset.csv -p json=0 -v
#
#	 ./aloja_cli.r -m aloja_bestrules_single -d aloja-dataset.csv -p percent="20%":filename="test":quiet=0 -v
#	 ./aloja_cli.r -m aloja_bestrules_pairs -d aloja-dataset.csv -p percent="20%":filename="test":quiet=0 -v
#	 ./aloja_cli.r -m aloja_bestrules_relations -d aloja-dataset.csv -p percent="20%":filename="test":quiet=0 -v
#	 ./aloja_cli.r -m aloja_bestrules_single_select -d aloja-dataset.csv -p bench="terasort":cluster="al-05":percent="20%":minval=50:filename="test":quiet=0 -v
#	 ./aloja_cli.r -m aloja_bestrules_pairs_select -d aloja-dataset.csv -p bench="terasort":cluster="al-05":percent="20%":minval=50:filename="test":quiet=0 -v
#	 ./aloja_cli.r -m aloja_bestrules_relations_select -d aloja-dataset.csv -p bench="terasort":cluster="al-05":percent="20%":minval=50:filename="test":quiet=0 -v

source("functions.r");

###############################################################################
# Read arguments from CLI

	suppressPackageStartupMessages(require(optparse));

	option_list = list(
		make_option(c("-m", "--method"), action="store", default=NULL, type='character', help="Method to be executed"),
		make_option(c("-p", "--params"), action="store", default=NULL, type='character', help="Generic list of parameters, separated by two points and no spaces"),
		make_option(c("-v", "--verbose"), action="store_true", default=FALSE, help="Outputs the result of the method"),
		make_option(c("-d", "--dataset"), action="store", default=NULL, type='character', help="For training methods: Dataset source of data"),
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

	if (opt$method %in% c("aloja_regtree","aloja_nneighbors","aloja_linreg","aloja_nnet","aloja_supportvms","aloja_pca","aloja_dataset_collapse","aloja_dataset_collapse_expand","aloja_binarize_instance"))
	{
		# Excluded "Benchmark", "Net", "Disk", "ID.Cluster", "Cl.Name"
		if (is.null(opt$vin)) params[["vin"]] <- c("Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Datanodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Service.Type","Bench.Type","Hadoop.Version","Datasize","Scale.Factor","Net.maxtxKB.s","Net.maxrxKB.s","Net.maxtxPck.s","Net.maxrxPck.s","Net.maxtxCmp.s","Net.maxrxCmp.s","Net.maxrxmsct.s","Disk.maxtps","Disk.maxsvctm","Disk.maxrd.s","Disk.maxwr.s","Disk.maxrqsz","Disk.maxqusz","Disk.maxawait","Disk.maxutil","BMK.CPU.avguser","BMK.CPU.maxuser","BMK.CPU.minuser","BMK.CPU.sdpopuser","BMK.CPU.varpopuser","BMK.CPU.avgnice","BMK.CPU.maxnice","BMK.CPU.minnice","BMK.CPU.sdpopnice","BMK.CPU.varpopnice","BMK.CPU.avgsystem","BMK.CPU.maxsystem","BMK.CPU.minsystem","BMK.CPU.sdpopsystem","BMK.CPU.varpopsystem","BMK.CPU.avgiowait","BMK.CPU.maxiowait","BMK.CPU.miniowait","BMK.CPU.sdpopiowait","BMK.CPU.varpopiowait","BMK.CPU.avgsteal","BMK.CPU.maxsteal","BMK.CPU.minsteal","BMK.CPU.sdpopsteal","BMK.CPU.varpopsteal","BMK.CPU.avgidle","BMK.CPU.maxidle","BMK.CPU.minidle","BMK.CPU.sdpopidle","BMK.CPU.varpopidle","BMK.MEM.avgKBmemfree","BMK.MEM.maxKBmemfree","BMK.MEM.minKBmemfree","BMK.MEM.sdpopKBmemfree","BMK.MEM.varpopKBmemfree","BMK.MEM.avgKBmemused","BMK.MEM.maxKBmemused","BMK.MEM.minKBmemused","BMK.MEM.sdpopKBmemused","BMK.MEM.varpopKBmemused","BMK.MEM.avgmemused","BMK.MEM.maxmemused","BMK.MEM.minmemused","BMK.MEM.sdpopmemused","BMK.MEM.varpopmemused","BMK.MEM.avgKBbuffers","BMK.MEM.maxKBbuffers","BMK.MEM.minKBbuffers","BMK.MEM.sdpopKBbuffers","BMK.MEM.varpopKBbuffers","BMK.MEM.avgKBcached","BMK.MEM.maxKBcached","BMK.MEM.minKBcached","BMK.MEM.sdpopKBcached","BMK.MEM.varpopKBcached","BMK.MEM.avgKBcommit","BMK.MEM.maxKBcommit","BMK.MEM.minKBcommit","BMK.MEM.sdpopKBcommit","BMK.MEM.varpopKBcommit","BMK.MEM.avgcommit","BMK.MEM.maxcommit","BMK.MEM.mincommit","BMK.MEM.sdpopcommit","BMK.MEM.varpopcommit","BMK.MEM.avgKBactive","BMK.MEM.maxKBactive","BMK.MEM.minKBactive","BMK.MEM.sdpopKBactive","BMK.MEM.varpopKBactive","BMK.MEM.avgKBinact","BMK.MEM.maxKBinact","BMK.MEM.minKBinact","BMK.MEM.sdpopKBinact","BMK.MEM.varpopKBinact","BMK.NET.avgRXpcks","BMK.NET.maxRXpcks","BMK.NET.minRXpcks","BMK.NET.sdpopRXpcks","BMK.NET.varpopRXpcks","BMK.NET.sumRXpcks","BMK.NET.avgTXpcks","BMK.NET.maxTXpcks","BMK.NET.minTXpcks","BMK.NET.sdpopTXpcks","BMK.NET.varpopTXpcks","BMK.NET.sumTXpcks","BMK.NET.avgRXKBs","BMK.NET.maxRXKBs","BMK.NET.minRXKBs","BMK.NET.sdpopRXKBs","BMK.NET.varpopRXKBs","BMK.NET.sumRXKBs","BMK.NET.avgTXKBs","BMK.NET.maxTXKBs","BMK.NET.minTXKBs","BMK.NET.sdpopTXKBs","BMK.NET.varpopTXKBs","BMK.NET.sumTXKBs","BMK.NET.avgRXcmps","BMK.NET.maxRXcmps","BMK.NET.minRXcmps","BMK.NET.sdpopRXcmps","BMK.NET.varpopRXcmps","BMK.NET.sumRXcmps","BMK.NET.avgTXcmps","BMK.NET.maxTXcmps","BMK.NET.minTXcmps","BMK.NET.sdpopTXcmps","BMK.NET.varpopTXcmps","BMK.NET.sumTXcmps","BMK.NET.avgRXcsts","BMK.NET.maxRXcsts","BMK.NET.minRXcsts","BMK.NET.sdpopRXcsts","BMK.NET.varpopRXcsts","BMK.NET.sumRXcsts","BMK.DSK.avgtps","BMK.DSK.maxtps","BMK.DSK.mintps","BMK.DSK.avgRDs","BMK.DSK.maxRDs","BMK.DSK.minRDs","BMK.DSK.sdpopRDs","BMK.DSK.varpopRDs","BMK.DSK.sumRDs","BMK.DSK.avgWRs","BMK.DSK.maxWRs","BMK.DSK.minWRs","BMK.DSK.sdpopWRs","BMK.DSK.varpopWRs","BMK.DSK.sumWRs","BMK.DSK.avgReqs","BMK.DSK.maxReqs","BMK.DSK.minReqs","BMK.DSK.sdpopReqs","BMK.DSK.varpopReqs","BMK.DSK.avgQus","BMK.DSK.maxQus","BMK.DSK.minQus","BMK.DSK.sdpopQus","BMK.DSK.varpopQus","BMK.DSK.avgwait","BMK.DSK.maxwait","BMK.DSK.minwait","BMK.DSK.sdpopwait","BMK.DSK.varpopwait","BMK.DSK.avgutil","BMK.DSK.maxutil","BMK.DSK.minutil","BMK.DSK.sdpoputil","BMK.DSK.varpoputil","BMK.DSK.avgsvctm","BMK.DSK.maxsvctm","BMK.DSK.minsvctm","BMK.DSK.sdpopsvctm","BMK.DSK.varpopsvctm");
		if (is.null(opt$vout)) params[["vout"]] <- "Exe.Time";
	}

	if (is.null(params$vin) && opt$method %in% c("aloja_outlier_dataset","aloja_outlier_instance"))
	{
		# Excluded "Benchmark", "Net", "Disk", "ID.Cluster", "Cl.Name"
		params[["vin"]] <- c("Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Datanodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Service.Type","Bench.Type","Hadoop.Version","Datasize","Scale.Factor","Net.maxtxKB.s","Net.maxrxKB.s","Net.maxtxPck.s","Net.maxrxPck.s","Net.maxtxCmp.s","Net.maxrxCmp.s","Net.maxrxmsct.s","Disk.maxtps","Disk.maxsvctm","Disk.maxrd.s","Disk.maxwr.s","Disk.maxrqsz","Disk.maxqusz","Disk.maxawait","Disk.maxutil","BMK.CPU.avguser","BMK.CPU.maxuser","BMK.CPU.minuser","BMK.CPU.sdpopuser","BMK.CPU.varpopuser","BMK.CPU.avgnice","BMK.CPU.maxnice","BMK.CPU.minnice","BMK.CPU.sdpopnice","BMK.CPU.varpopnice","BMK.CPU.avgsystem","BMK.CPU.maxsystem","BMK.CPU.minsystem","BMK.CPU.sdpopsystem","BMK.CPU.varpopsystem","BMK.CPU.avgiowait","BMK.CPU.maxiowait","BMK.CPU.miniowait","BMK.CPU.sdpopiowait","BMK.CPU.varpopiowait","BMK.CPU.avgsteal","BMK.CPU.maxsteal","BMK.CPU.minsteal","BMK.CPU.sdpopsteal","BMK.CPU.varpopsteal","BMK.CPU.avgidle","BMK.CPU.maxidle","BMK.CPU.minidle","BMK.CPU.sdpopidle","BMK.CPU.varpopidle","BMK.MEM.avgKBmemfree","BMK.MEM.maxKBmemfree","BMK.MEM.minKBmemfree","BMK.MEM.sdpopKBmemfree","BMK.MEM.varpopKBmemfree","BMK.MEM.avgKBmemused","BMK.MEM.maxKBmemused","BMK.MEM.minKBmemused","BMK.MEM.sdpopKBmemused","BMK.MEM.varpopKBmemused","BMK.MEM.avgmemused","BMK.MEM.maxmemused","BMK.MEM.minmemused","BMK.MEM.sdpopmemused","BMK.MEM.varpopmemused","BMK.MEM.avgKBbuffers","BMK.MEM.maxKBbuffers","BMK.MEM.minKBbuffers","BMK.MEM.sdpopKBbuffers","BMK.MEM.varpopKBbuffers","BMK.MEM.avgKBcached","BMK.MEM.maxKBcached","BMK.MEM.minKBcached","BMK.MEM.sdpopKBcached","BMK.MEM.varpopKBcached","BMK.MEM.avgKBcommit","BMK.MEM.maxKBcommit","BMK.MEM.minKBcommit","BMK.MEM.sdpopKBcommit","BMK.MEM.varpopKBcommit","BMK.MEM.avgcommit","BMK.MEM.maxcommit","BMK.MEM.mincommit","BMK.MEM.sdpopcommit","BMK.MEM.varpopcommit","BMK.MEM.avgKBactive","BMK.MEM.maxKBactive","BMK.MEM.minKBactive","BMK.MEM.sdpopKBactive","BMK.MEM.varpopKBactive","BMK.MEM.avgKBinact","BMK.MEM.maxKBinact","BMK.MEM.minKBinact","BMK.MEM.sdpopKBinact","BMK.MEM.varpopKBinact","BMK.NET.avgRXpcks","BMK.NET.maxRXpcks","BMK.NET.minRXpcks","BMK.NET.sdpopRXpcks","BMK.NET.varpopRXpcks","BMK.NET.sumRXpcks","BMK.NET.avgTXpcks","BMK.NET.maxTXpcks","BMK.NET.minTXpcks","BMK.NET.sdpopTXpcks","BMK.NET.varpopTXpcks","BMK.NET.sumTXpcks","BMK.NET.avgRXKBs","BMK.NET.maxRXKBs","BMK.NET.minRXKBs","BMK.NET.sdpopRXKBs","BMK.NET.varpopRXKBs","BMK.NET.sumRXKBs","BMK.NET.avgTXKBs","BMK.NET.maxTXKBs","BMK.NET.minTXKBs","BMK.NET.sdpopTXKBs","BMK.NET.varpopTXKBs","BMK.NET.sumTXKBs","BMK.NET.avgRXcmps","BMK.NET.maxRXcmps","BMK.NET.minRXcmps","BMK.NET.sdpopRXcmps","BMK.NET.varpopRXcmps","BMK.NET.sumRXcmps","BMK.NET.avgTXcmps","BMK.NET.maxTXcmps","BMK.NET.minTXcmps","BMK.NET.sdpopTXcmps","BMK.NET.varpopTXcmps","BMK.NET.sumTXcmps","BMK.NET.avgRXcsts","BMK.NET.maxRXcsts","BMK.NET.minRXcsts","BMK.NET.sdpopRXcsts","BMK.NET.varpopRXcsts","BMK.NET.sumRXcsts","BMK.DSK.avgtps","BMK.DSK.maxtps","BMK.DSK.mintps","BMK.DSK.avgRDs","BMK.DSK.maxRDs","BMK.DSK.minRDs","BMK.DSK.sdpopRDs","BMK.DSK.varpopRDs","BMK.DSK.sumRDs","BMK.DSK.avgWRs","BMK.DSK.maxWRs","BMK.DSK.minWRs","BMK.DSK.sdpopWRs","BMK.DSK.varpopWRs","BMK.DSK.sumWRs","BMK.DSK.avgReqs","BMK.DSK.maxReqs","BMK.DSK.minReqs","BMK.DSK.sdpopReqs","BMK.DSK.varpopReqs","BMK.DSK.avgQus","BMK.DSK.maxQus","BMK.DSK.minQus","BMK.DSK.sdpopQus","BMK.DSK.varpopQus","BMK.DSK.avgwait","BMK.DSK.maxwait","BMK.DSK.minwait","BMK.DSK.sdpopwait","BMK.DSK.varpopwait","BMK.DSK.avgutil","BMK.DSK.maxutil","BMK.DSK.minutil","BMK.DSK.sdpoputil","BMK.DSK.varpoputil","BMK.DSK.avgsvctm","BMK.DSK.maxsvctm","BMK.DSK.minsvctm","BMK.DSK.sdpopsvctm","BMK.DSK.varpopsvctm");
	}

	if (is.null(params$vin) && opt$method  == "aloja_predict_instance")
	{
		# Excluded "Benchmark", "Net", "Disk", "ID.Cluster", "Cl.Name"
		params[["vin"]] <- c("Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Datanodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Service.Type","Bench.Type","Hadoop.Version","Datasize","Scale.Factor","Net.maxtxKB.s","Net.maxrxKB.s","Net.maxtxPck.s","Net.maxrxPck.s","Net.maxtxCmp.s","Net.maxrxCmp.s","Net.maxrxmsct.s","Disk.maxtps","Disk.maxsvctm","Disk.maxrd.s","Disk.maxwr.s","Disk.maxrqsz","Disk.maxqusz","Disk.maxawait","Disk.maxutil","BMK.CPU.avguser","BMK.CPU.maxuser","BMK.CPU.minuser","BMK.CPU.sdpopuser","BMK.CPU.varpopuser","BMK.CPU.avgnice","BMK.CPU.maxnice","BMK.CPU.minnice","BMK.CPU.sdpopnice","BMK.CPU.varpopnice","BMK.CPU.avgsystem","BMK.CPU.maxsystem","BMK.CPU.minsystem","BMK.CPU.sdpopsystem","BMK.CPU.varpopsystem","BMK.CPU.avgiowait","BMK.CPU.maxiowait","BMK.CPU.miniowait","BMK.CPU.sdpopiowait","BMK.CPU.varpopiowait","BMK.CPU.avgsteal","BMK.CPU.maxsteal","BMK.CPU.minsteal","BMK.CPU.sdpopsteal","BMK.CPU.varpopsteal","BMK.CPU.avgidle","BMK.CPU.maxidle","BMK.CPU.minidle","BMK.CPU.sdpopidle","BMK.CPU.varpopidle","BMK.MEM.avgKBmemfree","BMK.MEM.maxKBmemfree","BMK.MEM.minKBmemfree","BMK.MEM.sdpopKBmemfree","BMK.MEM.varpopKBmemfree","BMK.MEM.avgKBmemused","BMK.MEM.maxKBmemused","BMK.MEM.minKBmemused","BMK.MEM.sdpopKBmemused","BMK.MEM.varpopKBmemused","BMK.MEM.avgmemused","BMK.MEM.maxmemused","BMK.MEM.minmemused","BMK.MEM.sdpopmemused","BMK.MEM.varpopmemused","BMK.MEM.avgKBbuffers","BMK.MEM.maxKBbuffers","BMK.MEM.minKBbuffers","BMK.MEM.sdpopKBbuffers","BMK.MEM.varpopKBbuffers","BMK.MEM.avgKBcached","BMK.MEM.maxKBcached","BMK.MEM.minKBcached","BMK.MEM.sdpopKBcached","BMK.MEM.varpopKBcached","BMK.MEM.avgKBcommit","BMK.MEM.maxKBcommit","BMK.MEM.minKBcommit","BMK.MEM.sdpopKBcommit","BMK.MEM.varpopKBcommit","BMK.MEM.avgcommit","BMK.MEM.maxcommit","BMK.MEM.mincommit","BMK.MEM.sdpopcommit","BMK.MEM.varpopcommit","BMK.MEM.avgKBactive","BMK.MEM.maxKBactive","BMK.MEM.minKBactive","BMK.MEM.sdpopKBactive","BMK.MEM.varpopKBactive","BMK.MEM.avgKBinact","BMK.MEM.maxKBinact","BMK.MEM.minKBinact","BMK.MEM.sdpopKBinact","BMK.MEM.varpopKBinact","BMK.NET.avgRXpcks","BMK.NET.maxRXpcks","BMK.NET.minRXpcks","BMK.NET.sdpopRXpcks","BMK.NET.varpopRXpcks","BMK.NET.sumRXpcks","BMK.NET.avgTXpcks","BMK.NET.maxTXpcks","BMK.NET.minTXpcks","BMK.NET.sdpopTXpcks","BMK.NET.varpopTXpcks","BMK.NET.sumTXpcks","BMK.NET.avgRXKBs","BMK.NET.maxRXKBs","BMK.NET.minRXKBs","BMK.NET.sdpopRXKBs","BMK.NET.varpopRXKBs","BMK.NET.sumRXKBs","BMK.NET.avgTXKBs","BMK.NET.maxTXKBs","BMK.NET.minTXKBs","BMK.NET.sdpopTXKBs","BMK.NET.varpopTXKBs","BMK.NET.sumTXKBs","BMK.NET.avgRXcmps","BMK.NET.maxRXcmps","BMK.NET.minRXcmps","BMK.NET.sdpopRXcmps","BMK.NET.varpopRXcmps","BMK.NET.sumRXcmps","BMK.NET.avgTXcmps","BMK.NET.maxTXcmps","BMK.NET.minTXcmps","BMK.NET.sdpopTXcmps","BMK.NET.varpopTXcmps","BMK.NET.sumTXcmps","BMK.NET.avgRXcsts","BMK.NET.maxRXcsts","BMK.NET.minRXcsts","BMK.NET.sdpopRXcsts","BMK.NET.varpopRXcsts","BMK.NET.sumRXcsts","BMK.DSK.avgtps","BMK.DSK.maxtps","BMK.DSK.mintps","BMK.DSK.avgRDs","BMK.DSK.maxRDs","BMK.DSK.minRDs","BMK.DSK.sdpopRDs","BMK.DSK.varpopRDs","BMK.DSK.sumRDs","BMK.DSK.avgWRs","BMK.DSK.maxWRs","BMK.DSK.minWRs","BMK.DSK.sdpopWRs","BMK.DSK.varpopWRs","BMK.DSK.sumWRs","BMK.DSK.avgReqs","BMK.DSK.maxReqs","BMK.DSK.minReqs","BMK.DSK.sdpopReqs","BMK.DSK.varpopReqs","BMK.DSK.avgQus","BMK.DSK.maxQus","BMK.DSK.minQus","BMK.DSK.sdpopQus","BMK.DSK.varpopQus","BMK.DSK.avgwait","BMK.DSK.maxwait","BMK.DSK.minwait","BMK.DSK.sdpopwait","BMK.DSK.varpopwait","BMK.DSK.avgutil","BMK.DSK.maxutil","BMK.DSK.minutil","BMK.DSK.sdpoputil","BMK.DSK.varpoputil","BMK.DSK.avgsvctm","BMK.DSK.maxsvctm","BMK.DSK.minsvctm","BMK.DSK.sdpopsvctm","BMK.DSK.varpopsvctm");
	}

	if (is.null(params$vin) && opt$method  == "aloja_predict_instance_slice")
	{
		params[["vin"]] <- c("Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Datanodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Service.Type","Bench.Type","Hadoop.Version","Datasize","Scale.Factor","Net.maxtxKB.s","Net.maxrxKB.s","Net.maxtxPck.s","Net.maxrxPck.s","Net.maxtxCmp.s","Net.maxrxCmp.s","Net.maxrxmsct.s","Disk.maxtps","Disk.maxsvctm","Disk.maxrd.s","Disk.maxwr.s","Disk.maxrqsz","Disk.maxqusz","Disk.maxawait","Disk.maxutil","BMK.CPU.avguser","BMK.CPU.maxuser","BMK.CPU.minuser","BMK.CPU.sdpopuser","BMK.CPU.varpopuser","BMK.CPU.avgnice","BMK.CPU.maxnice","BMK.CPU.minnice","BMK.CPU.sdpopnice","BMK.CPU.varpopnice","BMK.CPU.avgsystem","BMK.CPU.maxsystem","BMK.CPU.minsystem","BMK.CPU.sdpopsystem","BMK.CPU.varpopsystem","BMK.CPU.avgiowait","BMK.CPU.maxiowait","BMK.CPU.miniowait","BMK.CPU.sdpopiowait","BMK.CPU.varpopiowait","BMK.CPU.avgsteal","BMK.CPU.maxsteal","BMK.CPU.minsteal","BMK.CPU.sdpopsteal","BMK.CPU.varpopsteal","BMK.CPU.avgidle","BMK.CPU.maxidle","BMK.CPU.minidle","BMK.CPU.sdpopidle","BMK.CPU.varpopidle","BMK.MEM.avgKBmemfree","BMK.MEM.maxKBmemfree","BMK.MEM.minKBmemfree","BMK.MEM.sdpopKBmemfree","BMK.MEM.varpopKBmemfree","BMK.MEM.avgKBmemused","BMK.MEM.maxKBmemused","BMK.MEM.minKBmemused","BMK.MEM.sdpopKBmemused","BMK.MEM.varpopKBmemused","BMK.MEM.avgmemused","BMK.MEM.maxmemused","BMK.MEM.minmemused","BMK.MEM.sdpopmemused","BMK.MEM.varpopmemused","BMK.MEM.avgKBbuffers","BMK.MEM.maxKBbuffers","BMK.MEM.minKBbuffers","BMK.MEM.sdpopKBbuffers","BMK.MEM.varpopKBbuffers","BMK.MEM.avgKBcached","BMK.MEM.maxKBcached","BMK.MEM.minKBcached","BMK.MEM.sdpopKBcached","BMK.MEM.varpopKBcached","BMK.MEM.avgKBcommit","BMK.MEM.maxKBcommit","BMK.MEM.minKBcommit","BMK.MEM.sdpopKBcommit","BMK.MEM.varpopKBcommit","BMK.MEM.avgcommit","BMK.MEM.maxcommit","BMK.MEM.mincommit","BMK.MEM.sdpopcommit","BMK.MEM.varpopcommit","BMK.MEM.avgKBactive","BMK.MEM.maxKBactive","BMK.MEM.minKBactive","BMK.MEM.sdpopKBactive","BMK.MEM.varpopKBactive","BMK.MEM.avgKBinact","BMK.MEM.maxKBinact","BMK.MEM.minKBinact","BMK.MEM.sdpopKBinact","BMK.MEM.varpopKBinact","BMK.NET.avgRXpcks","BMK.NET.maxRXpcks","BMK.NET.minRXpcks","BMK.NET.sdpopRXpcks","BMK.NET.varpopRXpcks","BMK.NET.sumRXpcks","BMK.NET.avgTXpcks","BMK.NET.maxTXpcks","BMK.NET.minTXpcks","BMK.NET.sdpopTXpcks","BMK.NET.varpopTXpcks","BMK.NET.sumTXpcks","BMK.NET.avgRXKBs","BMK.NET.maxRXKBs","BMK.NET.minRXKBs","BMK.NET.sdpopRXKBs","BMK.NET.varpopRXKBs","BMK.NET.sumRXKBs","BMK.NET.avgTXKBs","BMK.NET.maxTXKBs","BMK.NET.minTXKBs","BMK.NET.sdpopTXKBs","BMK.NET.varpopTXKBs","BMK.NET.sumTXKBs","BMK.NET.avgRXcmps","BMK.NET.maxRXcmps","BMK.NET.minRXcmps","BMK.NET.sdpopRXcmps","BMK.NET.varpopRXcmps","BMK.NET.sumRXcmps","BMK.NET.avgTXcmps","BMK.NET.maxTXcmps","BMK.NET.minTXcmps","BMK.NET.sdpopTXcmps","BMK.NET.varpopTXcmps","BMK.NET.sumTXcmps","BMK.NET.avgRXcsts","BMK.NET.maxRXcsts","BMK.NET.minRXcsts","BMK.NET.sdpopRXcsts","BMK.NET.varpopRXcsts","BMK.NET.sumRXcsts","BMK.DSK.avgtps","BMK.DSK.maxtps","BMK.DSK.mintps","BMK.DSK.avgRDs","BMK.DSK.maxRDs","BMK.DSK.minRDs","BMK.DSK.sdpopRDs","BMK.DSK.varpopRDs","BMK.DSK.sumRDs","BMK.DSK.avgWRs","BMK.DSK.maxWRs","BMK.DSK.minWRs","BMK.DSK.sdpopWRs","BMK.DSK.varpopWRs","BMK.DSK.sumWRs","BMK.DSK.avgReqs","BMK.DSK.maxReqs","BMK.DSK.minReqs","BMK.DSK.sdpopReqs","BMK.DSK.varpopReqs","BMK.DSK.avgQus","BMK.DSK.maxQus","BMK.DSK.minQus","BMK.DSK.sdpopQus","BMK.DSK.varpopQus","BMK.DSK.avgwait","BMK.DSK.maxwait","BMK.DSK.minwait","BMK.DSK.sdpopwait","BMK.DSK.varpopwait","BMK.DSK.avgutil","BMK.DSK.maxutil","BMK.DSK.minutil","BMK.DSK.sdpoputil","BMK.DSK.varpoputil","BMK.DSK.avgsvctm","BMK.DSK.maxsvctm","BMK.DSK.minsvctm","BMK.DSK.sdpopsvctm","BMK.DSK.varpopsvctm");
		params[["vinst"]] <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster","Datanodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Service.Type","Bench.Type","Hadoop.Version","Datasize","Scale.Factor","Net.maxtxKB.s","Net.maxrxKB.s","Net.maxtxPck.s","Net.maxrxPck.s","Net.maxtxCmp.s","Net.maxrxCmp.s","Net.maxrxmsct.s","Disk.maxtps","Disk.maxsvctm","Disk.maxrd.s","Disk.maxwr.s","Disk.maxrqsz","Disk.maxqusz","Disk.maxawait","Disk.maxutil","BMK.CPU.avguser","BMK.CPU.maxuser","BMK.CPU.minuser","BMK.CPU.sdpopuser","BMK.CPU.varpopuser","BMK.CPU.avgnice","BMK.CPU.maxnice","BMK.CPU.minnice","BMK.CPU.sdpopnice","BMK.CPU.varpopnice","BMK.CPU.avgsystem","BMK.CPU.maxsystem","BMK.CPU.minsystem","BMK.CPU.sdpopsystem","BMK.CPU.varpopsystem","BMK.CPU.avgiowait","BMK.CPU.maxiowait","BMK.CPU.miniowait","BMK.CPU.sdpopiowait","BMK.CPU.varpopiowait","BMK.CPU.avgsteal","BMK.CPU.maxsteal","BMK.CPU.minsteal","BMK.CPU.sdpopsteal","BMK.CPU.varpopsteal","BMK.CPU.avgidle","BMK.CPU.maxidle","BMK.CPU.minidle","BMK.CPU.sdpopidle","BMK.CPU.varpopidle","BMK.MEM.avgKBmemfree","BMK.MEM.maxKBmemfree","BMK.MEM.minKBmemfree","BMK.MEM.sdpopKBmemfree","BMK.MEM.varpopKBmemfree","BMK.MEM.avgKBmemused","BMK.MEM.maxKBmemused","BMK.MEM.minKBmemused","BMK.MEM.sdpopKBmemused","BMK.MEM.varpopKBmemused","BMK.MEM.avgmemused","BMK.MEM.maxmemused","BMK.MEM.minmemused","BMK.MEM.sdpopmemused","BMK.MEM.varpopmemused","BMK.MEM.avgKBbuffers","BMK.MEM.maxKBbuffers","BMK.MEM.minKBbuffers","BMK.MEM.sdpopKBbuffers","BMK.MEM.varpopKBbuffers","BMK.MEM.avgKBcached","BMK.MEM.maxKBcached","BMK.MEM.minKBcached","BMK.MEM.sdpopKBcached","BMK.MEM.varpopKBcached","BMK.MEM.avgKBcommit","BMK.MEM.maxKBcommit","BMK.MEM.minKBcommit","BMK.MEM.sdpopKBcommit","BMK.MEM.varpopKBcommit","BMK.MEM.avgcommit","BMK.MEM.maxcommit","BMK.MEM.mincommit","BMK.MEM.sdpopcommit","BMK.MEM.varpopcommit","BMK.MEM.avgKBactive","BMK.MEM.maxKBactive","BMK.MEM.minKBactive","BMK.MEM.sdpopKBactive","BMK.MEM.varpopKBactive","BMK.MEM.avgKBinact","BMK.MEM.maxKBinact","BMK.MEM.minKBinact","BMK.MEM.sdpopKBinact","BMK.MEM.varpopKBinact","BMK.NET.avgRXpcks","BMK.NET.maxRXpcks","BMK.NET.minRXpcks","BMK.NET.sdpopRXpcks","BMK.NET.varpopRXpcks","BMK.NET.sumRXpcks","BMK.NET.avgTXpcks","BMK.NET.maxTXpcks","BMK.NET.minTXpcks","BMK.NET.sdpopTXpcks","BMK.NET.varpopTXpcks","BMK.NET.sumTXpcks","BMK.NET.avgRXKBs","BMK.NET.maxRXKBs","BMK.NET.minRXKBs","BMK.NET.sdpopRXKBs","BMK.NET.varpopRXKBs","BMK.NET.sumRXKBs","BMK.NET.avgTXKBs","BMK.NET.maxTXKBs","BMK.NET.minTXKBs","BMK.NET.sdpopTXKBs","BMK.NET.varpopTXKBs","BMK.NET.sumTXKBs","BMK.NET.avgRXcmps","BMK.NET.maxRXcmps","BMK.NET.minRXcmps","BMK.NET.sdpopRXcmps","BMK.NET.varpopRXcmps","BMK.NET.sumRXcmps","BMK.NET.avgTXcmps","BMK.NET.maxTXcmps","BMK.NET.minTXcmps","BMK.NET.sdpopTXcmps","BMK.NET.varpopTXcmps","BMK.NET.sumTXcmps","BMK.NET.avgRXcsts","BMK.NET.maxRXcsts","BMK.NET.minRXcsts","BMK.NET.sdpopRXcsts","BMK.NET.varpopRXcsts","BMK.NET.sumRXcsts","BMK.DSK.avgtps","BMK.DSK.maxtps","BMK.DSK.mintps","BMK.DSK.avgRDs","BMK.DSK.maxRDs","BMK.DSK.minRDs","BMK.DSK.sdpopRDs","BMK.DSK.varpopRDs","BMK.DSK.sumRDs","BMK.DSK.avgWRs","BMK.DSK.maxWRs","BMK.DSK.minWRs","BMK.DSK.sdpopWRs","BMK.DSK.varpopWRs","BMK.DSK.sumWRs","BMK.DSK.avgReqs","BMK.DSK.maxReqs","BMK.DSK.minReqs","BMK.DSK.sdpopReqs","BMK.DSK.varpopReqs","BMK.DSK.avgQus","BMK.DSK.maxQus","BMK.DSK.minQus","BMK.DSK.sdpopQus","BMK.DSK.varpopQus","BMK.DSK.avgwait","BMK.DSK.maxwait","BMK.DSK.minwait","BMK.DSK.sdpopwait","BMK.DSK.varpopwait","BMK.DSK.avgutil","BMK.DSK.maxutil","BMK.DSK.minutil","BMK.DSK.sdpoputil","BMK.DSK.varpoputil","BMK.DSK.avgsvctm","BMK.DSK.maxsvctm","BMK.DSK.minsvctm","BMK.DSK.sdpopsvctm","BMK.DSK.varpopsvctm");
	}

	if (opt$method %in% c("aloja_precision","aloja_precision_split","aloja_representative_tree"))
	{
		if (is.null(opt$vout)) params[["vout"]] <- "Exe.Time";
		if (is.null(opt$vin)) params[["vin"]] = c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster","Datanodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Service.Type","Bench.Type","Hadoop.Version","Datasize","Scale.Factor");
	}

	if (is.null(params$vin) && opt$method  %in% c("aloja_print_individual_summaries","aloja_print_summaries"))
	{
		params[["vin"]] <- c("Exe.Time","Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster","Cl.Name","Datanodes","VM.OS","VM.Cores","VM.RAM","Provider","VM.Size","Service.Type","Bench.Type","Hadoop.Version","Datasize","Scale.Factor");
	}

	if (opt$method  == "aloja_diversity")
	{
		if (is.null(params$vin)) params[["vin"]] <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size");
		if (is.null(params$vdisc)) params[["vdisc"]] <- "Cl.Name";
		if (is.null(params$vout)) params[["vout"]] <- "Exe.Time";
	}

	if (is.null(params$vin) && opt$method  == "aloja_reunion")
	{
		params[["vin"]] <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size");
	}

	if (opt$method  %in% c("aloja_variable_relations","aloja_variable_quicklm"))
	{
		if (is.null(opt$vout)) params[["vout"]] <- "Exe.Time";
		if (is.null(opt$vin)) params[["vin"]] <- c("Net","Disk","Maps","IO.SFac","Rep","Comp","IO.FBuf","Blk.size","Datanodes","VM.Cores","VM.RAM");
	}

	if (opt$method  %in% c("aloja_bestrules_single","aloja_bestrules_single_select","aloja_bestrules_pairs","aloja_bestrules_pairs_select","aloja_bestrules_relations","aloja_bestrules_relations_select"))
	{
		if (is.null(params$vin)) params[["vin"]] <- c("Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Datanodes","VM.Cores","VM.RAM");
		options(width=1000);
	}

	if (!is.null(opt$learned))
	{
		params_2 <- list();
		params_2[["tagname"]] <- opt$learned;
		params[["learned_model"]] <- do.call(aloja_load_object,params_2);
	}

	if (!is.null(opt$params))
	{
		saux_1 <- strsplit(opt$params, ":");
		saux_2 <- strsplit(saux_1[[1]],"=");

		for (i in 1:length(saux_2))
		{
			if (is.na(saux_2[[i]][1])) next;
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

