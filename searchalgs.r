
# Josep Ll. Berral-García
# ALOJA-BSC-MSRC hadoop.bsc.es
# 2015-03-09
# Heuristics and search library for ALOJA-ML

source("functions.r");
library("genalg");

###############################################################################
# Search algorithms based on heuristics                                       #
###############################################################################

gv_ref_model = NULL;
gv_ds_example = NULL;
gv_vin_simple = NULL;
gv_vin_complete = NULL;

aloja_genalg_search <- function (expression, vin, reference_model, vin_complete = NULL, iters = 20, mutation = 0.02)
{
	if (!is.numeric(mutation)) mutation <- as.numeric(mutation);
	if (!is.integer(iters)) iters <- as.integer(iters);

	dsex <- aloja_unfold_expression(expression,vin,reference_model);
	dsaux <- aloja_dbind(reference_model$dataset[,vin],dsex);
	dsaux <- aloja_binarize_ds(dsaux);	
	dsexb <- dsaux[(nrow(reference_model$dataset)+1):nrow(dsaux),];

	baseMin <- as.vector(sapply(colnames(dsexb),function(x) min(dsexb[,x])));
	baseMax <- as.vector(sapply(colnames(dsexb),function(x) max(dsexb[,x])));

	gv_ref_model <<- reference_model;
	gv_ds_example <<- dsexb;
	gv_vin_simple <<- vin;
	if (!is.null(vin_complete)) gv_vin_complete <<- vin_complete; # Ad_Hoc stuff for Aloja Clusters # TODO - Set the ones that miss from ref$varin from vin

	rbga.results <- rbga(baseMin, baseMax, evalFunc=aloja_genalg_evaluate, verbose=FALSE, mutationChance=mutation, iters=iters);

	binstance <- dsexb[0,];
	baux <- rbga.results$population[which(rbga.results$mean == min(rbga.results$mean)),];
	dummy <- ifelse (is.null(nrow(baux)), binstance[1,] <- baux, binstance[1,] <- baux[1,]);
	best_inst <- aloja_debinarize_instance(reference_model$dataset[,vin], vin, binstance);

	retval <- list();

	Prediction <- min(rbga.results$mean);
	retval[["best.inst"]] <- cbind(best_inst,Prediction);
	retval[["results"]] <- rbga.results;

	retval;
}

aloja_genalg_evaluate <- function (string=c())
{
	returnVal = NA;
	if (length(string) == ncol(gv_ds_example))
	{
		binstance <- gv_ds_example[0,];
		binstance[1,] <- string;
		dsdbin <- aloja_debinarize_instance(gv_ref_model$dataset[,gv_vin_simple], gv_vin_simple, binstance);
		viable <- all(!is.na(dsdbin));

		if (!is.null(gv_vin_complete)) # Ad_Hoc stuff for Aloja Clusters
		{
			extra <- (gv_ref_model$dataset[gv_ref_model$dataset$Cluster==dsdbin$Cluster,gv_vin_complete])[1,];
			dsdbin <- cbind(dsdbin,extra);
		}

		returnVal <- 9E15;
		if (viable) returnVal <- aloja_predict_instance(gv_ref_model,gv_ref_model$varin,dsdbin)[,"Prediction"];
	} else {
		stop(paste("Expecting a chromosome of length",ncol(gv_ds_example),"!",sep=" "));
	}
	returnVal;
}

