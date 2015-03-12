
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

###############################################################################
# Algorithms to decompose search results into Decision Trees                  #
###############################################################################

aloja_representative_tree <- function (predicted_instances = NULL, vin, method = "ordered", pred_file = NULL, dump_file = NULL, output = NULL, ...)
{
	if (is.null(predicted_instances) && is.null(pred_file) && is.null(dump_file)) return (NULL);
	if (is.null(predicted_instances) && !is.null(pred_file)) predicted_instances <- read.table(paste(pred_file,"-predictions.data",sep=""),sep=",",header=TRUE,stringsAsFactors=FALSE);
	if (is.null(predicted_instances) && !is.null(dump_file))
	{
		predicted_instances <- (read.table(dump_file,sep="",header=FALSE,stringsAsFactors=FALSE))[,c(2,3)];
		colnames(predicted_instances) <- c("Instance","Prediction");
	}

	b <- sapply(predicted_instances$Instance,function(x) strsplit(x,","));
	b <- as.data.frame(t(matrix(unlist(b),nrow=length(vin))));
	b <- cbind(b,predicted_instances$Prediction);
	colnames(b) <- c(vin,"Prediction");
	bord <- b[order(b$Prediction),];

	#daux <- rpart(Prediction ~., data = bord, parms=list(split='gini'));
	#daux <- rpart(Prediction ~., data = baux, control=rpart.control(minsplit = 2), parms=list(split='gini')); var1 <- rownames(daux$splits)[1];
	# eq: rpart(Prediction ~., data = bord, control=rpart.control(minsplit = 2), method = "class"); # This does the same that gini...

	gini_improvement <- function (ds, var1, target)
	{
		gini <- function(x, unbiased = FALSE)
		{
		    n <- length(x)
		    mu <- mean(x)
		    N <- if (unbiased) n * (n - 1) else n * n
		    ox <- x[order(x)];
		    dsum <- drop(crossprod(2 * 1:n - n - 1,  ox))
		    dsum / (mu * N)
		}
	
		impurities <- NULL;
		nobs <- NULL;
		for (i in levels(as.factor(ds[,var1])))
		{
			if (nrow(ds[ds[,var1]==i,]) > 0)
			{
				impurities <- c(impurities,gini(ds[ds[,var1]==i,target]));
				nobs <- c(nobs,nrow(ds[ds[,var1]==i,]));
			}
		}
		sum_nobs <- nrow(ds);

		impurity_root<- gini(ds[,target]);
		impurity_sum <- sum(sapply(1:length(impurities),function(x) impurities[x]*(nobs[x]/sum_nobs)));
		impurity_root - impurity_sum ;
	}
	
	attrib_search <- function (baux,level=0,method="ordered")
	{
		retval <- NULL;
		if (nrow(baux) > 1)
		{
			if (method == "ordered") # Using "ordered changes" method
			{
				ns <- colnames(baux)[!colnames(baux) %in% "Prediction"];

				changes <- NULL;
				chnames <- NULL;
				for (i in ns)
				{
					change <- 0;
					for (j in 2:nrow(baux))
					{
						if (baux[j-1,i] != baux[j,i]) change <- change + 1;
					}
					if (change > 0)
					{
						changes <- c(changes,change);
						chnames <- c(chnames,i);
					}
				}
				var1 <- chnames[(which(changes==min(changes)))[1]];

			} else if (method == "information") # Using information gain
			{
				daux <- information.gain(Prediction~., baux);
				var1 <- (rownames(daux)[which(daux==max(daux))])[1];

			} else if (method == "gini") # Using gini improvement
			{
				ns <- colnames(baux)[!colnames(baux) %in% "Prediction"];
				daux <- sapply(ns, function(x) gini_improvement(baux,x,"Prediction"));
				var1 <- (names(daux)[which(daux==max(daux))])[1];
			}
		
			retval <- list();
			for (i in levels(baux[,var1])) # TODO - Executar en Ordre
			{
				bnext <- baux[baux[,var1]==i,];
				retval[[paste(var1,i,sep="=")]] <- attrib_search(bnext,level=level+1,method=method);
			}
		} else {
			retval <- round(mean(baux$Prediction));
		}
		retval;
	}
	stree <- attrib_search(bord,method=method);

	retval <- stree
	if (!is.null(output) && output=="string") retval <- aloja_repress_tree_string (stree);
	if (!is.null(output) && output=="ascii") retval <- aloja_repress_tree_ascii (stree);

	retval;	
}

aloja_repress_tree_string <- function (stree)
{
	if (!is.numeric(stree))
	{
		levelval <- '';
		for(i in names(stree))
		{
			plevelval <- aloja_repress_tree_string (stree[[i]]);
			if (levelval != '') levelval <- paste(levelval,",",sep="");
			levelval <- paste(levelval,i,":",plevelval,sep="");
		}
		retval <- paste("{",levelval,"}",sep="");
	} else {
		retval <- stree;
	}
	retval;
}

aloja_repress_tree_ascii <- function (stree, level = 0)
{
	retval <- NULL;
	for(i in names(stree))
	{
		spaces <- paste(c(rep("  ",level),"*"),sep="",collapse="");
		icute <- str_replace(i,"="," : ");
		if (is.numeric(stree[[i]]))
		{
			retval <- c(retval,paste(spaces,icute,"->",mean(stree[[i]]),sep=" "));
		} else {
			retval <- c(retval,paste(spaces,icute,sep=" "));
			plevelval <- aloja_repress_tree_ascii (stree[[i]], level=level+1);
			retval <- c(retval,plevelval);
		}
	}

	if (level == 0) retval <- as.matrix(retval);
	retval;
}