# Josep Ll. Berral-García
# ALOJA-BSC-MSR aloja.bsc.es
# 2016-02-20
# Deprecated and Unused Functions for ALOJA-ML, kept for LEGACY and FUTURE

###############################################################################
# Print all variable crossing in a temporary directory                        #
###############################################################################

aloja_crossvariables <- function (ds, pnglabel = "cross", jfactor = 0)
{
	numaux <- sapply(data.frame(ds), is.numeric);

	system("mkdir -p temp");
	for (var1 in 1:(ncol(ds)-1))
	{
		if (numaux[var1])
		{
			auxdata1 <- ds[,var1];
		} else {
			auxdata1 <- match(as.factor(ds[,var1]),levels(as.factor(ds[,var1])));
		}
		auxlabel1 <- colnames(ds)[var1];

		for (var2 in (var1 + 1):ncol(ds))
		{
			if (numaux[var2])
			{
				auxdata2 <- ds[,var2];
			} else {
				auxdata2 <- match(as.factor(ds[,var2]),levels(as.factor(ds[,var2])));
			}
			auxlabel2 <- colnames(ds)[var2];

			auxframe <- na.omit(data.frame(auxdata1, auxdata2));
			png(paste("temp/",pnglabel,"-",var1,"-",var2,".png",sep=""),width=1000,height=500);
				if (!numaux[var2]) par(oma=c(0,0,0,2));
				plot(jitter(auxframe[,1],factor=jfactor),jitter(auxframe[,2],factor=jfactor),xlim=c(min(auxframe[,1]),max(auxframe[,1])),ylim=c(min(auxframe[,2]),max(auxframe[,2])), xlab=auxlabel1, ylab=auxlabel2);
				if (!numaux[var1]) axis(3, at=seq(1,length(levels(as.factor(ds[,var1])))), labels=levels(as.factor(ds[,var1])), las=2);
				if (!numaux[var2]) axis(4, at=seq(1,length(levels(as.factor(ds[,var2])))), labels=levels(as.factor(ds[,var2])), las=2);
			dev.off();
		}		
	}
}

###############################################################################
# ANOVA of current variables                                                  #
###############################################################################

aloja_anova <- function (ds)
{
	anova_1 <- list();
	anova_1[["alpha"]] <- 0.05;
	anova_1[["N"]] <- NULL;
	anova_1[["K"]] <- NULL;
	anova_1[["gmean"]] <- NULL;
	anova_1[["ssb"]] <- 0;
	anova_1[["ssw"]] <- 0;
	anova_1[["mse"]] <- NULL;
	anova_1[["f"]] <- NULL;

	bmks <- list();
	anova_1[["means"]] <- list();
	anova_1[["stdevs"]] <- list();

	for (i in levels(ds[,"Benchmark"]))
	{
		bmks[[i]] <- ds[ds[,"Benchmark"]==i,c("Exe.Time","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster")];
		anova_1$means[[i]] <- mean(bmks[[i]][,"Exe.Time"]);
		anova_1$stdevs[[i]] <- sd(bmks[[i]][,"Exe.Time"]);
	}
	anova_1$gmean <- mean(rapply(anova_1$means,function(x) x));

	for (i in levels(ds[,"Benchmark"]))
	{
		anova_1$ssb <- anova_1$ssb + (length(bmks[[i]][,"Exe.Time"]) * (anova_1$means[[i]] - anova_1$gmean)^2);
	}

	anova_1$N <- nrow(ds);
	anova_1$K <- length(levels(ds[,"Benchmark"]));
	for (i in 1:anova_1$N)
	{
		anova_1$ssw <- anova_1$ssw + (ds[i,"Exe.Time"] - anova_1$means[[ds[i,"Benchmark"]]])^2;
	}
	anova_1$mse <- anova_1$ssw / (anova_1$N - anova_1$K);
	anova_1$f <- (anova_1$ssb / (anova_1$K - 1)) / anova_1$mse;
	anova_1$critical <- qf(1-anova_1$alpha, anova_1$K-1, anova_1$N-1);

	print (c("Means are equal: ",anova_1$f < anova_1$critical));

	anova_1;
}

###############################################################################
# Search algorithms based on heuristics                                       #
###############################################################################

library("genalg");
library("FSelector");

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
# Principal Component Analysis methods                                        #
###############################################################################

aloja_pca <- function (ds, vin, vout, pngpca = NULL, saveall = NULL)
{
	vinaux <- vin;

	dsbin <- aloja_binarize_ds(ds[,c(vout,vin)]);
	vin <- colnames(dsbin[,-1]);

	pc <- princomp(dsbin[,vin]);
	pc[["dataset"]] <- cbind(dataset[,"ID"],dsbin);
	colnames(pc$dataset) <- c("ID",colnames(dsbin));
	pc[["pcaset"]] <- cbind(dataset[,"ID"],dsbin[,vout],pc$scores);
	colnames(pc$pcaset) <- c("ID",vout,colnames(pc$scores));

	pc[["vin_orig"]] <- vinaux;
	pc[["vin"]] <- vin;
	pc[["vout"]] <- vout;

	if (!is.null(pngpca))
	{
		system("mkdir -p temp");
		for (var1 in 1:(length(pc$scores[1,])-1))
		{
			for (var2 in (var1 + 1):ncol(pc$scores))
			{
				png(paste("temp/",pngpca,"-",var1,"-",var2,".png",sep=""),width=1000,height=500);
					plot(1, type="n", xlim=c(min(pc$scores[,var1]),max(pc$scores[,var1])),ylim=c(min(pc$scores[,var2]),max(pc$scores[,var2])), xlab="", ylab="");
					points(jitter(pc$scores[dsbin[,vout]>3000,var1],factor=0.6),jitter(pc$scores[dsbin[,vout]>3000,var2],factor=0.6),col="red");
					points(jitter(pc$scores[dsbin[,vout]>2000 & dsbin[,vout]<3000,var1],factor=0.6),jitter(pc$scores[dsbin[,vout]>2000 & dsbin[,vout]<3000,var2],factor=0.6),col="blue");
					points(jitter(pc$scores[dsbin[,vout]>1000 & dsbin[,vout]<2000,var1],factor=0.6),jitter(pc$scores[dsbin[,vout]>1000 & dsbin[,vout]<2000,var2],factor=0.6),col="green");
					points(jitter(pc$scores[dsbin[,vout]<1000,var1],factor=0.6),jitter(pc$scores[dsbin[,vout]<1000,var2],factor=0.6),col="black");
				dev.off();
			}
		}
	}

	if (!is.null(saveall))
	{
		write.table(pc$dataset, file = paste(saveall,"-dataset.csv",sep=""), sep = ",", row.names=FALSE);
		write.table(pc$pcaset, file = paste(saveall,"-transformed.csv",sep=""), sep = ",", row.names=FALSE);
		aloja_save_object(pc,tagname=saveall);
	}

	pc;
}

aloja_transform_data <- function (ds, pca_obj = NULL, pca_name = NULL, saveall = NULL)
{
	retval <- NULL;
	
	if (is.null(pca_name) && is.null(pca_obj))
	{
		print("[WARNING] No PCA object or file introduced");
		retval;
	}

	if (!is.null(pca_name))	pca_obj <- aloja_load_object(pca_name);

	dsbaux <- aloja_binarize_ds(ds[,pca_obj$vin_orig]);
	vin <- colnames(dsbaux);

	retval[["dataset"]] <- dsbaux;
	retval[["vin"]] <- vin;
	retval[["pca"]] <- pca_obj;

	dspca <- predict(pca_obj,dsbaux);
	retval[["pcaset"]] <- dspca;	

	if (!is.null(saveall))
	{
		write.table(dspca, file = paste(saveall,"-transformed.csv",sep=""), sep = ",", row.names=FALSE);
	}

	retval;
}

aloja_transform_instance <- function (inst_transform, pca_obj = NULL, pca_name = NULL, verbose = 0)
{
	if (!is.integer(verbose)) verbose <- as.integer(verbose);

	retval <- NULL;
	
	if (is.null(pca_name) && is.null(pca_obj))
	{
		print("[WARNING] No PCA object or file introduced");
		retval;
	}

	if (!is.null(pca_name)) pca_obj <- aloja_load_object(pca_name);

	datamodel <- pca_obj$dataset[0,pca_obj$vin];
	for (name_1 in colnames(datamodel))
	{
		if (name_1 %in% pca_obj$vin_orig)
		{
			var_aux <- inst_transform[which(pca_obj$vin_orig==name_1)];
			class(var_aux) <- class(datamodel[0,name_1]);
			datamodel[1,name_1] <- var_aux;
		} else {
			if (length(which(inst_transform==name_1)) >= 1)
			{
				datamodel[1,name_1] <- 1;
			} else {
				datamodel[1,name_1] <- 0;
			}
		}
	}
	vin <- colnames(datamodel);

	retval[["instance"]] <- datamodel;
	retval[["vin"]] <- vin;
	retval[["pca"]] <- pca_obj;

	dspca <- predict(pca_obj,datamodel);
	retval[["pcainst"]] <- dspca;

	if (verbose == 2)
	{
		retval;
	} else if (verbose == 1) {
		retval$pcainst;
	} else {
		paste(retval$pcainst,collapse=",");
	}
}


aloja_dataset_collapse <- function (ds, vin, vout, dimension1, dimension2, dimname1, dimname2, model_obj = NULL, model_name = NULL, saveall = NULL, preds = "after")
{
	retval <- list();

	dsid <- ds[,"ID"];
	ds <- ds[,c(vout,vin)];

	dsaux <- cbind(ds[,vout],apply(as.matrix(ds[,dimension1]),1,paste,collapse=":"),apply(as.matrix(ds[,dimension2]),1,paste,collapse=":"));
	colnames(dsaux) <- c(vout,dimname1,dimname2);

	maux <- matrix(NA,length(levels(as.factor(dsaux[,dimname1]))),length(levels(as.factor(dsaux[,dimname2]))));
	colnames(maux) <- levels(as.factor(dsaux[,dimname2]));
	rownames(maux) <- levels(as.factor(dsaux[,dimname1]));

	midaux <- matrix(NA,length(levels(as.factor(dsaux[,dimname1]))),length(levels(as.factor(dsaux[,dimname2]))));
	colnames(midaux) <- levels(as.factor(dsaux[,dimname2]));
	rownames(midaux) <- levels(as.factor(dsaux[,dimname1]));

	for (i in 1:nrow(dsaux))
	{
		dim1_aux <- dsaux[i,dimname1];
		dim2_aux <- dsaux[i,dimname2];

		midaux[dim1_aux,dim2_aux] <- ifelse(!is.na(midaux[dim1_aux,dim2_aux]),c(midaux[dim1_aux,dim2_aux], dsid[i]),dsid[i]);
		len_aux <- length(midaux[dim1_aux,dim2_aux]);

		prev_val <- ifelse(!is.na(maux[dim1_aux,dim2_aux]),as.numeric(maux[dim1_aux,dim2_aux]),0);
		maux[dim1_aux,dim2_aux] <- (prev_val * (len_aux-1) + as.numeric(dsaux[i,vout])) / len_aux;
	}

	if (!is.null(model_name)) model_obj <- aloja_load_object(model_name);

	# IMPORTANT - NA filling is done AFTER aggregation, so estimated values DO NOT affect aggregation
	if ((preds == "after") && !is.null(model_obj) && all(c(model_obj$varin %in% c(dimension1,dimension2),c(dimension1,dimension2) %in% model_obj$varin)))
	{
		for (i in 1:length(maux))
		{
			if (is.na(maux[i]))
			{
				row_aux <- ((i-1) %% nrow(maux)) + 1;
				col_aux <- ((i-1) %/% nrow(maux)) + 1;

				dim1_aux <- rownames(maux)[row_aux];
				dim2_aux <- colnames(maux)[col_aux];

				inst_aux <- c(strsplit(dim1_aux,split=":")[[1]],strsplit(dim2_aux,split=":")[[1]]);

				maux[i] <- aloja_predict_instance (model_obj,c(dimension1,dimension2),inst_aux);
			}
		}
	}

	# IMPORTANT - NA filling is done simulating values BEFORE aggregation, so estimated values DO affect aggregation
	if ((preds != "after") && !is.null(model_obj))
	{
		for (i in 1:length(maux))
		{
			if (is.na(maux[i]))
			{
				row_aux <- ((i-1) %% nrow(maux)) + 1;
				col_aux <- ((i-1) %/% nrow(maux)) + 1;

				dim1_aux <- rownames(maux)[row_aux];
				dim2_aux <- colnames(maux)[col_aux];

				split1 <- strsplit(dim1_aux,split=":")[[1]];
				split2 <- strsplit(dim2_aux,split=":")[[1]];

				inst_aux <- NULL;
				for (j in 1:length(model_obj$varin))
				{
					if (model_obj$varin[j] %in% dimension1) inst_aux <- c(inst_aux,sub("^\\s+",'',split1[which(dimension1 == model_obj$varin[j])]));
					if (model_obj$varin[j] %in% dimension2) inst_aux <- c(inst_aux,sub("^\\s+",'',split2[which(dimension2 == model_obj$varin[j])]));
					if (!(model_obj$varin[j] %in% c(dimension1,dimension2))) inst_aux <- c(inst_aux,'*');
				}

				piaux <- aloja_predict_instance (model_obj,vin,inst_aux);
				maux[i] <- mean(piaux[,"Prediction"]);
			}
		}
	}

	retval[["matrix"]] <- maux;
	retval[["IDs"]] <- midaux;
	retval[["collapsed"]] <- dimension2;

	if (!is.null(saveall))
	{
		write.table(retval$matrix,file=paste(saveall,"-matrix.csv",sep=""),sep=",",row.names=TRUE);
		write.table(retval$IDs,file=paste(saveall,"-ids.csv",sep=""),sep=",",row.names=TRUE);
		aloja_save_object(retval,tagname=saveall);
	}

	retval;
}

aloja_dataset_collapse_expand <- function (ds, vin, vout, dimension1, dimension2, dimname1, dimname2, inst_general, model_obj = NULL, model_name = NULL, saveall = NULL)
{
	retval <- list();

	datacol_obj <- aloja_dataset_collapse (ds,vin,vout,dimension1,dimension2,dimname1,dimname2);

	if (!is.null(model_name)) model_obj <- aloja_load_object(model_name);
	predicted_obj <- aloja_predict_instance(model_obj,vin,inst_general,sorted=TRUE);

	pmat <- cbind(predicted_obj[,"Prediction"],str_split_fixed(predicted_obj[,"Instance"], ",",length(vin)));
	colnames(pmat) <- c(vout,vin);

	dsaux <- cbind(pmat[,vout],apply(as.matrix(pmat[,dimension1]),1,paste,collapse=":"),apply(as.matrix(pmat[,dimension2]),1,paste,collapse=":"));
	colnames(dsaux) <- c(vout,dimname1,dimname2);

	maux <- matrix(NA,length(levels(as.factor(dsaux[,dimname1]))),length(levels(as.factor(dsaux[,dimname2]))));
	colnames(maux) <- levels(as.factor(dsaux[,dimname2]));
	rownames(maux) <- levels(as.factor(dsaux[,dimname1]));

	midaux <- matrix(NA,length(levels(as.factor(dsaux[,dimname1]))),length(levels(as.factor(dsaux[,dimname2]))));
	colnames(midaux) <- levels(as.factor(dsaux[,dimname2]));
	rownames(midaux) <- levels(as.factor(dsaux[,dimname1]));

	for (i in 1:nrow(dsaux))
	{
		dim1_aux <- dsaux[i,dimname1];
		dim2_aux <- dsaux[i,dimname2];

		midaux[dim1_aux,dim2_aux] <- ifelse(!is.na(midaux[dim1_aux,dim2_aux]),c(midaux[dim1_aux,dim2_aux], i),i);
		len_aux <- length(midaux[dim1_aux,dim2_aux]);

		prev_val <- ifelse(!is.na(maux[dim1_aux,dim2_aux]),as.numeric(maux[dim1_aux,dim2_aux]),0);
		maux[dim1_aux,dim2_aux] <- (prev_val * (len_aux-1) + as.numeric(dsaux[i,vout])) / len_aux;
	}

	cmaux <- matrix(NA,length(levels(as.factor(dsaux[,dimname1]))),length(levels(as.factor(dsaux[,dimname2]))));
	colnames(cmaux) <- levels(as.factor(dsaux[,dimname2]));
	rownames(cmaux) <- levels(as.factor(dsaux[,dimname1]));

	cmidaux <- matrix(NA,length(levels(as.factor(dsaux[,dimname1]))),length(levels(as.factor(dsaux[,dimname2]))));
	colnames(cmidaux) <- levels(as.factor(dsaux[,dimname2]));
	rownames(cmidaux) <- levels(as.factor(dsaux[,dimname1]));

	for (i in 1:nrow(dsaux))
	{
		dim1_aux <- dsaux[i,dimname1];
		dim2_aux <- dsaux[i,dimname2];

		cmaux[dim1_aux,dim2_aux] <- maux[dim1_aux,dim2_aux];
		cmidaux[dim1_aux,dim2_aux] <- 'NA';

		if (dim1_aux %in% rownames(datacol_obj$matrix) && dim2_aux %in% colnames(datacol_obj$matrix))
		{
			if (!is.na(datacol_obj$matrix[dim1_aux,dim2_aux]))
			{
				cmaux[dim1_aux,dim2_aux] <- datacol_obj$matrix[dim1_aux,dim2_aux];
				cmidaux[dim1_aux,dim2_aux] <- datacol_obj$IDs[dim1_aux,dim2_aux];
			}
		} 
	}

	retval[["matrix"]] <- datacol_obj$matrix;
	retval[["pmatrix"]] <- maux;
	retval[["cmatrix"]] <- cmaux;
	retval[["IDs"]] <- datacol_obj$IDs;
	retval[["cIDs"]] <- cmidaux;
	retval[["collapsed"]] <- dimension2;

	if (!is.null(saveall))
	{
		write.table(retval$matrix,file=paste(saveall,"-matrix.csv",sep=""),sep=",",row.names=TRUE);
		write.table(retval$IDs,file=paste(saveall,"-ids.csv",sep=""),sep=",",row.names=TRUE);
		write.table(retval$pmatrix,file=paste(saveall,"-pmatrix.csv",sep=""),sep=",",row.names=TRUE);
		write.table(retval$cmatrix,file=paste(saveall,"-cmatrix.csv",sep=""),sep=",",row.names=TRUE);
		write.table(retval$cIDs,file=paste(saveall,"-cids.csv",sep=""),sep=",",row.names=TRUE);
		aloja_save_object(retval,tagname=saveall);
	}

	retval;
}

aloja_dataset_clustering <- function (datamatrix, k = 3, learned_model = NULL)
{
	if (class(datamatrix) == "character")
	{
		maux <- as.matrix(read.csv(file=datamatrix, header=TRUE, sep=",", check.names=FALSE));
	} else {
		maux <- datamatrix;
	}

	if (!is.null(learned_model) && all(c(learned_model$varin %in% c(dimension1,dimension2),c(dimension1,dimension2) %in% learned_model$varin)))
	{
		for (i in 1:length(maux))
		{
			if (is.na(maux[i]))
			{
				row_aux <- ((i-1) %% nrow(maux)) + 1;
				col_aux <- ((i-1) %/% nrow(maux)) + 1;

				dim1_aux <- rownames(maux)[row_aux];
				dim2_aux <- colnames(maux)[col_aux];

				inst_aux <- c(dim1_aux,strsplit(dim2_aux,split=":")[[1]]);

				maux[i] <- aloja_predict_instance (learned_model,c(dimension1,dimension2),inst_aux);
			}
		}
	} else {
		maux[is.na(maux)] <- 0;
	}
	retval <- kmeans(maux, k);

	retval;
}

###############################################################################
# Characterization methods                                                    #
###############################################################################

aloja_check_similarity <- function (ds_new, ds_gral, vin, vout, var.base, gmodel = NULL, lmodel = NULL, alpha = 0.05)
{
	retval <- list();

	anova1 <- list();
	for (i in levels(ds_gral[,var.base]))
	{
		if (!is.null(gmodel)) model_aux <- gmodel;
		if (!is.null(lmodel[[i]])) model_aux <- lmodel[[i]];
		if (is.null(model_aux))
		{
			print(paste("[ERROR] No suitable model found"));
			NULL;
		}

		ds_aux <- ds_new;
		ds_aux[,var.base] <- i;
		paux1 <- aloja_predict_dataset(learned_model=model_aux,vin=varin,ds=ds_aux[,vin]);
		paux2 <- aloja_predict_dataset(learned_model=model_aux,vin=varin,ds=ds_gral[ds_gral[,var.base]==i,vin]);

		perr1 <- paux1 - ds_aux[,vout];
		perr2 <- paux2 - ds_gral[ds_gral[,var.base]==i,vout];
	
		perry <- c(perr1,perr2);
		group <- rep(1:2,c(length(perr1),length(perr2)));
		daux = data.frame(y = perry, group = factor(group));
		anova1[[i]] <- anova(lm(perry~group,daux));
	}

	result <- t(sapply(lapply(anova1,function(x) x$"Pr(>F)"[1]),unlist));
	retval[["kind"]] <- "Prediction";
	retval[["anova"]] <- anova1;
	retval[["result"]] <- result;

	if (max(result) > alpha)
	{
		retval[["cluster"]] <- colnames(result)[which.max(result)];
		retval[["significance"]] <- max(result);
	}
	retval;
}

aloja_check_cluster <- function (kcluster, bmk.vector) # FIXME - Problema de distàncies amb 2 bmks només
{
	retval <- list();

	distances <- NULL;
	gen_distances <- NULL;
	for (i in 1:nrow(kcluster$centers))
	{
		distances <- c(distances,(sum((kcluster$centers[i,] - bmk.vector$matrix[1,])^2))^(1/2));

		for (j in i:nrow(kcluster$centers))
		{
			if (j == i) next;
			gen_distances <- c(gen_distances,(sum((kcluster$centers[i,] - kcluster$centers[j,])^2))^(1/2));
		}
	}

	retval[["kind"]] <- "Cluster";
	retval[["full.distances"]] <- gen_distances;
	retval[["distances"]] <- distances;

	if (max(gen_distances) > min(distances))
	{
		retval[["cluster"]] <- colnames(result)[which(kcluster$cluster == which.min(distances))];
		retval[["min.distance"]] <- min(distances);
	}
	retval;
}

aloja_best_configurations <- function (bvectors = NULL, bvec_name = NULL)
{
	if (is.null(bvectors) && is.null(bvec_name))
	{
		print("[WARNING] No Vector Matrix object or file introduced");
		NULL;
	}

	if (!is.null(bvec_name)) bvectors <- aloja_load_object(bvec_name);

	result <- data.frame(Config=character(),Variance=numeric(),Benchmarks=integer(),Missing=character(),stringsAsFactors=FALSE);
	for(i in 1:ncol(bvectors$matrix))
	{
		vaux <- var(bvectors$matrix[,i],na.rm=TRUE);
		raux <- is.na(bvectors$matrix[,i]);
		result <- rbind(result,data.frame(Config=colnames(bvectors$matrix)[i],Variance=vaux,Benchmarks=length(raux[raux==FALSE]),Missing=paste(names(raux[raux==TRUE]),collapse=",")));
	}

	colnames(result)[1] <- paste(bvectors$collapsed,collapse=":");
	result[order(-result[,2], -result[,3]),];
}

###############################################################################
# Save the datasets and created models                                        #
###############################################################################

library(session);

aloja_save_status <- function ()
{
	save.session("RData");
	savehistory("Rhistory");
	system(paste("tar cvzf rsession-",Sys.Date(),".tar.gz RData Rhistory",sep=""));
	system("rm RData Rhistory");
}

aloja_save_datasets <- function (traux_0, tvaux_0, ttaux_0, name_0, algor_0)
{
	write.table(tvaux_0, file = paste(algor_0,"-",name_0,"-tv.csv",sep=""), sep = ",");
	write.table(traux_0, file = paste(algor_0,"-",name_0,"-tr.csv",sep=""), sep = ",");
	write.table(ttaux_0, file = paste(algor_0,"-",name_0,"-tt.csv",sep=""), sep = ",");
}

aloja_save_model <- function (model_0, tagname = "default")
{
	saveRDS(model_0,file=paste(tagname,"-model.dat",sep=""));
}

aloja_load_model <- function (tagname = "default")
{
	model_1 <- readRDS(paste(tagname,"-model.dat",sep=""));
	model_1;
}

###############################################################################
# R hacks and operators                                                       #
###############################################################################

':=' <- function(lhs, rhs)
{
	frame <- parent.frame();
	lhs <- as.list(substitute(lhs));
	if (length(lhs) > 1)
		lhs <- lhs[-1];
	if (length(lhs) == 1)
	{
		do.call(`=`, list(lhs[[1]], rhs), envir=frame);
		return(invisible(NULL));
	}
	if (is.function(rhs) || is(rhs, 'formula'))
		rhs <- list(rhs);
	if (length(lhs) > length(rhs))
		rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)));
	for (i in 1:length(lhs))
		do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame);
	return(invisible(NULL));
}

shannon.entropy <- function(p)
{
	if (min(p) < 0 || sum(p) <= 0) return(NA);
	p.norm <- p[p > 0]/sum(p);
	-sum(log2(p.norm)*p.norm);
}

