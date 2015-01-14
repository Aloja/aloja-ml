
# Josep Ll. Berral-García
# ALOJA-BSC-MSR hadoop.bsc.es
# 2015-01-14
# Implementation of M5-Prediction-like method with Quadratic Regression

# Usage:
#	mtree <- m5pq_tree(formula = target ~ .,dataset = dataframe);
#	prediction <- m5pq.predict(mtree = mtree, newdata = dataframe);
#	m5pq.plot.tree(mtree);

library(rpart);

###############################################################################
# Regression Tree M5-Prediction-like with Quadratic Regression                #
###############################################################################

m5pq.tree <- function (formula, dataset)
{
	colnames(dataset) <- gsub(" ",".",colnames(dataset));

	vout <- get(as.character(formula[[2]]));
	vin <- if (as.character(formula[[3]]) == ".") colnames(dataset)[(!colnames(dataset) %in% vout)] else as.character(formula[[3]]);

	dataset <- dataset[,c(vout,vin)];

	fit <- rpart(formula=dataset[,vout]~.,data=dataset[,vin],method="anova");
	nodes <- as.numeric(rownames(mtree$rpart$frame));

	err_branch <- regs <- preds <- list();
	indexes <- NULL;
	mae <- rae <- 0;
	for (i in unique(fit$where))
	{
		daux <- dataset[fit$where==i,c(vout,vin)];
		j <- nodes[i];

		regs[[j]] <- lm(formula=daux[,vout] ~ . + (.)^2, data=data.frame(daux[,vin]));
		indexes <- c(indexes,j);

		preds[[j]] <- regs[[j]]$fitted.values;
		err_branch[[j]] <- paste(j,nrow(daux),mean(abs(preds[[j]] - daux[,vout])),mean(abs((preds[[j]] - daux[,vout])/daux[,vout])),sep=" ");

		mae <- mae + sum(abs(preds[[j]] - daux[,vout]));
		rae <- rae + sum(abs((preds[[j]] - daux[,vout])/daux[,vout]));
	}
	mae <- mae / nrow(dsbaux);
	rae <- rae / nrow(dsbaux);
	print(c(nrow(dsbaux),mae,rae));

	retval <- list();
	retval[["rpart"]] <- fit;
	retval[["regs"]] <- regs;
	retval[["indexes"]] <- indexes;
	retval[["mae"]] <- mae;
	retval[["rae"]] <- rae;

	err_data <- t(data.frame(strsplit(unlist(err_branch)," ")));
	rownames(err_data) <- err_data[,1];
	err_data <- data.frame(err_data[,-2]);
	colnames(err_data) <- c("Instances","MAE","MAPE");

	retval[["error_branch"]] <- err_data;	

	preds <- unlist(preds);
	retval[["fitted.values"]] <- preds[order(as.integer(names(preds)))];

	retval;
}

m5pq.predict <- function (model, newdata)
{
	retval <- NULL;
	colnames(newdata) <- gsub(" ",".",colnames(newdata));

	fit_node <- model$rpart;
	fit_node$frame$yval <- as.numeric(rownames(fit_node$frame));

	for (i in 1:nrow(newdata))
	{
		node <- as.numeric(predict(fit_node,newdata[i,]));

		pred <- as.numeric(predict(model$regs[[node]],newdata[i,]));
		retval <- c(retval,pred);
	}

	retval;
}

m5pq.plot.tree <- function (model, uniform = TRUE, main = "Classification Tree", use.n = FALSE, all = FALSE)
{
	fit_node <- model$rpart;
	fit_node$frame$yval <- as.numeric(rownames(fit_node$frame));

	plot(fit_node,uniform=uniform,main=main);
	text(fit_node, use.n=use.n, all=all, cex=.8);
}

m5pq.regressions <- function (model)
{
	retval <- list()
	for (i in sort(model$index)) retval[[paste("Reg-",i,sep="")]] <- model$regs[[i]]$coefficients[!is.na(model$regs[[i]]$coefficients)];
	retval;
}

m5pq.regression.coefficients <- function (model, index)
{
	model$regs[[index]]$coefficients[!is.na(model$regs[[index]]$coefficients)];
}

m5pq.regression.model <- function (model, index)
{
	model$regs[[index]]$model;
}
