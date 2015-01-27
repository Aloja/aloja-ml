source("src/functions-azml.r");

dataset <-  as.data.frame(unclass(maml.mapInputPort(1)));
colnames(dataset) <- c("X","ID","Benchmark","Exe.Time","Exec.Conf","Running.Cost..","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster","Histogram","PARAVER","End.time");

varout <- "Exe.Time";
varin <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");
dataset <- dataset[,c("ID",varout,varin)];

params <- list();
params[["ds"]] = dataset;
params[["vin"]] = varin;
params[["vout"]] = varout;
#params[["hlayers"]] = 5;
#params[["decay"]] = 5e-4;
#params[["maxit"]] = 1000;
#params[["prange"]] = NULL;
#model <- do.call(aloja_nnet,params);

params[["exsel"]] = 8000;
model <- do.call(aloja_regtree,params);

#retval1 <- cbind(model$predtest*model$maxout[varout,1]+model$minout[varout,1],model$testset[,varout]);
#retval2 <- cbind(model$predval*model$maxout[varout,1]+model$minout[varout,1],model$validset[,varout]);
#retval3 <- cbind(model$predtrain*model$maxout[varout,1]+model$minout[varout,1],model$trainset[,varout]);
retval1 <- cbind(model$predtest,model$testset[,varout]);
retval2 <- cbind(model$predval,model$validset[,varout]);
retval3 <- cbind(model$predtrain,model$trainset[,varout]);
retval <- rbind(retval1,retval2);
retval <- as.data.frame(rbind(retval,retval3));

#write.table(dataset, file = paste("test-ds.csv",sep=""), sep = ",", row.names=FALSE);

maml.mapOutputPort("retval");
