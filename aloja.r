
library(stringr);
library(RWeka);
library(devtools);
library(scales);
library(reshape);
library(nnet);
set.seed(1234567890);

source("functions.r");
source("nnet_plot_update.r");
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

options(width=as.integer(Sys.getenv("COLUMNS")));

fileread <- "aloja-dataset.csv";
fileproc <- "aloja-process.csv";
fileout <- "output.txt";
histdataset <- FALSE;
confdataset <- TRUE;

###############################################################################
# Read datasets and prepare them for usage                                    #
###############################################################################

dataset <- read.table(fileread,header=T,sep=",");

aux <- strptime(dataset[,"End.time"],format="%Y%m%d%H%M%S");
dataset[,"End.time"] <- NULL;
names <- colnames(dataset);
dataset <- cbind(dataset,aux);
colnames(dataset) <- c(names,"End.time");
rm(aux,names);

if (confdataset)
{
	split <- str_split_fixed(dataset[,"Exec.Conf"], "/", 2);
	exec_conf <- str_split_fixed(split[,1], "_", 13);
	aux <- strptime(paste(exec_conf[,1],exec_conf[,2],sep=""),format="%Y%m%d%H%M%S");
	exec_conf <- exec_conf[,-c(1,2)];
	exec_conf <- cbind(matrix(as.character(aux)),exec_conf);
	colnames(exec_conf) <- paste("Conf.",c("Time","Conf","Net","Disk","B","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster"),sep="")
	bench_conf <- str_split_fixed(split[,2], "_", 2);
	colnames(bench_conf) <- c("Conf.Benchmark","Conf.Options");
	dataset <- cbind(dataset,exec_conf,bench_conf);
	rm(exec_conf,bench_conf,split);
	"Deglosed Exec.Conf";
}
rm(confdataset);

if (histdataset)
{
	split <- str_split_fixed(dataset[,"Histogram"], "/", 2);
	histogram <- str_split_fixed(split[,1], "_", 13);
	colnames(histogram) <- paste("Hist.",c("Date","Time","Conf","Net","Disk","B","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster"),sep="")
	bench_hist <- str_split_fixed(split[,2], "_", 2);
	colnames(bench_conf) <- c("Hist.Benchmark","Hist.Options");
	dataset <- cbind(dataset,histogram,bench_hist);
	rm(histogram,bench_hist,split);
	"Deglosed Histogram";
}
rm(histdataset);

dataset <- subset(dataset,select=-c(X,Exec.Conf,Histogram,PARAVER));
dataset_sub <- subset(dataset,select=c(Exe.Time,Running.Cost..,Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size,Cluster,End.time));

write.table(dataset,file=fileproc,sep=",",row.names=F);

###############################################################################
# Print summaries for each benchmark                                          #
###############################################################################

sink(file=fileout,append=FALSE);
options(width=1000);

############################################################
# Summary for General
cat("Summary for General Data","\n");
summary(subset(dataset,select=-c(ID)),maxsum=10);

############################################################
# Summary per Benchmark
for (name in levels(dataset[,"Benchmark"]))
{
	cat("\n","Summary per Benchmark",name,"\n");
	print(summary(dataset_sub[dataset[,"Benchmark"]==name,],maxsum=10));
}
rm(name);

sink(NULL);
options(width=as.integer(Sys.getenv("COLUMNS")));

###############################################################################
# Relation among output variables                                             #
###############################################################################

plot(dataset_sub[,1],dataset_sub[,2]);
aux <- dataset_sub[strptime(dataset_sub[,"End.time"],format="%F")<=strptime("2014-03-11 00:00:00 CET",format="%F"),];
points(aux[,1],aux[,2],col="red");
#points(aux[,1]*0.6,aux[,2],col="green");


###############################################################################
# ANOVA of current variables                                                  #
###############################################################################

anova_1 <- list();
anova_1$alpha <- 0.05;
anova_1$N <- NULL;
anova_1$K <- NULL;
anova_1$gmean <- NULL;
anova_1$ssb <- 0;
anova_1$ssw <- 0;
anova_1$mse <- NULL;
anova_1$f <- NULL;

bmks <- list();
anova_1$means <- list();
anova_1$stdevs <- list();

for (i in levels(dataset[,"Benchmark"]))
{
	bmks[[i]] <- dataset[dataset[,"Benchmark"]==i,c("Exe.Time","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster")];
	anova_1$means[[i]] <- mean(bmks[[i]][,"Exe.Time"]);
	anova_1$stdevs[[i]] <- sd(bmks[[i]][,"Exe.Time"]);
}
anova_1$gmean <- mean(rapply(anova_1$means,function(x) x));

for (i in levels(dataset[,"Benchmark"]))
{
	anova_1$ssb <- anova_1$ssb + (length(bmks[[i]][,"Exe.Time"]) * (anova_1$means[[i]] - anova_1$gmean)^2);
}

anova_1$N <- length(dataset[,1]);
anova_1$K <- length(levels(dataset[,"Benchmark"]));
for (i in 1:anova_1$N)
{
	anova_1$ssw <- anova_1$ssw + (dataset[i,"Exe.Time"] - anova_1$means[[dataset[i,"Benchmark"]]])^2;
}
anova_1$mse <- anova_1$ssw / (anova_1$N - anova_1$K);
anova_1$f <- (anova_1$ssb / (anova_1$K - 1)) / anova_1$mse;
anova_1$critical <- qf(1-anova_1$alpha, anova_1$K-1, anova_1$N-1);

print (c("Means are equal: ",anova_1$f < anova_1$critical));
rm(i,bmks);

###############################################################################
# Learning from the variables                                                 #
###############################################################################

vout <- "Exe.Time";
vin <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");

############################################################
# LinReg (just to discard)
aux <- dataset[,c(vout,vin)];
aux <- aux[,sapply(aux,is.numeric)];
ml0 <- lm(formula=aux[,vout] ~ ., data=aux[,names(aux)!=vout]);
mean(abs(ml0$fitted.values - dataset[,vout]));
plot(ml0$fitted.values,dataset[,vout])
abline(1,1)
rm(aux);

############################################################
# Decision/Regression Tree
aux <- dataset[,c(vout,vin)];

tsplit <- 0.25;
selected <- sample(1:length(aux[,1]),length(aux[,1])*tsplit);
ttaux <- aux[selected,];
ntaux <- aux[-selected,];

###################################################
## Training M5P without example selection
vsplit <- 0.66;
selected <- sample(1:length(ntaux[,1]),length(ntaux[,1])*vsplit);
traux <- ntaux[selected,];
tvaux <- ntaux[-selected,];

traux <- traux[traux[,vout] <= mean(traux[,vout]) + 3 * sd(traux[,vout]),];
tvaux <- tvaux[tvaux[,vout] <= mean(tvaux[,vout]) + 3 * sd(tvaux[,vout]),];

result <- bestm5p(vout, vin, traux, tvaux, c("1","2","5","10","25","50","75","100","150","200"));
prediction <- predict(result$ml,newdata=tvaux);
#png("m5p-simple-app.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvaux[,vout],main=paste("Best Validation M5P M = ",result$mmin));
	abline(1,1);
	plot(result$trmae,ylim=c(min(c(result$trmae,result$tvmae)),max(result$trmae,result$tvmae)),main="Error vs M");
	points(result$tvmae,col="red");
	legend("topleft",pch=1,c("trmae","tvmae"),col=c("black","red"));
#dev.off();

#savemodel (traux, tvaux, ttaux, result$ml, "simple","m5p");
#loadmodel("simple","m5p");
#rm(prediction,selected,aux,tsplit,vsplit);

testing <- predict(result$ml,newdata=ttaux);
#png("m5p-simple-test.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvaux[,vout],main=paste("Best Validation M5P M = ",result$mmin));
	abline(1,1);
	plot(testing,ttaux[,vout],main=paste("Test M5P M = ",result$mmin));
	abline(1,1);
#dev.off();
mean(abs(testing - ttaux[,vout]));
mean(abs((testing - ttaux[,vout])/ttaux[,vout]));

###################################################
## Training M5P with example selection
vsplit <- 0.66;
aux5000 <- ntaux[ntaux[,vout]>8000,];
aux0000 <- ntaux[ntaux[,vout]<=8000,];

selected5000 <- sample(1:length(aux5000[,1]),length(aux5000[,1])*vsplit);
selected0000 <- sample(1:length(aux0000[,1]),length(aux0000[,1])*vsplit);

trauxes <- rbind(aux5000[selected5000,],aux0000[selected0000,]);
tvauxes <- rbind(aux5000[-selected5000,],aux0000[-selected0000,]);

trauxes <- trauxes[trauxes[,vout] <= mean(trauxes[,vout]) + 3 * sd(trauxes[,vout]),];
tvauxes <- tvauxes[tvauxes[,vout] <= mean(tvauxes[,vout]) + 3 * sd(tvauxes[,vout]),];

result <- bestm5p(vout, vin, trauxes, tvauxes, c("1","2","5","10","25","50","75","100","150","200"));
prediction <- predict(result$ml,newdata=tvauxes);
#png("m5p-exselect-app.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvauxes[,vout],main=paste("Best Validation M5P M = ",result$mmin));
	abline(1,1);
	plot(result$trmae,ylim=c(min(c(result$trmae,result$tvmae)),max(result$trmae,result$tvmae)),main="Error vs M");
	points(result$tvmae,col="red");
	legend("topleft",pch=1,c("trmae","tvmae"),col=c("black","red"));
#dev.off();

#savemodel (trauxes, tvauxes, ttaux, result$ml, "exselect","m5p");
#loadmodel ("exselect","m5p");
#rm(prediction,selected0000,selected5000,aux5000,aux0000,vsplit);

testing <- predict(result$ml,newdata=ttaux);
#png("m5p-exselect-test.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvauxes[,vout],main=paste("Best Validation M5P M = ",result$mmin));
	abline(1,1);
	plot(testing,ttaux[,vout],main=paste("Test M5P M = ",result$mmin));
	abline(1,1);
#dev.off();
mean(abs(testing - ttaux[,vout]));
mean(abs((testing - ttaux[,vout])/ttaux[,vout]));

###################################################
## Training M5P with benchmark separation

for (name in levels(dataset[,"Benchmark"]))
{
	bmkaux <- ntaux[ntaux[,"Benchmark"]==name,];
	bmkaux <- bmkaux[bmkaux[,vout] <= mean(bmkaux[,vout]) + 3 * sd(bmkaux[,vout]),];

	vsplit <- 0.66;
	selected <- sample(1:length(bmkaux[,1]),length(bmkaux[,1])*vsplit);
	trauxbmk <- bmkaux[selected,];
	tvauxbmk <- bmkaux[-selected,];

	result <- bestm5p(vout, vin, trauxbmk, tvauxbmk, c("1","2","5","10","25","50","75","100","150","200"));
	prediction <- predict(result$ml,newdata=tvauxbmk);
	#png(paste("m5p-benchmark-",name,".png",sep=""),width=1000,height=500);
		par(mfrow=c(1,2));
		plot(prediction,tvauxbmk[,vout],main=paste("Best M5P M = ",result$mmin," MAE = ", min(result$tvmae), " ", name, sep=""));
		abline(1,1);
		plot(result$trmae,ylim=c(min(c(result$trmae,result$tvmae)),max(result$trmae,result$tvmae)),main="Error vs M");
		points(result$tvmae,col="red");
		legend("topleft",pch=1,c("trmae","tvmae"),col=c("black","red"));
	#dev.off();

	#savemodel (trauxes, tvauxes, ttaux, result$ml, paste("benchmark",name,sep=""),"m5p");
}
#rm(bmkaux,vsplit,selected,prediction);

############################################################
# k-Nearest Neighbor

vsplit <- 0.66;
selected <- sample(1:length(ntaux[,1]),length(ntaux[,1])*vsplit);
trauxibk <- ntaux[selected,];
tvauxibk <- ntaux[-selected,];

trauxibk <- trauxibk[trauxibk[,vout] <= mean(trauxibk[,vout]) + 3 * sd(trauxibk[,vout]),];
tvauxibk <- tvauxibk[tvauxibk[,vout] <= mean(tvauxibk[,vout]) + 3 * sd(tvauxibk[,vout]),];

ibk1 <- IBk(formula=trauxibk[,vout] ~ . , data = trauxibk[,vin], control = Weka_control(K = 3, I = TRUE));
evaluate_Weka_classifier(ibk1, numFolds = 10);

prediction <- predict(ibk1,newdata=tvauxibk);
#png(paste("ibk-simple-",3,"-","I",".png",sep=""),width=1000,height=500);
	plot(prediction,tvauxibk[,vout],main="K-NN K = 1 Weight = Inv.Dist.");
	abline(1,1);
#dev.off();

#savemodel (trauxibk, tvauxibk, ttaux, ibk1, "simple","ibk");

testing <- predict(ibk1,newdata=ttaux);
#png("ibk-simple-test.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvauxibk[,vout],main=paste("Best Validation k-NN K = ",1));
	abline(1,1);
	plot(testing,ttaux[,vout],main=paste("Test k-NN K = ",1));
	abline(1,1);
#dev.off();
mean(abs(testing - ttaux[,vout]));
mean(abs((testing - ttaux[,vout])/ttaux[,vout]));

############################################################
# Others (Regression)

## Binarization of categorical values
bntaux <- bindataset(ntaux);
bttaux <- bindataset(ttaux);
vbin <- colnames(bntaux[-1]);

###################################################
## LinReg Again (Binarized & Polynomial)
vsplit <- 0.66;
selected <- sample(1:length(bntaux[,1]),length(bntaux[,1])*vsplit);
trauxbin <- bntaux[selected,];
tvauxbin <- bntaux[!(rownames(tvauxbin) %in% selected)),];

#trauxbin <- trauxbin[trauxbin[,"dfsioe_read"]==0,];
#tvauxbin <- tvauxbin[tvauxbin[,"dfsioe_read"]==0,];
#bttaux <- bttaux[bttaux[,"dfsioe_read"]==0,];

#trauxbin <- read.table("linreg-polynom3-approach/linreg-trpolynom3.csv",header=T,sep=",")
#tvauxbin <- read.table("linreg-polynom3-approach/linreg-tvpolynom3.csv",header=T,sep=",")
#bttaux <- read.table("linreg-polynom3-approach/linreg-ttpolynom3.csv",header=T,sep=",")

trauxbin <- trauxbin[trauxbin[,1] <= mean(trauxbin[,1]) + 3 * sd(trauxbin[,1]),];
tvauxbin <- tvauxbin[tvauxbin[,1] <= mean(tvauxbin[,1]) + 3 * sd(tvauxbin[,1]),];

#linreg1 <- LinearRegression(formula=trauxbin[,vout] ~ . , data = trauxbin[,-1], control = Weka_control());
#evaluate_Weka_classifier(linreg1, numFolds = 10);
linreg1 <- lm(formula=trauxbin[,vout] ~ . + (.)^2 + (.)^3, data=trauxbin[,vbin]);
prediction <- predict(linreg1,newdata=tvauxbin);

#png("linreg-polynom-app.png",width=1000,height=500);
	plot(prediction,tvauxbin[,1],main="Polynomial Regression var + var^2 + var^3");
	abline(1,1);
#dev.off();
mean(abs(prediction - tvauxbin[,1]));	

#savemodel (trauxbin, tvauxbin, bttaux, linreg1, "polynom3","linreg");

testing <- predict(linreg1,newdata=bttaux);
#png("linreg-polynom3-test.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvauxbin[,1],main="Polynomial Regression var + var^2 + var^3");
	abline(1,1);
	plot(testing,bttaux[,1],main="Test Polynomial Regression var + var^2 + var^3");
	abline(1,1);
#dev.off();
mean(abs(testing - bttaux[,1]));
mean(abs((testing - bttaux[,1])/bttaux[,1]));

points(testing[rownames(bttaux) %in% rownames(bttaux[bttaux[,"dfsioe_read"]==1,])],bttaux[rownames(bttaux) %in% rownames(bttaux[bttaux[,"dfsioe_read"]==1,]),1],col="red");


###################################################
## Neural Networks

#mod1<-nnet(rand.vars,resp,data=dat.in,size=10,linout=T)

bttauxbkp <- bttaux;
bttaux <- bttauxbkp;
bttaux <- cbind(bttaux[,1:25],rep(0,length(bttaux[,1])),bttaux[,26:33]);

# Normalize values
trauxnorm <- NULL;
tvauxnorm <- NULL;
ttauxnorm <- NULL;
for (i in 1:length(trauxbin))
{
	trauxnorm <- cbind(trauxnorm, (trauxbin[,i]-min(c(trauxbin[,i],tvauxbin[,i])))/max(c(trauxbin[,i],tvauxbin[,i])));
	tvauxnorm <- cbind(tvauxnorm, (tvauxbin[,i]-min(c(trauxbin[,i],tvauxbin[,i])))/max(c(trauxbin[,i],tvauxbin[,i])));
	ttauxnorm <- cbind(ttauxnorm, (bttaux[,i]-min(c(trauxbin[,i],tvauxbin[,i])))/max(c(trauxbin[,i],tvauxbin[,i]))); # Same Norm (tr,tv) as not seen before
}

nn1<-nnet(y=trauxnorm[,1],x=trauxnorm[,-c(1,8,26)],size=5,decay=5e-4,maxit=1000);
c <- (nn1$fitted.values - min(nn1$fitted.values))
c <- c / max(c);
plot(c,trauxnorm[,1]);
abline(0,1);

prediction <- predict(nn1,newdata=tvauxnorm[,-c(1,8,26)]);
#png("nnet-31-5-1-app.png",width=1000,height=500);
	plot(prediction,tvauxnorm[,1],main="NN 31-5-1, decay 5e-4, maxit 1000");
	abline(0,1);
#dev.off();

#savemodel (trauxnorm, tvauxnorm, ttauxnorm, NULL, "31-5-1","nnet");

testing <- predict(nn1,newdata=ttauxnorm[,-c(1,8,26)]);
#png("nnet-31-5-1-test.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvauxnorm[,1],main="NN 31-5-1, decay 5e-4, maxit 1000");
	abline(0,1);
	plot(testing,ttauxnorm[,1],main="Test NN 31-5-1, decay 5e-4, maxit 1000");
	abline(0,1);
#dev.off();
mean(abs(testing - ttauxnorm[,1]));
mean(abs((testing - ttauxnorm[,1])/ttauxnorm[,1]));
plot.nnet(nn1);
plot.nnet(nn1$wts,nn1$n);
 

###############################################################################
# Clustering and dimensional techniques                                       #
###############################################################################

############################################################
# Principal Components Analysis

auxpca <- rbind(bntaux,bttaux);
pc <- princomp(auxpca);
pc$loadings;

#a <- (aux[,1]-min(auxpca[,1]))/max(auxpca[,1]-min(auxpca[,1]));
#caux <- ((a^(1/6))-min((a^(1/6))))/max((a^(1/6))-min((a^(1/6))));

#plot(1, type="n", xlim=c(1000,3000),ylim=c(-160,110), xlab="", ylab="");
#for(i in 1:length(auxpca[,1])) points(pc$scores[i,5],pc$scores[i,6],col=gray(caux[i]));

for (var1 in 1:(length(pc$scores[1,])-1))
{
	for (var2 in (var1 + 1):length(pc$scores[1,]))
	{
		#png(paste("pcatmp/pca-",var1,"-",var2,".png",sep=""),width=1000,height=500);
			plot(1, type="n", xlim=c(min(pc$scores[,var1]),max(pc$scores[,var1])),ylim=c(min(pc$scores[,var2]),max(pc$scores[,var2])), xlab="", ylab="");
			points(jitter(pc$scores[auxpca[,1]>3000,var1],factor=0.6),jitter(pc$scores[auxpca[,1]>3000,var2],factor=0.6),col="red");
			points(jitter(pc$scores[auxpca[,1]>2000 & auxpca[,1]<3000,var1],factor=0.6),jitter(pc$scores[auxpca[,1]>2000 & auxpca[,1]<3000,var2],factor=0.6),col="blue");
			points(jitter(pc$scores[auxpca[,1]>1000 & auxpca[,1]<2000,var1],factor=0.6),jitter(pc$scores[auxpca[,1]>1000 & auxpca[,1]<2000,var2],factor=0.6),col="green");
			points(jitter(pc$scores[auxpca[,1]<1000,var1],factor=0.6),jitter(pc$scores[auxpca[,1]<1000,var2],factor=0.6),col="black");
		#dev.off();
	}
}

paux <- pc$scores;
pout <- auxpca[,1];

###################################################
## LinReg (with reduced dimension)

tsplit <- 0.25;
selected <- sample(1:length(paux[,1]),length(paux[,1])*tsplit);
ttpaux <- cbind(pout,paux)[selected,1:21];
ntpaux <- cbind(pout,paux)[-selected,1:21];

vsplit <- 0.66;
selected <- sample(1:length(ntpaux[,1]),length(ntpaux[,1])*vsplit);
trpaux <- ntpaux[selected,];
tvpaux <- ntpaux[-selected,];

trpaux <- trpaux[trpaux[,1] <= mean(trpaux[,1]) + 3 * sd(trpaux[,1]),];
tvpaux <- tvpaux[tvpaux[,1] <= mean(tvpaux[,1]) + 3 * sd(tvpaux[,1]),];

linreg2 <- lm(formula=trpaux[,1] ~ . + (.)^2 + (.)^3, data=data.frame(trpaux[,2:21]));
prediction <- predict(linreg2,newdata=data.frame(tvpaux[,2:21]));

#png("linreg-polynom-reduction.png",width=1000,height=500);
	plot(prediction,tvpaux[,1],main="Polynomial Regression var + var^2 + var^3 (RED-DIM)",xlim=c(0,10000));
	abline(0,1);
#dev.off();
mean(abs(prediction[prediction<10000 & prediction>0]-tvpaux[prediction<10000 & prediction>0,1]));

#savemodel (trpaux, tvpaux, ttpaux, NULL, "polynom3-reduction","linreg");

testing <- predict(linreg2,newdata=data.frame(ttpaux[,2:21]));
#png("linreg-polynom3-reduction.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvpaux[,1],main="Polynomial Regression var + var^2 + var^3",xlim=c(0,10000));
	abline(0,1);
	plot(testing,ttpaux[,1],main="Test Polynomial Regression var + var^2 + var^3",xlim=c(0,10000));
	abline(0,1);
#dev.off();
mean(abs(testing[testing<10000 & testing>0]-ttpaux[testing<10000 & testing>0,1]));
mean(abs((testing[testing<10000 & testing>0]-ttpaux[testing<10000 & testing>0,1])/ttpaux[testing<10000 & testing>0,1]));
points(testing[rownames(ttpaux) %in% rownames(auxpca[auxpca[,"dfsioe_read"]==1,])],ttpaux[rownames(ttpaux) %in% rownames(auxpca[auxpca[,"dfsioe_read"]==1,]),1],col="red");

###################################################
## Training M5P (with reduced dimension)

result <- bestm5p(1, 2:21, data.frame(trpaux), data.frame(tvpaux), c("1","2","5","10","25","50","75","100","150","200"));
prediction <- predict(result$ml,newdata=data.frame(tvpaux));
#png("m5p-simple-app.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvpaux[,1],main=paste("Best Validation M5P (Red.Dim.) M = ",result$mmin));
	abline(0,1);
	plot(result$trmae,ylim=c(min(c(result$trmae,result$tvmae)),max(result$trmae,result$tvmae)),main="Error vs M");
	points(result$tvmae,col="red");
	legend("topleft",pch=1,c("trmae","tvmae"),col=c("black","red"));
#dev.off();
mean(abs(prediction[prediction<10000 & prediction>0]-tvpaux[prediction<10000 & prediction>0,1]));

#savemodel (traux, tvaux, ttaux, result$ml, "reduction","m5p");

testing <- predict(result$ml,newdata=data.frame(ttpaux[,2:21]));
#png("m5p-reduction-test.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvpaux[,1],main=paste("Best Validation M5P (Red.Dim.) M = ",result$mmin));
	abline(0,1);
	plot(testing,ttpaux[,1],main=paste("Test M5P (Red.Dim.) M = ",result$mmin));
	abline(0,1);
#dev.off();
mean(abs(testing[testing<10000 & testing>0]-ttpaux[testing<10000 & testing>0,1]));
mean(abs((testing[testing<10000 & testing>0]-ttpaux[testing<10000 & testing>0,1])/ttpaux[testing<10000 & testing>0,1]));
points(testing[rownames(ttpaux) %in% rownames(auxpca[auxpca[,"dfsioe_read"]==1,])],ttpaux[rownames(ttpaux) %in% rownames(auxpca[auxpca[,"dfsioe_read"]==1,]),1],col="red");

############################################################
# K-Means as clustering

auxkmn <- rbind(bntaux,bttaux);
kc <- kmeans(auxkmn[-1],3);
#plot(kc$cluster,auxkmn[,5]);


