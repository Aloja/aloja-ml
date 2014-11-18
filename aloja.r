
source("functions.r");
source("nnet_plot_update.r");
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

options(width=as.integer(Sys.getenv("COLUMNS")));

###############################################################################
# Read datasets and prepare them for usage                                    #
###############################################################################

	aux <- obtain_data (fread = "aloja-dataset.csv", cds = FALSE, hds = FALSE, fproc = "aloja-process");

	dataset <- aux$ds;
	dataset_sub <- aux$ds_sub;

	print_summaries (fprint="output.txt", ds=dataset, ds_sub=dataset_sub, fwidth = 1000, ms = 10);

###############################################################################
# Relation among output variables                                             #
###############################################################################

# TODO FIXME
plot(dataset_sub[,1],dataset_sub[,2]);
aux <- dataset_sub[strptime(dataset_sub[,"End.time"],format="%F")<=strptime("2014-03-11 00:00:00 CET",format="%F"),];
points(aux[,1],aux[,2],col="red");
#points(aux[,1]*0.6,aux[,2],col="green");

###############################################################################
# ANOVA of current variables                                                  #
###############################################################################

	anova_1 <- anova_ds (dataset);
	print (c("Means are equal: ",anova_1$f < anova_1$critical));

###############################################################################
# Learning from the variables                                                 #
###############################################################################

	vout <- "Exe.Time";
	vin <- c("Benchmark","Net","Disk","Maps","IO.SFac","Rep","IO.FBuf","Comp","Blk.size","Cluster");

###############################################################################
# Decision/Regression Tree

	#######################################################################
	## Training M5P without example selection

	#m5p1 <- regtrees(dataset,vin,vout);
	m5p1 <- regtrees(dataset,vin,vout,saveall=c("simple","m5p"),pngval="m5p-simple-app",pngtest="m5p-simple-test");

	#######################################################################
	## Training M5P with example selection

	#m5p2 <- regtrees(dataset,vin,vout,ttaux=m5p1$testset,exsel=8000);
	m5p2 <- regtrees(dataset,vin,vout,ttaux=m5p1$testset,exsel=8000,saveall=c("exselect","m5p"),pngval="m5p-exsel-app",pngtest="m5p-exsel-test");

	#######################################################################
	## Training M5P with benchmark separation

	m5px <- list();
	for (name in levels(dataset[,"Benchmark"]))
	{
		baux <- dataset[dataset[,"Benchmark"]==name,];
		taux <- m5p1$testset[m5p1$testset[,"Benchmark"]==name,];

		#m5px[[name]] <- regtrees(ds=baux,vin=vin,vout=vout,ttaux=taux);
		m5px[[name]] <- regtrees(ds=baux,vin=vin,vout=vout,ttaux=taux,saveall=c(paste("benchmark",name,sep="-"),"m5p"),pngval=paste("m5p-benchmark",name,"val",sep="-"),pngtest=paste("m5p-benchmark",name,"test",sep="-"));
	}
	rm (baux,taux,name);

############################################################
# k-Nearest Neighbor

	#ibk1 <- regnneighbors(dataset,vin,vout);
	ibk1 <- regnneighbors(dataset,vin,vout,saveall=c("simple","ibk"),pngval="ibk-simple-app",pngtest="ibk-simple-test");

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
tvauxbin <- bntaux[!(rownames(bntaux) %in% selected),];

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

#savemodel (trauxnorm, tvauxnorm, ttauxnorm, NULL, "32-5-1","nnet");

testing <- predict(nn1,newdata=ttauxnorm[,-c(1,8,26)]);
#png("nnet-32-5-1-test.png",width=1000,height=500);
	par(mfrow=c(1,2));
	plot(prediction,tvauxnorm[,1],main="NN 32-5-1, decay 5e-4, maxit 1000");
	abline(0,1);
	plot(testing,ttauxnorm[,1],main="Test NN 32-5-1, decay 5e-4, maxit 1000");
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


