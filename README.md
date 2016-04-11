Aloja-ML
========

This repo contains the scripts and tests done for the Machine Learning part of ALOJA.

### Files

* **aloja_cli.r** The wrapper/interface to call functions and run experiments from command line.
* **functions.r** The library with all the ALOJA functions, like learning methods and data-set treatments, called from the diverse scripts and interfaces.
* **functions-azml.r** The library with all the ALOJA functions, adapted for be called from Microsoft AzureML (OLD)
* **models.r** Some implemented models used by _functions.r_.
* **precision.r** Functions for computing precision of executions, also compare providers by similar executions, used by _functions.r_.
* **relations.r** Functions for computing simple relations between features of executions, used by _functions.r_.
* **searchtrees.r** Functions for representing execution datasets as a tree, used by _functions.r_.
* **deprecated.r** Functions in disuse or deprecated from _functions.r_, kept for legacy.
* **aloja-dataset.csv** Example Datasets (mush check headers before using with _aloja\_cli.r_.

### Requirements

* R and required packages: base, arules, e1071, kknn, nnet, optparse, RSNNS, rms, rpart, session, snowfall, stringr ...and dependencies.
* build-essential

### R Installation

* Example for Ubuntu 14.04:

>_Shell code_
>
>wget http://security.ubuntu.com/ubuntu/pool/main/t/tiff/libtiff4\_3.9.5-2ubuntu1.6\_amd64.deb; dpkg -i ./libtiff4\_3.9.5-2ubuntu1.6\_amd64.deb
>
>apt-get install "r-base" "r-base-core" "r-base-dev" "r-base-html" "r-cran-bitops" "r-cran-boot" "r-cran-class" "r-cran-cluster" "r-doc-html" "r-cran-codetools" "r-cran-foreign" "r-cran-kernsmooth" "r-cran-lattice" "r-cran-mass" "r-cran-matrix" "r-cran-mgcv" "r-cran-nlme" "r-cran-nnet" "r-cran-rpart" "r-cran-spatial" "r-cran-survival" "r-recommended" "r-cran-colorspace" "r-cran-dichromat" "r-cran-digest" "r-cran-foreach" "r-cran-gtable" "r-cran-ggplot2" "r-cran-iterators" "r-cran-labeling" "r-cran-munsell" "r-cran-plyr" "r-cran-rcolorbrewer" "r-cran-rcpp" "r-cran-reshape" "r-cran-scales" "r-cran-stringr" "gsettings-desktop-schemas" -y --force-yes
>
>_R code_
>
>install.packages(c("arules","e1071","kknn","optparse","RSNNS","rms","session","snowfall"), repos="http://cran.es.r-project.org", dependencies=TRUE,quiet=TRUE);

### CLI Functionalities

#### Basic Syntax:
>./aloja\_cli.r -m method [-d dataset] [-l learned model] [-p param1=aaaa:param2=bbbb:param3=cccc:...] [-a] [-n dims] [-v]
>
>./aloja\_cli.r --method method [--dataset dataset] [--learned learned model] [--params param1=aaaa:param2=bbbb:param3=cccc:...] [--allvars] [--numvars dims] [--verbose]

#### Examples of Training and Prediction:
>./aloja\_cli.r -m aloja\_regtree -d aloja-dataset.csv -p saveall=m5p1
>
>./aloja\_cli.r -m aloja\_regtree -d aloja-dataset.csv -p saveall=m5p1:vin="Benchmark,Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size":vout="Exe.Time"
>
>./aloja\_cli.r -m aloja\_predict\_dataset -l m5p1 -d m5p1-tt.csv -v
>
>./aloja\_cli.r -m aloja\_predict\_instance -l m5p1 -p inst\_predict="sort,ETH,RR3,8,10,1,65536,None,32,Azure L" -v
>
>./aloja\_cli.r -m aloja\_predict\_instance -l m5p1 -p inst\_predict="sort,ETH,RR3,8|10,10,1,65536,*,32,Azure L":sorted=asc -v
>
>./aloja\_cli.r -m aloja\_predict\_instance -l m5p1 -p inst\_predict="sort,ETH,RR3,8|10,10,1,65536,*,32,Azure L":vin="Benchmark,Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp, \ Blk.size,Cluster":sorted=asc -v

#### Examples of Detecting Outliers in the Dataset:
>./aloja\_cli.r -m aloja\_outlier\_dataset -d m5p1-tt.csv -l m5p1 -p sigma=3:hdistance=3:saveall=m5p1test
>
>./aloja\_cli.r -m aloja\_outlier\_instance -l m5p1 -p instance="sort,ETH,RR3,8,10,1,65536,None,32,Azure L":observed=100000:display=1 -v

#### Examples of Minimal Instances defining the Dataset:
>./aloja\_cli.r -m aloja\_minimal\_instances -l m5p1 -p saveall=m5p1mi
>
>./aloja\_cli.r -m aloja\_minimal\_instances -l m5p1 -p kmax=200:step=10:saveall=m5p1mi

#### Examples of JSON Tree defining the Dataset:
>./aloja\_cli.r -m aloja\_representative\_tree -p method=ordered:pred\_file=instances.csv:output=string -v


### Deprecated Functionalities

#### Examples of Dimensionality Reduction:
>./aloja\_cli.r -m aloja\_pca -d dataset.csv -p saveall=pca1
>
>./aloja\_cli.r -m aloja\_regtree -d pca1-transformed.csv -p prange=1e-4,1e+4:saveall=m5p-simple-redim -n 20
>
>./aloja\_cli.r -m aloja\_predict\_instance -l m5p-simple-redim -p inst\_predict="1922.904354752,70.1570440421649,2.9694955079494,-3.64259027685954, \ -0.748746678239734,0.161321484374316,0.617610510007444,-0.459044093400257,0.251211132013151,0.251937462205716,-0.142007748147355,-0.0324862729758309, \ 0.406308900544488,0.13593705166432,0.397452596451088,-0.731635384355167,-0.318297127484775,-0.0876192175148721,-0.0504762335523307,-0.0146283091875174" -v
>
>./aloja\_cli.r -m aloja\_predict\_dataset -l m5p-simple-redim -d m5p-simple-redim-tt.csv -v
>
>./aloja\_cli.r -m aloja\_transform\_data -d newdataset.csv -p pca\_name=pca1:saveall=newdataset
>
>./aloja\_cli.r -m aloja\_transform\_instance -p pca\_name=pca1:inst\_transform="sort,ETH,RR3,8,10,1,65536,None,32,Azure L" -v

#### Examples of Dataset Collapse (+Complete with prediction):
>./aloja\_cli.r -m aloja\_dataset\_collapse -d dataset.csv -p dimension1="Benchmark":dimension2="Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size, \ Cluster":dimname1="Benchmark":dimname2="Configuration":saveall=dsc1
>
>./aloja\_cli.r -m aloja\_dataset\_collapse -d dataset.csv -p dimension1="Benchmark":dimension2="Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size, \ Cluster":dimname1="Benchmark":dimname2="Configuration":saveall=dsc1:model\_name=m5p1
>
>./aloja\_cli.r -m aloja\_dataset\_collapse\_expand -d aloja-dataset.csv -p dimension1="Benchmark":dimension2="Net,Disk,Maps,IO.SFac,Rep,IO.FBuf, \ Comp,Blk.size,Cluster":dimname1="Benchmark":dimname2="Configuration":saveall=dsc1:model\_name=m5p1:inst\_general="sort,ETH,RR3,8|10,10,1,65536,*,32,Azure L"
>
>./aloja\_cli.r -m aloja\_best\_configurations -p bvec\_name=dsc1 -v




