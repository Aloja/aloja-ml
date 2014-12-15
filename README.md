Aloja-ML
========

This repo contains the scripts and tests done for the Machine Learning part of ALOJA.

### Files

* **aloja.r** Contains the main scripting R snippets for running experiments. A scrapboard for experimentation.
* **aloja\_calls.r** Contains the main scripting R snippets, in a more "procedure call" formatting. Used for testing functions before including them in aloja_cli.r.
* **aloja_cli.r** A wrapper to call functions and run experiments from command line.
* **functions.r** The library with all the ALOJA functions, like learning methods and data-set treatments, called from aloja.r, aloja\_calls.r, aloja\_cli.r
* **aloja-dataset.csv** Contains the example dataset
* **Directories** Contain the dataset partitions, recodifications and plots for each specific experiment or test

### Requirements

* Rcran
* Java
* build-essential
* R required packages: base, devtools, nnet, scales, session, stringr, reshape, RWeka, bitops, colorspace, digest, grid, httr, munshell, plyr, Rcpp, RCurl, rJava, RWekajars, tools (and their related dependencies)

### R Installation

* If using Ubuntu 12.04:

>_Shell code_
>
>apt-get install "openjdk-7-jre-lib" "openjdk-7-jre-headless" "openjdk-7-jdk" "r-base" "r-base-core" "r-base-dev" "r-base-html" "r-cran-bitops" "r-cran-boot" "r-cran-class" "r-cran-cluster" "r-cran-codetools" "r-cran-foreign" "r-cran-kernsmooth" "r-cran-lattice" "r-cran-mass" "r-cran-matrix" "r-cran-mgcv" "r-cran-nlme" "r-cran-nnet" "r-cran-rpart" "r-cran-spatial" "r-cran-survival" "r-recommended" "r-cran-colorspace" "r-cran-getopt" "r-cran-rcolorbrewer" "r-cran-rcpp" "libcurl4-openssl-dev" "libxml2-dev" "gsettings-desktop-schemas" -y --force-yes
>
>R CMD javareconf
>
>_R code_
>
>update.packages(ask = FALSE,repos="http://cran.es.r-project.org",dependencies = c('Suggests'),quiet=TRUE);
>
>install.packages(c("rjson", "evaluate", "labeling", "memoise", "munsell", "stringr", "rJava"), repos="http://cran.es.r-project.org", dependencies=TRUE,quiet=TRUE);
>
>install.packages(c("devtools", "DiscriMiner", "emoa", "httr", "jsonlite", "optparse", "pracma", "rgp", "rstudioapi", "session", "whisker", "RWeka", "RWekajars"), repos="http://cran.es.r-project.org", dependencies=TRUE,quiet=TRUE);

* If using Ubuntu 14.04:

>_Shell code_
>
>wget http://security.ubuntu.com/ubuntu/pool/main/t/tiff/libtiff4\_3.9.5-2ubuntu1.6\_amd64.deb; dpkg -i ./libtiff4\_3.9.5-2ubuntu1.6\_amd64.deb
>
>apt-get install "openjdk-7-jre-lib" "openjdk-7-jre-headless" "openjdk-7-jdk" "r-base" "r-base-core" "r-base-dev" "r-base-html" "r-cran-bitops" "r-cran-boot" "r-cran-class" "r-cran-cluster" "r-cran-codetools" "r-cran-foreign" "r-cran-kernsmooth" "r-cran-lattice" "r-cran-mass" "r-cran-matrix" "r-cran-mgcv" "r-cran-nlme" "r-cran-nnet" "r-cran-rpart" "r-cran-spatial" "r-cran-survival" "r-recommended" "r-cran-rjson" "r-cran-rcurl" "r-cran-colorspace" "r-cran-dichromat" "r-cran-digest" "r-cran-evaluate" "r-cran-getopt" "r-cran-labeling" "r-cran-memoise" "r-cran-munsell" "r-cran-plyr" "r-cran-rcolorbrewer" "r-cran-rcpp" "r-cran-reshape" "r-cran-rjava" "r-cran-scales" "r-cran-stringr" "gsettings-desktop-schemas" -y --force-yes
>
>R CMD javareconf
>
>_R code_
>
>install.packages(c("devtools", "DiscriMiner", "emoa", "httr", "jsonlite", "optparse", "pracma", "rgp", "rstudioapi", "session", "whisker", "RWeka", "RWekajars"), repos="http://cran.es.r-project.org", dependencies=TRUE,quiet=TRUE);

### CLI Functionalities

#### Basic Syntax:
>./aloja\_cli.r -m method [-d dataset] [-l learned model] [-p param1=aaaa:param2=bbbb:param3=cccc:...] [-a] [-n dims] [-v]
>
>./aloja\_cli.r --method method [--dataset dataset] [--learned learned model] [--params param1=aaaa:param2=bbbb:param3=cccc:...] [--allvars] [--numvars dims] [--verbose]

#### Examples of Training and Prediction:
>./aloja\_cli.r -m aloja\_regtree -d aloja-dataset.csv -p saveall=m5p1
>
>./aloja\_cli.r -m aloja\_predict\_instance -l m5p1 -p inst\_predict="sort,ETH,RR3,8,10,1,65536,None,32,Azure L" -v
>
>./aloja\_cli.r -m aloja\_predict\_dataset -l m5p1 -d m5p1-tt.csv -v

#### Examples of Dimensionality Reduction:
>./aloja\_cli.r -m aloja\_pca -d dataset.csv -p saveall=pca1
>
>./aloja\_cli.r -m aloja\_regtree -d pca1-transformed.csv -p prange=1e-4,1e+4:saveall=m5p-simple-redim -n 20
>
>./aloja\_cli.r -m aloja\_predict\_instance -l m5p-simple-redim -p inst\_predict="1922.904354752,70.1570440421649,2.9694955079494,-3.64259027685954,-0.748746678239734,0.161321484374316,0.617610510007444,-0.459044093400257,0.251211132013151,0.251937462205716,-0.142007748147355,-0.0324862729758309,0.406308900544488,0.13593705166432,0.397452596451088,-0.731635384355167,-0.318297127484775,-0.0876192175148721,-0.0504762335523307,-0.0146283091875174" -v
>
>./aloja\_cli.r -m aloja\_predict\_dataset -l m5p-simple-redim -d m5p-simple-redim-tt.csv -v
>
>./aloja\_cli.r -m aloja\_transform\_data -d newdataset.csv -p pca\_name=pca1:saveall=newdataset
>
>./aloja\_cli.r -m aloja\_transform\_instance -p pca\_name=pca1:inst\_transform="sort,ETH,RR3,8,10,1,65536,None,32,Azure L" -v

#### Examples of Benchmark Selection:
>./aloja\_cli.r -m aloja\_dataset\_collapse -d dataset.csv -p dimension1="Benchmark":dimension2="Net,Disk,Maps,IO.SFac,Rep,IO.FBuf,Comp,Blk.size, \ Cluster":dimname1="Benchmark":dimname2="Configuration":saveall=dsc1
>
>./aloja\_cli.r -m aloja\_best\_configurations -p bvec\_name=dsc1 -v

