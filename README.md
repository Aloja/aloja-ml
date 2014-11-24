Aloja-ML
========

This repo contains the scripts and tests done for the Machine Learning part of ALOJA.

#### Files:

* **aloja.r** Contains the main scripting for R to process data, run experiments also plot stuff
* **aloja_calls.r** Contains the main scripting, in a more "procedure call" formatting
* **aloja_cli.r** A wrapper to call functions and run experiments from command line
* **functions.r** Library with all the ALOJA functions, like learning methods and data-set treatments, used by aloja.r
* **nnet_plot_update.r** Library obtained from https://gist.githubusercontent.com/fawda123 for nnet plots
* **aloja-dataset.csv** Contains the dataset
* **Directories** Contain the dataset partitions, recodifications and plots for each specific experiment or test

#### Requirements:

* Rcran
* Java
* build-essential
* R packages: bitops, colorspace, devtools, dichromat, digest, DiscriMiner, evaluate, httr, jsonlite, labeling, memoise, munsell, nnet, optparse, plyr, pracma, RColorBrewer, Rcpp, reshape, rJava, rstudioapi, RWeka, RWekajars, scales, session, stringr, whisker, RCurl
