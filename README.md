# shadia 
[![DOI](https://zenodo.org/badge/124828586.svg)](https://zenodo.org/doi/10.5281/zenodo.13375033)
Package containing functions and data for American shad population modeling (click [here](https://shadia-ui.github.io/index.html) for project website).

## Note
This is the development version of the `shadia` package for R (shadia v2.0). This package is under continual development at this time, as it is part of ongoing research and management. For that reason, it is important to correspond with package developers before using the dam passage performance standard models in decision making or research. 

Frozen (stable) versions of the software are provided for consistency during dam relisencing studies and reproducibility of research through the [`shadia` website](https://shadia-ui.github.io/index.html). The site now also provides up-to-date information about the status of all [models](https://shadia-ui.github.io/models.html).

Please check regularly for updates, changes, and bug patches. Please submit issues directly to this repository using the links above.

</br>
 
## Installation
The development version of `shadia` can be installed with  [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) in R using the repository url:

`devtools::install_github("danStich/shadia")`

To install `shadia`, you will need to have `devtools` installed ahead of time in R, but that requires some special tools. To install on **Windows**, you will need to download and install the appropriate version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/). To install on **Mac**, you will need to have the [XCode command-line tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/) installed. And, if running from **Linux**, you will need to install the developer version of R (`r-base-dev`) if you have not already.

</br>
 
## Use

The purpose of this package is to distribute code used to run dam passage performance standard models for anadromous alosines (shad and herring). Currently, the model is implemented for the **Connecticut**, **Kennebec**, **Merrimack**, **Mohawk and Hudson**, **Penobscot**, **Saco**, and **Susquehanna** rivers, USA, but we are actively adding new rivers. 

The main package functions can be run without any arguments to estimate population abundance in various reaches or in whole rivers under 'no dam' passage scenarios (see [examples](https://shadia-ui.github.io/examples.html)). Alternatively, the user can pass one or more values for upstream and downstream fish passage at a given dam which can then be applied throughout the watershed, or separately at each dam. Outputs include population abundance of spawners in the watersheds, within specific production units of each river, and the proportion of repeat spawners in each age class.

The models take several (10-30) seconds to run for one iteration on most standard workstations.

</br>
 
## Warning 

Management decisions should not be based on a single model run. The models rely on stochastic inputs for parameterization, as detailed in [Stich et al. (2019)](http://www.nrcresearchpress.com/doi/10.1139/cjfas-2018-0008#.W2SVohRKgeI). As such, any two model runs might result in substantially different predictions, even under the same passage scenario. We recommend at least 100 model runs per scenario to provide a minimal characterization of stochasticity, and a cursory understanding of variability in the response(s) of interest. In these cases, we strongly recommend running the model using the `snowfall` package as demonstrated in the help file for each model, which can be accessed by typing `?...RiverModel` (where '`...`' is the name of each river in lowercase) in the console and pressing `< Enter >`.

</br>

## Directories

`data/` Contains built-in data sets for the package

`man/`  help files and documentation

`R/`    R functions in scripts

`src/`  C++ source files written with `Rcpp`
