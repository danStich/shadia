# shadia
Package containing functions and data for American shad population modeling. 

## Note
This package is under continual development at this time, as it is part of ongoing research. Therefore, there is not currently a *stable* version of the package. Please check regularly for updates, changes, and bug patches. Please submit issues directly to this repository using the links above.

</br>
 
## Installation
This package can be installed with the [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) package in R using the repository url:

`devtools::install_github("danStich/shadia")`

To install `shadia`, you will need to have `devtools` installed ahead of time in R, but that requires some special tools. To install on **Windows**, you will need to download and install the appropriate version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/). To install on **Mac**, you will need to have the [XCode command-line tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/) installed. And, if running from **Linux**, you will need to install the developer version of R (`r-base-dev`) if you have not already.

</br>
 
## Use

The purpose of this package is to distribute code used to run the American shad dam passage performance standard model. Currently, the model is implemented for the Connecticut, Merrimack, Penobscot, Saco, and Susquehanna rivers, USA, but we are actively adding new rivers. The main package functions, `connecticutRiverModel()`, `merrimackRiverModel()`, `penobscotRiverModel()`, `sacoRiverModel`, and `susquehannaRiverModel()` can be run without any arguments to estimate population abundance in various reaches or in whole rivers under 'no dam' passage scenarios. Alternatively, the user can pass one or more values for upstream and downstream fish passage at a given dam which can then be applied throughout the watershed, or separately at each dam. Outputs include population abundance of spawners in the watersheds, within specific production units of each river, and the proportion of repeat spawners in each age class.

The models take several (10-30) seconds to run for one iteration on most standard workstations.

</br>
 
## Warning 

Management decisions should not be based on a single model run. The models rely on stochastic inputs for parameterization, as detailed in [Stich et al. (2018)](http://www.nrcresearchpress.com/doi/10.1139/cjfas-2018-0008#.W2SVohRKgeI). As such, any two model runs might result in substantially different predictions, even under the same passage scenario. We recommend at least 100 model runs per scenario to provide a minimal characterization of stochasticity, and a cursory understanding of variability in the response(s) of interest. In these cases, we strongly recommend running the model using the `snowfall` package as demonstrated in the help file for each model, which can be accessed by typing `?...RiverModel` (where '...' is the name of each river in lowercase) in the console and pressing `< Enter >`.

</br>

## Directories

`data/` Contains built-in data sets for the package

`man/`  help files and documentation

`R/`    R functions in scripts

`src/`  C++ source files written with `Rcpp`
