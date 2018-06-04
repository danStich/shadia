# shadia
Package containing functions and data for American shad population modeling. 

</br>
 
## Installation
This package can be installed with the [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) package in R using the repository url:

`devtools::install_github("danStich/shadia")`

To install `shadia`, you will need to have `devtools` installed ahead of time in R, but that requires some special tools. To install on **Windows**, you will need to download and install the appropriate version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/). To install on **Mac**, you will need to have the [XCode command-line tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/) installed. And, if running from **Linux**, you will need to install the developer version of R (`r-base-dev`) if you have not already.

</br>
 
## Use

The purpose of this package is to distribute code used to run the American shad dam passage performance standard model for the Penobscot River, Maine, USA. The main package function, `penobscotRiverModel()` can be run without any arguments to estimate population abundance in various reaches of the Penobscot River under a 'no dam' passage scenario. Alternatively, the user can pass one or more values for fish passage at a given dam which can then be applied throughout the watershed, or separately at each dam. Outputs include population abundance of spawners in the watershed, within specific production units of the Penobscot River, and the proportion of repeat spawners in each age class.

The model takes several (10-20) seconds to run once on most standard workstations.

</br>
 
## Warning 
Management decisions should not be based on a single model run. The model relies on stochastic inputs for parameterization, as detailed in Stich et al. (*In review*). As such, any two model runs might result in substantially different predictions, even under the same passage scenario. We recommend at least 100 model runs per scenario to provide a minimal characterization of stochasticity, and a cursory understanding of variability in the response(s) of interest. In these cases, we strongly recommend running the model using the `snowfall` package as demonstrated in the help file for `penobscotRiverModel`, which can be accessed by typing `?penobscotRiverModel` in the console and pressing `< Enter >`.

## Directories

`data/`

* Contains built-in data sets for the package

`man/`

* help files and documentation

`R/`

* R functions in scripts

`src/`

* C++ source files written with `Rcpp`
