
# rpcm.tree

This package provides a function to detect differential item functioning
in a rasch poisson count model.

**Warning**: This package is currently a work in progress and might not
perform as expected. Due to numerical limitations, only the glmer\_fit
function can be used to compute the rpcm\_tree.

## Installation

You can install the released version of rpcm.tree with:

``` r
if (!require("devtools")) install.packages("devtools")
library("devtools")
devtools::install_github("boweber/rpcm-tree")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library("rpcm.tree")
```
