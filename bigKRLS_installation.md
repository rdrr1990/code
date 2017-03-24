# "big" Install

bigKRLS is an R algorithm for Kernel-Regularized Least Squares that uses big data packages for size and C++ for speed. This architecture takes a bit of work to set up because R isn't necessarily properly set to connect C++ to C and FORTRAN (i.e., R throws lengthy error messages about lquadmath, lqfortran, clang++, and/or g++). Once initial installation is complete, everything will be connected under the hood. 

## Supported Operating Systems
bigKRLS has been run on Mac OS X Yosemite 10.10.5, Linux Ubuntu 14.04, and Windows 7 and Windows 8.

Windows users should use R but not RStudio (unfortunately, for now...).  


## Pre-Requisites

bigKRLS is designed to run on R version 3.3.0 ("Supposedly Educational" released 2016-05-03) or newer. Older, even fairly recent, versions of R will not work with bigmemory. 

-- Install the newest R at https://cran.r-project.org 

### Windows users must install up-to-date Rtools (3.3 or newer):

[https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/)

### Mac OSX 
OSX users may still need gfortran, which can be installed with the following two terminal commands :

```
curl -O http://r.research.att.com/libs/gfortran-4.8.2-darwin13.tar.bz2  
sudo tar fvxz gfortran-4.8.2-darwin13.tar.bz2 -C /
```

If troubles persist, we found the following pages particularly helpful: [A](http://thecoatlessprofessor.com/programming/setting-up-rstudio-to-work-with-rcpparmadillo/), [B](http://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks-lgfortran-and-lquadmath-error/), and section 2.16 of: [C](http://dirk.eddelbuettel.com/code/rcpp/Rcpp-FAQ.pdf).


## The Environment

```{r, eval = F}
install.packages("devtools")  
library(devtools)  
install.packages(c("Rcpp", "RcppArmadillo", "bigmemory", "biganalytics"))  
```


## Install via GitHub
You should now be able to install bigKRLS. Windows users should first run these extra lines:
```{r, eval = F}
find_rtools()
find_rtools(T)  
```
Finally, install the most current version with standard devtools syntax:

```{r, eval = F}
install_github('rdrr1990/bigKRLS')
library(bigKRLS)
vignette("bigKRLS_basics")
```
You should be good to go!


## Memory Limits
Despite improvements, the algorithm is still incredibly memory intensive. We recommend proceeding cautiously and bearing in mind that memory usage is a quadratic function of the number of observations, N (roughly 5N^2^, possibly worse for the eigendecomposition). Users should estimate models N = 1,000, 2,500, and 5,000 to see how their system performs before considering larger models. See https://sites.google.com/site/petemohanty/software for detail.

## License 
Code released under GPL (>= 2).


