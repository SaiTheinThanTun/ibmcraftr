README.Rmd
================

Tools for Individual-Based Models in Infectious Disease
=======================================================

I have been developing an individual-based model to derive the cost-effective strategies to target malaria hotspots and eliminate malaria in Myanmar. In order to explore as many model structures as possible, I'm developing this tools which are generic enough to be used in any individual-based model for any infectious disease. At this moment, the package has 2 generic functions.

1. Create a synthetic population having several states.
-------------------------------------------------------

This function populates a matrix in which columns represent the states of the individuals and rows represent the individuals. Making it a generic function will let you explore as many disease state as you want. This is expecially useful when you're comparing your IBM with your ODE model.

``` r
library(ibmcraftr)
syn_pop(c(3,2,1)) # will populate 3 individuals in state 1, 2 in state 2 and 1 in state 3.
#>      [,1] [,2] [,3]
#> [1,]    1    0    0
#> [2,]    1    0    0
#> [3,]    1    0    0
#> [4,]    0    1    0
#> [5,]    0    1    0
#> [6,]    0    0    1
```

2. Make state transitions.
--------------------------

Using the state matrix of a population created previously, calculate the transitions from one state to other state(s) using the transition rate(s).

``` r
pop <- syn_pop(c(19,1,0,0))
state_trans(1,2,.1,pop) #state transition from 1 to 2, at rate .1
#>       [,1] [,2] [,3] [,4]
#>  [1,]    0    0    0    0
#>  [2,]    0    0    0    0
#>  [3,]    0    0    0    0
#>  [4,]    0    0    0    0
#>  [5,]    0    0    0    0
#>  [6,]    0    0    0    0
#>  [7,]    0    0    0    0
#>  [8,]    0    0    0    0
#>  [9,]   -1    1    0    0
#> [10,]    0    0    0    0
#> [11,]    0    0    0    0
#> [12,]    0    0    0    0
#> [13,]    0    0    0    0
#> [14,]    0    0    0    0
#> [15,]    0    0    0    0
#> [16,]    0    0    0    0
#> [17,]    0    0    0    0
#> [18,]    0    0    0    0
#> [19,]    0    0    0    0
#> [20,]    0    0    0    0
state_trans(1,4,100,pop) #state transition from 1 to 4, at rate 100
#>       [,1] [,2] [,3] [,4]
#>  [1,]   -1    0    0    1
#>  [2,]   -1    0    0    1
#>  [3,]   -1    0    0    1
#>  [4,]   -1    0    0    1
#>  [5,]   -1    0    0    1
#>  [6,]   -1    0    0    1
#>  [7,]   -1    0    0    1
#>  [8,]   -1    0    0    1
#>  [9,]   -1    0    0    1
#> [10,]   -1    0    0    1
#> [11,]   -1    0    0    1
#> [12,]   -1    0    0    1
#> [13,]   -1    0    0    1
#> [14,]   -1    0    0    1
#> [15,]   -1    0    0    1
#> [16,]   -1    0    0    1
#> [17,]   -1    0    0    1
#> [18,]   -1    0    0    1
#> [19,]   -1    0    0    1
#> [20,]    0    0    0    0
```
