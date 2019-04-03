<!-- README.md is generated from README.Rmd. Please edit that file -->

latte
=====

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/latte)](https://cran.r-project.org/package=latte)
[![Travis build
status](https://travis-ci.org/dkahle/latte.svg?branch=master)](https://travis-ci.org/dkahle/latte)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/dkahle/latte?branch=master&svg=true)](https://ci.appveyor.com/project/dkahle/latte)
<!-- badges: end -->

**latte** is an R package that makes back-end connections to
[LattE](https://www.math.ucdavis.edu/~latte/software.php) and
[4ti2](http://www.4ti2.de). Most of its functions were previously part
of the [**algstat** package](https://github.com/dkahle/algstat), but
have been pulled out and improved upon.

Note: the following assumes you have
[LattE](https://www.math.ucdavis.edu/~latte/) and
[4ti2](http://www.4ti2.de) installed and **latte** recognizes their
path. To help **latte** find 4ti2 and LattE on your machine, you can use
`set_4ti2_path()` and `set_latte_path()` at the beginning of the
session. Better, you can save environment variables that point to where
their executables are using `usethis::edit_r_environ()`; just add two
lines. For example, my lines look like:

``` bash
LATTE=/Applications/latte/bin
FOURTITWO=/Applications/latte/bin
```

You load **latte** like this:

``` r
library("latte")
#   Please cite latte! See citation("latte") for details.
```

Lattice point counting
----------------------

Most [LattE](https://www.math.ucdavis.edu/~latte/) programs are
available as functions in **latte**. For example, `latte_count()` uses
LattE’s `count` to determine the number of integer points in a
[polytope](http://en.wikipedia.org/wiki/Polytope):

``` r
latte_count(c("x + y <= 10", "x >= 0", "y >= 0"))
# [1] 66
```

It’s easy to confirm the solution with a simple visualization:

``` r
library(ggplot2); theme_set(theme_bw())
polytope <- data.frame(x = c(0,10,0), y = c(0,0,10))
points   <- expand.grid(x = 0:10, y = 0:10)
points   <- subset(points, x + y <= 10)
points$number <- 1:nrow(points)
ggplot(aes(x = x, y = y), data = polytope) +
  geom_polygon(fill = "red", alpha = .2) + 
  geom_text(aes(y = y + .25, label = number), size = 3.5, data = points) +
  geom_point(data = points) + 
  coord_equal()
```

![](tools/countExample-1.png)

Integer programming
-------------------

In addition to table counting, it can also do integer programming with
LattE’s `latte-maximize` and `latte-minimize` programs. To do this, it
uses tools from [**mpoly**](http://github.com/dkahle/mpoly):

``` r
latte_max("-2 x + 3 y", c("x + y <= 10", "x >= 0", "y >= 0"))
# $par
#  x  y 
#  0 10 
# 
# $value
# [1] 30
latte_min("-2 x + 3 y", c("x + y <= 10", "x >= 0", "y >= 0"))
# $par
#  x  y 
# 10  0 
# 
# $value
# [1] -20
```

We can check that the solution given above is correct, but the value is
not. So, it needs some more work:

``` r
points$objective <- with(points, -2*x + 3*y)
ggplot(aes(x = x, y = y), data = polytope) +
  geom_polygon(fill = "red", alpha = .2) + 
  geom_point(aes(size = objective), data = points) + 
  coord_equal()
```

![](tools/ipCheck-1.png)

Lattice bases
-------------

You can compute all sorts of bases of integer lattices using
[4ti2](http://www.4ti2.de). For example, if **A** is the matrix

``` r
(A <- genmodel(varlvls = c(2, 2), facets = list(1, 2)))
#      [,1] [,2] [,3] [,4]
# [1,]    1    0    1    0
# [2,]    0    1    0    1
# [3,]    1    1    0    0
# [4,]    0    0    1    1
```

Its Markov basis can be computed

``` r
markov(A, p = "arb")
#      [,1]
# [1,]    1
# [2,]   -1
# [3,]   -1
# [4,]    1
```

Note that the `p = "arb"` signifies that arbitrary precision arithmetic
is supported by using the `-parb` flag at the command line.

Other bases are available as well:

``` r
  zbasis(A, p = "arb")
#      [,1]
# [1,]   -1
# [2,]    1
# [3,]    1
# [4,]   -1
groebner(A, p = "arb")
#      [,1]
# [1,]   -1
# [2,]    1
# [3,]    1
# [4,]   -1
  graver(A)
#      [,1]
# [1,]    1
# [2,]   -1
# [3,]   -1
# [4,]    1
```

`zsolve()` and `qsolve()`
-------------------------

You can solve linear systems over the integers and rationals uing
`zsolve()` and `qsolve()`:

``` r
mat <- rbind(
  c( 1, -1),
  c(-3,  1),
  c( 1,  1)
)
rel <- c("<", "<", ">")
rhs <- c(2, 1, 1)
sign <- c(0, 1)

zsolve(mat, rel, rhs, sign)
# $zinhom
#      [,1] [,2]
# [1,]    2    0
# [2,]    0    1
# [3,]    1    0
# [4,]    1    1
# 
# $zhom
#      [,1] [,2]
# [1,]    1    3
# [2,]    1    1
# [3,]    1    2
```

``` r
mat <- rbind(
  c( 1,  1),
  c( 1,  1)
)
rel <- c(">", "<")
sign <- c(0, 0)

qsolve(mat, rel, sign, p = "arb")
# $qhom
#      [,1] [,2]
# 
# $qfree
#      [,1] [,2]
# [1,]    1   -1
```

Primitive partition identities
------------------------------

The primitive partition identities can be computed with `ppi()`:

``` r
ppi(3)
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   -2    0   -1   -3    1
# [2,]    1   -3   -1    0   -2
# [3,]    0    2    1    1    1
```

This is equivalent to the Graver basis of the numbers 1 to 3. In other
words, its the numbers whose columns, when multiplied by \[1, 2, 3\]
element-wise, sum to zero:

``` r
t(1:3) %*% ppi(3)
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    0    0    0    0    0
```

Installation
------------

-   From CRAN

``` r
install.packages("latte")
```

-   From Github (dev version):

``` r
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/mpoly")
devtools::install_github("dkahle/latte")
```

Acknowledgements
----------------

This material is based upon work supported by the National Science
Foundation under Grant Nos.
[1622449](https://nsf.gov/awardsearch/showAward?AWD_ID=1622449) and
[1622369](https://www.nsf.gov/awardsearch/showAward?AWD_ID=1622369).
