## ----setup, echo=FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#",
  fig.path = "tools/"
)

## ----load-latter---------------------------------------------------------
library("latter")

## ----format_latte--------------------------------------------------------
(mat <- matrix(1:9, nrow = 3))
format_latte(mat)

## ----format_latte-2------------------------------------------------------
cat(format_latte(mat))

## ----format_latte-3------------------------------------------------------
attr(mat, "linearity") <- c(1, 3)
attr(mat, "nonnegative") <- 2
mat
cat(format_latte(mat))

## ----read_write_latte----------------------------------------------------
(filename <- tempfile())
write_latte(mat, filename)
read_latte(filename)

## ----read_write_latte-2--------------------------------------------------
read_latte(filename, format = "Ab")

## ----Ab-notation---------------------------------------------------------
A <- matrix(c(-1, 0, 0, -1, 1, 1), byrow = TRUE, nrow = 3)
b <- c(0, 0, 10)
(mat <- cbind(b, -A))

## ----Ab-notation-2-------------------------------------------------------
code <- format_latte(mat)
cat(code)

## ----count-basic---------------------------------------------------------
count(code)

## ----count-graph, fig.align='center', fig.height=3.5, fig.width=3.5, message=FALSE----
library("ggplot2"); theme_set(theme_classic()); library("magrittr")
polytope <- data.frame(x = c(0, 10, 0), y = c(0, 0, 10))

points <- expand.grid(x = 0:10, y = 0:10) %>% 
  dplyr::filter(x + y <= 10) %>%
  dplyr::mutate(., number = 1:nrow(.))

ggplot(aes(x = x, y = y), data = polytope) +
  geom_polygon(fill = "red", alpha = .2, size = 1) + 
  geom_text(aes(label = number), nudge_y = .3, size = 2.5, data = points) +
  geom_point(data = points, size = .75) + 
  scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
  scale_y_continuous(breaks = 0:10, minor_breaks = NULL) +
  coord_equal()

## ----count-1-------------------------------------------------------------
count(c("x + y <= 10", "x >= 0", "y >= 0"))

## ----count-2-------------------------------------------------------------
vertices <- list(c(0,0), c(10,0), c(0,10))
count(vertices)

## ----count-3-------------------------------------------------------------
count(list(A = A, b = b))

## ----ehrhart-------------------------------------------------------------
count(c("x + y <= 10", "x >= 0", "y >= 0"), ehrhart_polynomial = TRUE)

## ----ip------------------------------------------------------------------
latte_max("-2 x + 3 y", c("x + y <= 10", "x >= 0", "y >= 0"))
latte_min("-2 x + 3 y", c("x + y <= 10", "x >= 0", "y >= 0"))

## ----ip-check, fig.align='center', echo=FALSE, dpi=200, fig.height=2.5, fig.width=3----
theme_set(theme_bw(6))
points %<>% dplyr::mutate(f = -2*x + 3*y)
ggplot(aes(x = x, y = y), data = polytope) +
  geom_polygon(fill = "red", alpha = .2) + 
  geom_point(aes(size = f), data = points) + 
  coord_equal() +
  scale_size("f(x,y)", range = c(.2, 4))

