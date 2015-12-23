## ------------------------------------------------------------------------
library(latter)

## ------------------------------------------------------------------------
(mat <- matrix(1:9, nrow = 3))
format_latte(mat)

## ------------------------------------------------------------------------
cat(format_latte(mat))

## ------------------------------------------------------------------------
attr(mat, "linearity") <- c(1, 3)
attr(mat, "nonnegative") <- 2
mat
cat(format_latte(mat))

## ------------------------------------------------------------------------
(filename <- tempfile())
write.latte(mat, filename) # the output here is invisible, it's the same as format_latte()
read.latte(filename)

## ------------------------------------------------------------------------
read.latte(filename, format = "Ab")

## ----count---------------------------------------------------------------
count(c("x + y <= 10", "x >= 0", "y >= 0"))

## ----countExample, fig.height=2.5, fig.width=2.5, dpi=200, fig.align='center'----
library(ggplot2); theme_set(theme_bw(8)); library(magrittr)
polytope <- data.frame(x = c(0, 10, 0), y = c(0, 0, 10))

points   <- expand.grid(x = 0:10, y = 0:10) %>% dplyr::filter(x + y <= 10) %>%
  dplyr::mutate(., number = 1:nrow(.))

ggplot(aes(x = x, y = y), data = polytope) +
  geom_polygon(fill = "red", alpha = .2) + 
  geom_text(aes(label = number), nudge_y = .25, size = 3.5, data = points) +
  geom_point(data = points) + 
  coord_equal()

## ----ip------------------------------------------------------------------
latte_max("-2 x + 3 y", c("x + y <= 10", "x >= 0", "y >= 0"))
latte_min("-2 x + 3 y", c("x + y <= 10", "x >= 0", "y >= 0"))

## ----ipCheck, fig.height=6, dpi=200--------------------------------------
points$objective <- with(points, -2*x + 3*y)
ggplot(aes(x = x, y = y), data = polytope) +
  geom_polygon(fill = "red", alpha = .2) + 
  geom_point(aes(size = objective), data = points) + 
  coord_equal()

