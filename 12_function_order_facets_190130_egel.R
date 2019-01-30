# reordering within facets

# state: january 2019
# source: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R

# @export
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
}


# @rdname reorder_within
# @export
scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}


# @rdname reorder_within
# @export
scale_y_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}


# reorder_within and scale_x_reordered work.
# (Note that you need to set scales = "free_x" in the facet)

# example
iris_gathered <- gather(iris, metric, value, -Species)
ggplot(iris_gathered, aes(reorder_within(Species, value, metric), value)) +
    geom_boxplot() +
    scale_x_reordered() +
    facet_wrap(~ metric, scales = "free_x")

