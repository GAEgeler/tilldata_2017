# detects dark color: for labelling the bars
# source: https://stackoverflow.com/questions/49437263/is-there-a-is-light-or-is-dark-color-function-in-r

# state: mai 2018

isDark <- function(color) {
    (sum(grDevices::col2rgb(color) *c(299, 587,114))/1000 < 123)
}