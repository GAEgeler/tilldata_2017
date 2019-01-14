# change first letter of shop_description
firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}