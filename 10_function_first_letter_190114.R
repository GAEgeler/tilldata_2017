# change first letter of shop_description
# source: https://stackoverflow.com/questions/18509527/first-letter-to-upper-case

# state: august 2018

firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}