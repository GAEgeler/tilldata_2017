# logits to probabilites

# source: https://sebastiansauer.github.io/convert_logit2prob/

# state: december 2018

logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}
