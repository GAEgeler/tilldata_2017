
# function from here: https://stackoverflow.com/questions/32399843/how-can-i-confirm-that-two-r-objects-have-the-same-structure

CompareStructure <-
    function(x, y) {
        # function to recursively compare a nested list of structure annotations
        # using pairwise comparisons
        TypeCompare <-
            function(xSTR, ySTR) {
                if (length(xSTR) == length(ySTR)) {
                    all(mapply(
                        xSTR,
                        ySTR,
                        FUN = function(xValue, yValue) {
                            if (is.list(xValue) && is.list(yValue)) {
                                all(TypeCompare(xValue, yValue))
                            } else if (is.list(xValue) == is.list(yValue)) {
                                identical(xValue, yValue)
                            } else {
                                FALSE
                            }
                        }
                    ))
                } else {
                    FALSE
                }
            }
        
        # if both inputs are lists
        if (is.list(x) && is.list(y)) {
            # use Rapply to recursively apply function down list
            xSTR <-
                rapply(
                    x,
                    f = function(values) {
                        c(mode(values), length(values))
                    },
                    how = "list"
                )
            
            # use Rapply to recursively apply function down list
            ySTR <-
                rapply(
                    y,
                    f = function(values) {
                        c(mode(values), length(values))
                    },
                    how = "list"
                )
            
            # call the compare function on both structure annotations
            return(TypeCompare(xSTR, ySTR))
            
        } else {
            # if inputs are not same class == automatic not same structure
            if (class(x) != class(y)) {
                FALSE
            } else {
                # get dimensions of the x input, if null get length
                xSTR <-
                    if (is.null((dimsX <- dim(x)))) {
                        length(x)
                    } else {
                        dimsX
                    }
                
                # get dimensions of the y input, if null get length
                ySTR <-
                    if (is.null((dimsY <- dim(y)))) {
                        length(y)
                    } else {
                        dimsY
                    }
                
                # call the compare function on both structure annotations
                return(TypeCompare(xSTR, ySTR))
            }
        }
    }
