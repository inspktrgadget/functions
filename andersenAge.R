# function to do initial ages as andersen suitability function
andersen <- function(a,b,c,d,age) {
    a * exp(-(((log(age) - b)^2) / c)) + d
}

andersen.optimizer <- function(data, age) {
    a <- data[1];
    b <- data[2];
    c <- data[3];
    d <- data[4];
    return(andersen(a,b,c,d,age))
}

andersen.sse <- function(data, vals, age) {
    v.hat <- andersen.optimizer(data, age);
    return(sum((vals - v.hat)^2))
}

# age <- 0:10
# vals <- c(10,20,30,35,37,30,20,10,8,7,6) / 37
# vals2 <- c(100, 60, 40, 30, 24, 19, 15, 12, 11, 10, 9) / 100
# 
# params <- nlm(andersen.sse, c(1,1,1,1), vals, age)
# 
# plot(vals ~ age)
# curve(andersen.optimizer(params$estimate, x), add=T)
