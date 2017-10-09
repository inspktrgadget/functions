# define exponential growth by age function
# and optimize params for each year, month

exp.growth <- function(age, max.wt, a, c) {
    max.wt / (1 + exp(-a*(age - c)))
}

exp.growth.optimizer <- function(data, age) {
    max.wt <- data[1];
    a <- data[2];
    c <- data[3];
    return(max.wt / (1 + exp(-a*(age - c))))
}

exp.growth.sse <- function(data, weight, age) {
    w.hat <- exp.growth.optimizer(data, age);
    return(sum((weight - w.hat)^2))
}



