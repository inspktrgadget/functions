# functions to compute and minimize length weight relationship parameters
lw <- function(a,b,l) {a*l^b}

lw.optimizer <- function(data, l) {
    a <- data[1];
    b <- data[2];
    a*l^b
}

lw.sse <- function(data, l, weight) {
    w.hat <- lw.optimizer(data, l);
    return(sum((weight - w.hat)^2))
}