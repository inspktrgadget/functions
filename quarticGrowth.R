## THIS FUNCTION DOES NOT WORK VERY WELL
## PERHAPS TRY A CUBIC POLYNOMIAL???
quartic.growth<- function(a,b,c,d,e,age) {
    a*age^4 + b*age^3 + c*age^2 + d*age + e
}

quartic.growth.optimizer <- function(data, age) {
    a <- data[1];
    b <- data[2];
    c <- data[3];
    d <- data[4];
    e <- data[5];    
    a*age^4 + b*age^3 + c*age^2 + d*age + e;
}

quartic.growth.sse <- function(data, weight, age) {
    w.hat <- test.fun(data, age);
    return(sum((weight - w.hat)^2))
}