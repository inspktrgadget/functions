# natural mortality as a function of age with functions to optimize such
m.decay.func <- function(age, m.decay, max.m, min.m) {
    exp((-1)*m.decay*age)*(max.m - min.m) + min.m
}

m.decay.optimizer <- function(data, age) {
    m.decay <- data[1];
    max.m <- data[2];
    min.m <- data[3]
    return(m.decay.func(age, m.decay, max.m, min.m))
}

m.decay.sse <- function(data, age, vals) {
    val.hat <- m.decay.optimizer(data, age);
    return(sum((vals - val.hat)^2))
}