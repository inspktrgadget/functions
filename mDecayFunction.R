# natural mortality as a function of age with functions to optimize such
m_decay_func <- function(age, m.decay, max.m, min.m) {
    exp((-1)*m.decay*age)*(max.m - min.m) + min.m
}

m_decay_optimizer <- function(data, age) {
    m.decay <- data[1];
    max.m <- data[2];
    min.m <- data[3]
    return(m_decay_func(age, m.decay, max.m, min.m))
}

m_decay_sse <- function(data, age, vals) {
    val.hat <- m_decay_optimizer(data, age);
    return(sum((vals - val.hat)^2))
}