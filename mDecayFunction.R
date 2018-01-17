# natural mortality as a function of age with functions to optimize such
m_decay_func <- function(max_m, min_m, m_decay, age) {
    exp((-1)*m_decay*age)*(max_m - min_m) + min_m
}

m_decay_optimizer <- function(data, age) {
    max_m <- data[1];
    min_m <- data[2]
    m_decay <- data[3];
    return(m_decay_func(max_m, min_m, m_decay, age))
}

m_decay_sse <- function(data, age, vals) {
    val_hat <- m_decay_optimizer(data, age);
    return(sum((vals - val_hat)^2))
}