spawn.exp.func <- function(l, l50, alpha) {
    1 / (1 + exp(alpha * (l - l50)))
}