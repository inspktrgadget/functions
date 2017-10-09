# function to describe constant maturity
const.mat <- function(alpha, l50, beta, a50, l, a) {
    1 / (1 + exp(((-alpha)*(l - l50)) - (beta * (a - a50))))
}