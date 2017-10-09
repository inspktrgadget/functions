# example of the exponential suitability function used in gadget
# see gadget manual 4.9.3

exp.suit <- function(alpha, beta, gamma, delta, l, L) {
    delta / (1 + exp(-(alpha) - (beta*l) - (gamma*L)))
}

alpha <- 66
beta <- 0.046
#alpha <- alpha*((-1)*beta)
curve(exp.suit(alpha,beta,0,1,x,500), 0, 175)

# example of exponentiall50 suitability function used in gadget
# see gadget manual 4.9.4

exp.l50 <- function(alpha, l50, l) {
    1 / (1 + exp((-4*alpha)*(l - l50)))
}

alpha <- (0.046 / 4)
l50 <- 67
curve(exp.l50(alpha, l50, x), add=T, col='purple', lwd=4)
