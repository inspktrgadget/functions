# this is to find maturity parameters from actual data

library(dplyr)

# define the continuous maturity function
m <- function(alpha, l50, beta, a50, l, a) {
    1 / (1 + exp(((-alpha)*(l - l50)) - (beta*(a - a50))))
}

# params from gadget optimization run
alpha <- 0.001 * 105.68825
l50 <- 20

# plot curves to understand function
b <- 1:60
plot((b/100) ~ b, type='n', xlim=c(0,60), ylim=c(0,1))
for (i in 1:30) {
    curve(m(alpha, l50, 0.1, 7, x, a = i), add=T, col=i)
}

# maturity data taken from Magnusson 1996
length <- 29:52
male.gss <- c(0.028,0.034,0.084,0.076,0.133,0.194,0.32,0.439,0.523,0.651,0.838,
              0.855,0.892,0.960,0.971,0.973,0.988,0.987,0.988,0.992,0.993,0.994,0.962,1)
female.gss <- c(0,0,0,0.011,0.026,0.072,0.120,0.251,0.379,0.536,0.656,0.778,0.896,
                0.970,0.980,0.997,0.991,0.990,0.990,0.989,0.997,1,1,1)
gss <- data.frame(length, male = male.gss, female = female.gss)
gss <- gss %>% group_by(length) %>% summarize(mean = mean(male, female))
plot(mean ~ length, data=gss)
curve(m(0.51217145, 36.5713934, 0, 0, x, 0), add=T) # params taken from sse below

# function to take sse from maturity function on above data
m.sse <- function(params) {
    alpha = params[1]
    l50 = params[2]
    beta = params[3] 
    a50 = params[4] 
    m.predict = m(alpha, l50, beta, a50, gss$length, 0);
    sse <- sum((gss$mean - m.predict)^2)
    return(sse)
}
nlm(m.sse, c(0.11, 30, 0, 0))



