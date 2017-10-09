# code to compute and fit von Bertalanffy growth curves to species

vb <- function(linf, k, t0, age) {
        length <- linf*(1 - exp(-k*(age-t0)));
        return(length);
}

# linf <- 150
# k <- 0.06
# curve(vb(linf, k, x, 0), 0, 150)

# define an sse function that we can minimize on
vb.optimizer <- function(b, age) {
    linf <- b[1];
    k <- b[2];
    t0 <- b[3];
    return(vb(linf, k, t0, age))
}

vb.sse <- function(b, length, age) {
    lhat <- vb.optimizer(b, age);
    return(sum((length-lhat)^2));
}

# read in data here to perform the minimizing function on


# # performing a nonlinear minimization on parameters to fit this vb curve to data better
# t <- #read in some age data
# l <- #read in some length data
# vbMin <- nlm(sse, c(150, 0.13, -1))
# 
# # visualize the output
# #plot(mean~age, data=la)
# linf.min <- vbMin$estimate[1]
# k.min <- vbMin$estimate[2]
# t0.min <- vbMin$estimate[3]
# #curve(vb(linf.min, k.min, x, t0.min), add=T, col='red', lwd=1.4) # as you can see, not quite there
# 
# g <- ggplot(filter(is_fg_count, count>0), aes(x=age, y=length)) + geom_point() +
#     stat_function(fun=function (x) vb(175, 0.1622, x, 2)) 
# # curve to test parameter bounds    + stat_function(fun=function(x) vb(linf.min, k.min, x, t0.min))
# # curve to test parameter bounds    + stat_function(fun=function(x) vb(linf.min, k.min, x, t0.min)) 


