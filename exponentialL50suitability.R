# define and plot the exponentiall50 suitability function in gadget
exp.l50 <- function(alpha, l, l50) {
    1 / (1 + exp((-alpha)*(l - l50)))
}

# alpha <- 0.046
# l50 <- 66
# 
# curve(exp.l50(alpha, x, l50), 0, 199)
# 
# library(ggplot2)
# alpha <- 0.046
# l50 <- 66
# x <- 1:150
# ggplot(data=NULL, aes(x=x)) + 
#     stat_function(fun=exp.l50, args=list(alpha=alpha, l50=l50)) + 
#     theme_bw() + xlab("Length (cm)") + ylab("Selectivity") + 
#     theme(axis.title=element_text(size=20),
#           axis.text=element_text(size=16))
    