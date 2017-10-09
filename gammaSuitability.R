# code to setup and optimize the gamma suitability function
# gadget user guide 4.9.7
library(dplyr)
library(ggplot2)

gamma <- function(a,b,g,length) {
    ((length / ((a-1)*b*g))^(a-1))*exp(a - 1 - (length / (b*g)))
}

gamma.optimizer <- function(data, length) {
    a <- data[1]
    b <- data[2]
    g <- data[3]
    return(gamma(a,b,g,length))
}

gamma.sse <- function(data, vals, length) {
    v.hat <- gamma.optimizer(data, length)
    return(sum((vals - v.hat)^2))
}

# length <- 1:199
# vals <- c(10,20,30,35,37,30,20,10,8,7,6) / 37
# vals2 <- c(100, 60, 40, 30, 24, 19, 15, 12, 11, 10, 9) / 100
# 
# params <- nlm(gamma.sse, c(1,1,2), vals2, age)
# 
# plot(vals2 ~ age)
# curve(gamma.optimizer(params$estimate, x), add=T)
# 
# # plotting the curves; plotting family of curves is difficult in ggplot2
# # you need to create and plot the data
# a <- seq(-2,2,by=0.5)
# b <- seq(-2,2,by=0.5)
# g <- seq(-2,2,by=0.5)
# age <- 0:10
# data <- expand.grid(a=a,b=b,g=b,age=age)
# data <- mutate(data, vals = gamma(a,b,g,age)) %>% filter(vals != 'NaN')
# 
# 
# ggplot(data=data, aes(x=age, y=vals, color=factor(g))) + geom_line() +
#     facet_wrap(~a+b, scales='free_y')
