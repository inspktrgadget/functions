vb_simple <- function(linf, k, recl, age) {
    linf * (1 - (exp(((-1)*k) * (age - (1 + ((log (1 - (recl / linf))) / k))))))
}

vb_simple_optimizer <- function(data, age) {
    linf <- data[1];
    k <- data[2];
    recl <- data[3];
    return(vb_simple(linf, k, recl, age))
}

vb_simple_sse <- function(data, length, age) {
    l_hat <- vb_simple_optimizer(data, age)
    return(sum((l_hat - length)^2))
}

# ## optimizing paramter values for gadget zbraInit
# std.file <- '~/gadget/models/gadgetTest/zbraInit/WGTS/out.fit/zbra.std'
# std <- read.table(std.file, comment.char = ';')
# names(std) <- c('year', 'step', 'area', 'age', 'number',
#                 'mean.length', 'mean.wt', 'length.sd',
#                 'number.consumed', 'biomass.consumed')
# age <- std$age
# vals <- std$mean.length
# 
# params <- nlm(vb.simple.sse, c(155, 0.08, 15), age=age, length=vals)
# 
# plot(mean.length ~ age, std)#taken from gadgetTest/modelCheck/getInitOutput.R
# curve(vb.simple.optimizer(params$estimate, x), add=T)




