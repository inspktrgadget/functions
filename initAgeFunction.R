## script to define and parameterize an initial value function for gadget
init.age.function <- function(age, init.decay, init.max, init.min) {
    (exp(-(init.decay*age))*(init.max - init.min)) + init.min
}

init.age.optimizer <- function(data, age) {
    init.decay <- data[1];
    init.max <- data[2];
    init.min <- data[3];
    return(init.age.function(age, init.decay, init.max, init.min))
}

init.age.sse <- function(data, age, vals) {
    v.hat <- init.age.optimizer(data, age);
    return(sum((v.hat - vals)^2))
}

atl.init <- 
    is_fg_count %>%
    filter(year == min(year),
           month == min(month),
           count >= 1) %>%
    mutate(count = count / 2) %>%
    group_by(age) %>%
    summarize(init.vals = sum(count))
params <- nlm(init.age.sse, c(0.2, 2.82, 0.002), 
              age=atl.init$age, vals=atl.init$init.vals/1e8)
plot(init.vals/1e8 ~ age, data=atl.init)
curve(init.age.optimizer(params$estimate, x), add=T)
