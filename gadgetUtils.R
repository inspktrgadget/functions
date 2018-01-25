# handy wrapper functions for producing gadget files in association with Rgadget
vonb_formula <- function(linf, k, recl, age) {
    vonb <- 
        as.quoted(
            paste0(linf,
                   "* (1 - exp((-1 *",
                   k,
                   ") * (",
                   age,
                   "- (1 + (log(1 - (",
                   recl,
                   "/",
                   linf,
                   ")))/",
                   k,
                   "))))"))
    sapply(vonb, to.gadget.formulae)
}

## functions to write suitability lines
# fleet suitability
fleet_suit <- function(fleet="comm", 
                       stock=NULL, 
                       fun="exponentiall50", 
                       params=NULL,
                       sep_stock_params = FALSE,
                       param_names = stock[1]) {
    if (sep_stock_params) {
        params2paste <- 
            do.call("paste", 
                    lapply(stock, function(x) {
                        lapply(params, function(y) {
                            if (is.numeric(y)) {
                                return(y)
                            } else {sprintf("#%1$s.%2$s.%3$s",
                                        x, fleet, y)}
                    })
                }))
    } else {
        params2paste <- 
            do.call("paste",
                    lapply(params, function(x) {
                        if (is.numeric(x)) {
                            return(x)
                        } else {
                            sprintf("#%1$s.%2$s.%3$s",
                                    param_names, fleet, x)
                        }
                    }))
    }
    fun_call <- paste(stock, "function", fun, params2paste, sep = "\t")
    return(paste0("\n", fun_call))
}

# predator suitability
pred_suit <- function(pred="comm", 
                      stock=NULL, 
                      fun="newexponentiall50", 
                      params=NULL) {
    paste0("\n",
           paste(stock, "function", fun, 
                 ifelse(is.numeric(params),
                        params,
                        do.call(paste, lapply(params, function(x) {
                            if (is.numeric(x)) {
                                return(x)
                            } else {
                                sprintf("#%1$s.%2$s.%3$s",
                                        stock, pred, x)
                            }
                        }))),
                 sep="\t"))
}

# surveydistribution suitability
surveydist_suit <- function(pred="survey",
                            stock=NULL,
                            fun="newexponentiall50",
                            params=NULL) {
    paste0(paste("function", fun, 
                 ifelse(is.numeric(params),
                        params,
                        do.call(paste, lapply(params, function(x) {
                            if (is.numeric(x)) {
                                return(x)
                            } else {
                                sprintf("#%1$s.%2$s.%3$s",
                                        stock, pred, x)
                            }
                        }))),
                 sep="\t"))
}


init_age_factor <- function(init_max, init_min, init_decay, age) {
    expr <- as.quoted(paste("exp(((-1) *", 
                            init_decay, 
                            ") * ", 
                            age, 
                            ") * (", 
                            init_max,
                            " - ",
                            init_min,
                            ") + ",
                            init_min))
    sapply(expr, to.gadget.formulae)
}

m_estimate_formula <- function(max_m, min_m, m_decay, age) {
    expr <- as.quoted(paste("exp(((-1) *", 
                            m_decay, 
                            ") * ", 
                            age, 
                            ") * (", 
                            max_m,
                            " - ",
                            min_m,
                            ") + ",
                            min_m))
    sapply(expr, to.gadget.formulae)
}

init_params <- function(params.data, switch, value, 
                        lower.bound, upper.bound, optimise) {
    w.switch <- grep(switch, params.data$switch);
    if (length(w.switch) != 0) {
        update.switch <- data.frame(switch = params.data$switch[w.switch], 
                                value = value, lower = lower.bound, 
                                upper = upper.bound, optimise = optimise);
        update.switch$switch <- as.character(update.switch$switch);
        params.data[w.switch, ] <- update.switch;
        return(params.data);
    }
    else {return(params.data)}
}

# function to output recruitment function for spawnfile
# this should be generalized to accept any recruitment function
bevHoltRec <- function(stock_data) {
    "bevertonholt" %>%
        c(paste0("#", stock_data[[1]]$stockname, ".bh.mu")) %>%
        c(paste0("#", stock_data[[1]]$stockname, ".bh.lam")) %>%
        paste(., sep = "\t")
}

# function to set up spawnfile
spawnfile <- function(stock_data, start_year, end_year, ...) {
    argslist <- list(
        spawnsteps = 1,
        spawnareas = stock_data[[1]]$livesonareas,
        firstspawnyear = start_year,
        lastspawnyear = end_year,
        spawnstocksandratios = paste(stock_data[[1]]$stockname, 1, sep = "\t"),
        proportionfunction = paste("constant", 1, sep = "\t"),
        mortalityfunction = paste("constant", 0, sep = "\t"),
	    weightlossfunction = paste("constant", 0, sep = "\t"),
        recruitment = bevHoltRec(stock_data),
        stockparameters = paste(sprintf("#%s.rec.len", stock_data[[1]]$stockname),
                                sprintf("#%s.rec.sd", stock_data[[1]]$stockname),
                                paste0(stock_data$doesgrow$growthparameters[3:4], 
                                       collapse = "\t"),
                                sep = "\t")
    )
    override_defaults <- list(...)
    if (any(names(override_defaults) %in% names(argslist))) {
        names2override <- intersect(names(argslist), names(override_defaults))
        argslist[names2override] <- override_defaults[names2override] 
    }
    return(call("gadgetfile", "spawnfile", component = list(argslist)))
}

standard.age.factor <- "exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s"
andy.age.factor <- "(%2$s.age.alpha * exp((-1)  * (((log(%1$s)) - %2$s.age.beta) * ((log(%1$s)) - %2$s.age.beta) / %2$s.age.gamma)) + %2$s.age.delta)"
gamma.age.factor <- "(%1$s / ((%2$s.age.alpha - 1) * (%2$s.age.beta * %2$s.age.gamma))) ** (%2$s.age.alpha - 1) * exp(%2$s.age.alpha - 1 - (%1$s / (%2$s.age.beta * %2$s.age.gamma)))"
