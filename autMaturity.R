# this code is to get the maturity parameters that are used in gadget.fit
# one could alternatively take them from params.final if doesmature = T


aut.mat <- '~/gadget/gadget-models/path/to/autMaturity.txt'
mat.data <- read.table(aut.mat, sep='\t', header=T)
mat.data <- mutate(mat.data, 
                   len = as.numeric(gsub('len', '', length)),
                   mat = ifelse(age == 'mat', 1, 0))

newdat <- NULL;
for (i in 1:nrow(mat.data)) {
    len.mat <- mat.data[i, ];
    new.len.mat <- data.frame(len = rep(len.mat$len, len.mat$number),
                              mat = rep(len.mat$mat, len.mat$number));
    newdat <- rbind(newdat, new.len.mat)
}


logit <- Rgadget:::logit
logit.vals <- function(mat.par) {
    a <- mat.par[1];
    b <- mat.par[2];
    logit.curve <- logit(a,b,newdat$len);
    return(logit.curve)
}

logit.sse <- function(mat.par) {
    vals <- logit.vals(mat.par);
    sse <- sum((newdat$mat - vals)^2);
    return(sse);
}

nlm(logit.sse, c(-18, 0.5))
plot(mat ~ len, newdat)
curve(logit(-18.2401844, 0.5536777, x), add=T, col='red')
