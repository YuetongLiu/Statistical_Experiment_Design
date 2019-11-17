######  Fixed effect models for randomized (complete) block design
# This code can also be used for the interaction plot for a balanced
#  2-factor factorial design

# alpha = vector of treatment effects
# beta = vector of block effects
# nrep = number of replications for each treatment within a block
#     (see Section 5.1.3 of Tamhane's book).
# mu = overall mean (default 0)
# sigma  = SD of E_{ij} (default 1)
# output: plot that checks for additivity (or no interaction)
rbd.interact <- function(alpha, beta, nrep, mu=0, sigma=1)
{ ntrt <- length(alpha)
b <- length(beta)
Xmeans <- matrix(NA, ncol=ntrt, nrow=b)
for (i in 1:ntrt)
{ for (j in 1:b)
{ cell.sample <- rnorm(nrep, mean=alpha[i]+beta[j], sd=sigma)
Xmeans[j,i] <- mean(cell.sample)
}
}
par(mfrow=c(1,2))
# ybar_{+j} vs block for each treatment
matplot(Xmeans,xlab="block",ylab=expression(bar(y)[+j]))
title(expression(bar(y)[+j] * " vs block for each trt"))
matlines(Xmeans)
# ybar_{i+} vs treatment for each block
matplot(t(Xmeans),xlab="treatment",ylab=expression(bar(y)[i*'+']))
title(expression(bar(y)[i*'+'] * " vs trt for each block"))
matlines(t(Xmeans))
}


## Try a few times to get an idea of variations
## and amount of deviation from parallel curves based on randomness in model.
set.seed(1234)
rbd.interact(c(1,2,-3), c(2,-1,3,-4), nrep=1)

rbd.interact(c(1,2,-3), c(0,0,0,0), nrep=1)
rbd.interact(c(1,2,-3), c(0,0,0,0), nrep=10)

# copy-and-paste these examples and try others on your own
# rbd.interact(c(1,1.5,-2.5), c(2,1,-1,-2), nrep=1)
#  Repeat with the up arrow in R console to see variations
#    for other replications with same inputs
#  How often do the curves cross (or are far from parallel)?
# rbd.interact(c(-1,0,1), c(2,0,0,-2), nrep=1)

# rbd.interact(c(1,3,-4), c(4,2,-2,-4), nrep=1)
#   treatment and block effects farther apart relative to sigma
#   so frequency of crossing curves should decrease
