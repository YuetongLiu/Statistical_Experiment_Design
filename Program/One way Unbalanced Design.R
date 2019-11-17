# simulation for the fixed effects model for the
#    one-way layout with unequal sample sizes.
# ntrt = numer of treatments
# nrepvec = vector of sample sizes for the treatments, length is ntrt
# muvec = vector of population means of length ntrt, default is zero vector
# sigmacom = common population SD 
# theta =  parameter to control heteroscedaticity with sigma_i propto mu_i^theta
#        (assuming muvec>-0)
# sigmacustom = proportionality constant for sigma vector
# do.plot = flag, T to plot sample sd_i versus sample mn_i
# output is (a) vector of sample SDs for the ntrt groups
#   to check the amount of variability if there is a common population SD
#  (b) F ratio 
oneway.unbalanced <- function(ntrt, nrepvec, muvec=0,
                              sigmacom=1, theta=0, sigmacustom=1, do.plot=F)
{
  ## if the vector of means is not specified then
  ## assume it's a vector of zeros
  if (sum(muvec^2)==0) muvec <- rep(0,ntrt)
  if (length(nrepvec)!=ntrt)
    stop("nrepvec must have length of ntrt")
  
  ## empty matrix to store samples
  X <- matrix(NA, ncol=ntrt, nrow=max(nrepvec))
  
  ## SDs for the ntrt subgroups,
  ## proportional to the means^theta
  sigmas <- sigmacom * muvec^theta * sigmacustom
  
  ## generate the data
  for (i in 1:ntrt) X[1:nrepvec[i], i] <- rnorm(nrepvec[i], muvec[i], sigmas[i])
  ## compute sd's for all groups
  sds <- apply(X, 2, sd, na.rm=TRUE)
  ## compute F-test statistic
  smeans <- apply(X, 2, mean, na.rm=TRUE)
  ybar <- mean(c(X), na.rm=TRUE)
  
  MSt <- sum(nrepvec*(smeans-ybar)^2)/(ntrt-1)
  MSe <- sum(sds^2*(nrepvec-1))/(sum(nrepvec) - ntrt)
  F <- MSt/MSe
  ## plot them if requested
  if (do.plot) plot(muvec, sds, ylim=c(0,max(sds)+0.1))
  
  return(list(sd=sds, F=F))
}

# sample run of the function
par(mfrow=c(1,1))
out=oneway.unbalanced(5, nrepvec=2:6)
print(out)

# many replications of the simulation in oneway.unbalanced
# ntrt = numer of treatments
# nrepvec = vector of sample sizes for the treatments, length is ntrt
# Nrepl = number of replications
# means = vector of population means of length ntrt, default is zero vector
# sigma = vector of population SDs 
# theta =  parameter to control heteroscedaticity with sigma_i propto mu_i^theta
#        (assuming muvec>-0)
# sigmacustom = proportionality constant for sigma vector
# do.ratios = flag, T to get ratio of (max group SD)/(min group SD) 
# cutoff = to keep track of frequency of (max group SD)/(min group # SD)>cutoff
# output is vector of sample SDs for the ntrt groups
# do.F = flag, T to get F ratio
# if do.ratios and do.F are T, output includes
#  histograms of the ratio of (max group SD)/(min group SD) and the F ratio
many.oneway.unbalanced <- function(ntrt, nrepvec, Nrepl=100, means=1:ntrt, theta=0,
                                   sigma = 1, sigmacustom = 1, do.ratios=T, cutoff=2, do.F=T)
{
  
  ### prepare empty containers
  sd.store <- matrix(NA, ncol=ntrt, nrow=Nrepl)
  F.store <- rep(NA, Nrepl)
  
  ### run the simulations  
  for (i in 1:Nrepl)
  { onerun <- oneway.unbalanced(ntrt, nrepvec=nrepvec, muvec=means, theta=theta,
                                sigmacom=sigma, sigmacustom=sigmacustom, do.plot=F)
  sd.store[i,] <- onerun$sd
  F.store[i] <- onerun$F
  }
  
  ### compute some statistics
  
  if(do.ratios+do.F>=1) par(mfrow=c(1,do.ratios+do.F))
  if (do.ratios)
  { ## for each Sample, get ratio of the max sd to the min sd
    ratios <- apply(sd.store, 1, function(x){max(x)/min(x)})
    hist(ratios,main="max grp SD/ min grp SD",xlab="")
    cat("\n", round(sum(ratios>cutoff)/Nrepl*100,0),
        "% of max/min SD ratios is bigger than ",cutoff,"\n")
  }
  if (do.F)
  { hist(F.store,main="F ratios",xlab="F")
    cat("\n Average F-statistic is ",round(mean(F.store),2)," \n",
        round(sum(F.store>qf(0.95,df1=ntrt-1,df2=(sum(nrepvec)-ntrt)))/Nrepl*100),
        "% of F-stats are bigger than 95% percentile of F-dist \n")
  }
}

# sample run
many.oneway.unbalanced(ntrt=3, nrep=(4:6), means=c(1,1,1), sigmacustom=c(1,1,1), Nrepl=1000)
###Here you should see around 5\% of replications have a F-statistic larger than 
###the 95th percentile of the corresponding F distribution.
###Try more simulation cases with different inputs to understand
# the variability as sample sizes increase etc. 

