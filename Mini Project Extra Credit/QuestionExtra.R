# import bootstrap library
library(boot);

# cpu data that is assumed to come from a random sample of gamma distribution
cpu <- c(70, 36, 43, 69, 82, 48, 34, 62, 35, 15, 59, 139, 46, 37, 42,
			 30, 55, 56, 36, 82, 38, 89, 54, 25, 35, 24, 22, 9, 56, 19);

# function to find the negative log likelihood function
neg.loglik.fun <- function(par, dat)
{	
	result <- sum(dgamma(dat, shape=par[1], rate=par[2], log=TRUE))
	return(-result)
}

# function to find the shape and rate parameters
par.mle <- function(input_data, par.init = c(3, 0.01)) 
{
	mle.est <- optim(par=par.init, fn=neg.loglik.fun, method = "L-BFGS-B", 
				lower=rep(0,2), dat=input_data)$par
	names(mle.est) <- c("shape", "rate")	
	return(mle.est)	
} 

# function to get mle of median
median.mle <- function(dat) 
{ 
	# MLE of gamma parameters
	par.est <- par.mle(dat)
	# MLE of median
	median.est <- qgamma(0.5, shape = par.est["shape"], rate = par.est["rate"])
	return(median.est)
}


# show the estimated rate and shape parameters
(mle.est <- par.mle(cpu));

# mle of the median
true.Median <- (median.mle(cpu));

# function to create bootstrap samples from the exiting data using the
# estimated shape and rate parameter
gamma.resample <- function(dat, mle.est) 
{ 
  xsim <- rgamma(length(dat), mle.est$shape, mle.est$rate)
  return(xsim)
}

# no of resamples
ReSampleCount <- 1000

Sample.BootStrap.medians <- c()

# keep collecting bootstrap medians until we get 1000 such bootstrap sample medians
while (length(Sample.BootStrap.medians) <= ReSampleCount - 100) 
{
	# generate 100 bootstrap resamples 
	median.par.boot <- tryCatch (
	{
		median.par.boot <- boot(cpu, median.mle, R=100, sim="parametric", 
						ran.gen=gamma.resample,
                         mle=list(shape=mle.est["shape"], rate=mle.est["rate"]));
	},
	error=function(err) 
	{

	},
	finally = {
		
		Sample.BootStrap.medians <- c(Sample.BootStrap.medians, median.par.boot$t)
	}
	)
}



# verify if bias and standard error are correct 
bias <- (mean(Sample.BootStrap.medians) - true.Median);

stdError <- sd(Sample.BootStrap.medians);

# Get 95% Confidence Interval

# Normal Approximation Method
Normal.Approx <- c(true.Median - bias - qnorm(0.975) * stdError, true.Median - bias - qnorm(0.025));

# Percentile Bootstrap Method
(Percentile <- c(sort(Sample.BootStrap.medians)[c(25,975)]));

# Basic Bootstrap Method
Basic <- c(2*true.Median - Percentile[2], 2*true.Median - Percentile[1]); 

# Comparison between Parametric and Non-Parametric Method
# Parametric -------vs------- Non-Parametric
# 			Normal Approximation
# (35.04708, 45.50470) 		  (30.31, 53.35)
#
#				Percentile
# (35.94931, 53.84567)		  (35.50, 55.50)
#
# 				   Basic
# (35.28328, 51.49624)		  (29.50, 49.50)				