# Parameter of interest : True Median
# Nonparamteric Bootstrap
library(boot);

# this function will return the bootstrap sample's median by choosing a random
# set of indices
median.npar <- function(x, indices) {
	result <- median(x[indices])
	return(result)
}

# Number of times the experiment needs to be simulated for each value of n
nsim <- 1000;

# declare matrices to store the CI of all three bootstrap CI evaluation methods
NormalCI <- matrix(0,nsim,2);
PercentCI <- matrix(0,nsim,2);
BasicCI <- matrix(0,nsim,2);

# n values
n <- c (5,10,30,100);

# store the coverage probability for each value of n for each methods
NormalCoverageP <- numeric(0);
PercentcoverageP <- numeric(0);
BasicCoverageP <- numeric(0);

# True Median
true.Median <- qgamma(0.5,3,5);

for (a in n) 
{
	#tempSampleMedian <- numeric(0);

	for (i in 1:nsim) 
	{
		bias <- 0;
		stdError <- 0;
		SampleMedian <- 0;
		median.npar.boot <- 0;

		# input sample data for bootstrap 
		g <- rgamma(n,3,5); 

		# this will generate R bootstrap replicates of a statistic applied to a data
		median.npar.boot <- boot(g,median.npar,R=2000,sim="ordinary",stype="i");

		# find the bias and standard error
		bias <- mean(median.npar.boot$t) - median.npar.boot$t0; 

		stdError <- sd(median.npar.boot$t);

		# keep collecting the sample median 
		# SampleMedian <- median.npar.boot$t0;
		#tempSampleMedian <- c(tempSampleMedian, SampleMedian);

		# Get 95% Confidence Interval for the median
		MyCI <- boot.ci(median.npar.boot);

		# Normal Approximation method
		# c(sampleMedian - bias - qnorm(0.975)*stdError, sampleMedian - bias - qnorm(0.025)*stdError);
		# Store the CI for normal by extracting 2nd & 3rd index values
		tempNormalCI <- MyCI$normal[2:3];
		NormalCI[i,1] <- tempNormalCI[1]; 
		NormalCI[i,2] <- tempNormalCI[2];

		# Percentile Bootstrap method
		# sort(median.npar.boot$t)[c(25,975)]
		tempPercentCI <- MyCI$percent[4:5];
		PercentCI[i,1] <- tempPercentCI[1];
		PercentCI[i,2] <- tempPercentCI[2];

		# Basic bootstrap method
		# c(2*sampleMedian - upper,2*sampleMedian - lower) 
		tempBasicCI <- MyCI$basic[4:5];
		BasicCI[i,1] <- tempBasicCI[1];
		BasicCI[i,2] <- tempBasicCI[2];
	}

	# find average sample median
	#AvgSampleMedian <- mean(sampleMedian);

	# Compute the coverage probability for all the three methods by checking if the AvgSampleMedian falls within the range
	# of intervals
	NormalCoverageP <- c(NormalCoverageP, mean( (true.Median >= NormalCI[,1])*(true.Median <= NormalCI[,2]) ) );

	PercentcoverageP <- c(PercentcoverageP, mean( (true.Median >= PercentCI[,1])*(true.Median <= PercentCI[,2]) ) );

	BasicCoverageP <- c(BasicCoverageP, mean( (true.Median >= BasicCI[,1])*(true.Median <= BasicCI[,2]) ) );
}

#> NormalCoverageP;
#[1] 0.848 0.865 0.847 0.840
#> PercentcoverageP;
#[1] 0.871 0.880 0.879 0.876
#> BasicCoverageP;
#[1] 0.735 0.758 0.736 0.718