>
# compute confidence interval for a proportion p
# given parameters are p, n and level of confidece = 95%

par(mfrow = c(2,3));
conf.interval <- function(n, p, alpha) 
{    
    # create a random binomial distribution with n observations 
    x <- rbinom(n,1,p)
     
    # calculate pcap
    pCap <- mean(x)
    
    ci <- pCap + c(-1, 1) * qnorm(1 - (alpha/2)) * sqrt(pCap*(1-pCap)/n)
     
    return (ci)	
     
}
 
# declare the probability matrix
probability.mat <- c(0.05,0.10,0.25,0.5,0.9,0.95);
 
# no of times to be simulated
nsim <- 100;
 
#n <- 88;
#probability <- 0.05;

coverageP <- numeric(0);

for (i in 1:6 ) 
{
	coverageP <- numeric(0);

	for (n in 1:100)
	{
		# compute Confidence Interval for nsim no of simulations
		ci.mat <- replicate(nsim,conf.interval(n,probability.mat[i],0.05));

		# store nsim pair of confidence interval 
		coverageP <- c(coverageP, mean( (probability.mat[i] >= ci.mat[1,])*(probability.mat[i] <= ci.mat[2,]) ));
		
	}

	# find the mean coverage probability
	#coverageP <- c(coverageP,mean(NsimMean));

	# plot the coverage probability for each value of P
	# with n against coverage probability
	plot(1:100,coverageP,type="l",col="blue");

}
