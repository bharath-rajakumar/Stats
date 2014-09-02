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

# declare n values
n.mat <- c(5,10,30,100);
 
# no of times to be simulated
nsim <- 100;
 
#n <- 88;
#probability <- 0.05;
# declare a two dim matrix to hold coverage probability for various values of n and p
finalAnswer <- matrix(0, nrow = length(probability.mat), ncol = length(n.mat));

# create a row name and column name so that the single column or row values can be fetched quickly
rownames(finalAnswer) <- c("A", "B", "C", "D", "E", "F");
colnames(finalAnswer) <- c("P", "Q", "R", "S");

coverageP <- numeric(0);

for (i in 1:6 ) 
{
	coverageP <- numeric(0);

	for (j in 1:4)
	{
		# compute Confidence Interval for nsim no of simulations
		ci.mat <- replicate(nsim,conf.interval(n.mat[j],probability.mat[i],0.05));

		# store nsim pair of confidence interval 
		coverageP <- c(coverageP, mean( (probability.mat[i] >= ci.mat[1,])*(probability.mat[i] <= ci.mat[2,]) ));

		# add it to the finalAnswer matrix where rows are the probabilities p and the columns are the sample size n
		#finalAnswer[i,j] <- coverageP;
	}

	# find the mean coverage probability
	#coverageP <- c(coverageP,mean(NsimMean));

	# plot the coverage probability for each value of P
	# with n against coverage probability
	# plot(1:100,coverageP,type="l",col="blue");

}

# plot for n = 5
tempN1 <- c(finalAnswer[,"A"]);
plot(probability.mat,tempN1,type="l",col="red");

# add plot for n = 10
tempN2 <- c(finalAnswer[,"B"]);
lines(probability.mat,tempN1,type="l",col="blue");

# add plot for n = 30
tempN2 <- c(finalAnswer[,"C"]);
lines(probability.mat,tempN1,type="l",col="yellow");

# add plot for n = 100
tempN2 <- c(finalAnswer[,"D"]);
lines(probability.mat,tempN1,type="l",col="green");



