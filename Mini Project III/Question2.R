# this function will find the p-value for a two-sided t-test of a null
# hypothesis
find.p.value <- function(n) {
	# generate random normal sample N(0,1)
	r <- rnorm(n,0,1)

	# perform a two-sided t-test of null hypothesis
	result <- t.test(r,alternative = c("two.sided"))

	# extract the p-value
	return(result$p.value)
}
# Simulate the expermient for 1000 times and for different values of n
n <- c(5, 10, 30, 100);

nsim <- 1000;

# initiailize the vector that will hold all the p-values
pValues <- numeric(0);
for(i in n) {
	pValues <- replicate(nsim, find.p.value(i));	

	# plot the histogram
	hist(pValues);

	# generate a random uniform distribution with N = nsim
	u <- runif(nsim,0,1);

	# perform chi-squared goodness of fit test
	chisq.test(mydata,u);
	
	pValues <- numeric(0);
} 