# MULTIPLE : plot all the graph in one window
# par(mfrow = c(2,5));
globalMLEvector <- numeric(0);
globalMOMEVector <- numeric(0);
x <- seq(5,100);
y <- seq(1,10);
for(theta in 1:10)
{
	mleVector <- numeric(0);
	momeVector <- numeric(0);
	n = 0;
	
	for(n in 5:100)
	{
		# create a random uniform distribution with n and theta
		randX<-runif(n,0,theta);

		# find maximum likelihood 
		mle<-randX[which.max(randX)];

		# find mome 
		mome<-2*mean(randX);

		# find standard error of mle and mome
		mleStdError <- (mle - theta)*(mle - theta);
		
		momeStdError <- (mome - theta)*(mome - theta);

		# store the results in a vector
		mleVector <- c(mleVector, mleStdError);
		
		momeVector <- c(momeVector, momeStdError);
		
	}

	# MULTIPLE : plot the graph for each value of theta
	# plot(x,mleVector,type="l",col="red");
	# lines(x,momeVector,type="l",col="blue");

	# ONE : store global error 
	globalMLEvector <- c(globalMLEvector, mean(mleVector));

	globalMOMEVector <- c(globalMOMEVector, mean(momeVector));

}

# ONE: plot the graph between theta and n

plot(y,globalMOMEVector,type="l",col="blue");
lines(y,globalMLEvector,type="l",col="red");
