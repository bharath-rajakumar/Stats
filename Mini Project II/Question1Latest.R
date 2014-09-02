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

	#number of random variables to be generated
	n = 0;
	
	#number of times the simulation should be run
	nsim <- 1000;
	
	for(n in 5:100)
	{
		mle <- numeric(0);
		mome <- numeric(0);
		mleStdError <- numeric(0);
		momeStdError <- numeric(0);
		averageMleStdError <- numeric(0);
		averageMomeStdError <- numeric(0);
		for(i in 1:nsim) 
		{
			# create a random uniform distribution with n and theta
			randX<-runif(n,0,theta);

			# find maximum likelihood 
			mle<-c(mle,randX[which.max(randX)]);

			# find mome 
			mome<-c(mome,2*mean(randX));

			# find standard error of mle and mome
			mleStdError <- c(mleStdError,(mle[i] - theta)*(mle[i] - theta));
		
			momeStdError <- c(momeStdError,(mome[i] - theta)*(mome[i] - theta));
		}
		# find the mean of mle standard error and mome standard error
		averageMleStdError <- c(averageMleStdError,mean(mleStdError));

		averageMomeStdError <- c(averageMomeStdError,mean(momeStdError));
		

	}

	# MULTIPLE : plot the graph for each value of theta
	# plot(x,mleVector,type="l",col="red");
	# lines(x,momeVector,type="l",col="blue");

	# ONE : store global error 
	globalMLEvector <- c(globalMLEvector, mean(averageMleStdError));

	globalMOMEVector <- c(globalMOMEVector, mean(averageMomeStdError));

}

# ONE: plot the graph between theta and n

plot(y,globalMOMEVector,xlab = "Theta",ylab = "Mean Squared Error",type="l",col="blue");
lines(y,globalMLEvector,type="l",col="red");
