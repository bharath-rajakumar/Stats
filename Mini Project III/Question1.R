# Input sample data
mydata<- c(-2.434,-2.336,	-2.192,	-2.01,	-1.967,	-1.707,	-1.678,	-1.563,	-1.476,	-1.388,
-1.331,	-1.269,	-1.229,	-1.227,	-1.174,	-1.136,	-1.127,	-1.124,	-1.12,	-1.073,
-1.052,	-1.051,	-1.032,	-0.938,	-0.884,	-0.847,	-0.846,	-0.716,	-0.644,	-0.625,
-0.588,	-0.584,	-0.496,	-0.489,	-0.473,	-0.453,	-0.427,	-0.395,	-0.386,	-0.386,
-0.373,	-0.344,	-0.28,	-0.246,	-0.239,	-0.211,	-0.188,	-0.155,	-0.149,	-0.112,
-0.103,	-0.101,	-0.033,	-0.011,	0.033,	0.11,	0.139,	0.143,	0.218,	0.218,
0.251,	0.261,	0.308,	0.343,	0.357,	0.463,	0.477,	0.482,	0.489,	0.545,
0.59,	0.638,	0.652,	0.656,	0.673,	0.772,	0.775,	0.776,	0.787,	0.969,
0.978,	1.005,	1.013,	1.039,	1.072,	1.168,	1.185,	1.263,	1.269,	1.297,
1.36,	1.37,	1.681,	1.721,	1.735,	1.779,	1.792,	1.881,	1.903,	2.009);

# transform mydata into a matrix
xmat <- matrix(mydata, byrow = T, ncol = 10);

# import the nortest package
library(nortest);

# perform chi-squared goodness of fit test and store it
ptest <- pearson.test(xmat);

#	Pearson chi-square normality test

#data:  xmat
#P = 8.94, p-value = 0.5378

# Since p-value > 0.05, we need to accept the null hypothesis i.e. the sample comes from a standard normal distribution

# Using Q-Q plot we can check if the distribution is indeed a standard normal distribution
qqnorm(xmat);
qqline(xmat);

# From the Q-Q plot we can observe that most of the points are on the line, and we can conclude that 
# the sample comes from a standard normal distribution

# the actual class intervals that this function creates to perform the tests can be found using n.classes value
# list the parameters that are associated with Pearson chi-square test
names(ptest);

# [1] "statistic" "p.value"   "method"    "data.name" "n.classes" "df"  
# Find number of classes 
ptest$n.classes;
[1] 13

# The number of classes is calculated using the formula
n.classes = ceiling(2 * (n^(2/5)))

# 10.3 (b)
# Store the Uniform(-3,3) distribution in a variable
u <- runif(100,-3,3);

# perform chi-squared goodness of fit test 
chisq.test(mydata,u);

#	Pearson's Chi-squared test

#data:  mydata and u
#X-squared = 9700, df = 9603, p-value = 0.2412

# Since p-value > 0.05, we need to accept that the distribution comes from Uniform distribution(-3,3)

# Using Q-Q plot we can check if the distribution is indeed a Uniform distribution(-3,3)
qqplot(mydata,u);
qqline(mydata);

# From the Q-Q plot we can observe that most of the points do NOT lie on the line, and we can conclude that 
# the sample does not come from a Uniform distribution(-3,3)
