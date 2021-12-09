library("mvtnorm")
dimensions = 4
num_samples = 5000
burn_in_threshold = 25

# Create a multivariate normal distribution f with some parameters
f_means = runif(dimensions)
f_cov = rWishart(1, dimensions, diag(dimensions))[, , 1]
f = function(x) {
	return(dmvnorm(x, f_means, f_cov))
}

# Create a multivariate normal proposal distribution
means = runif(dimensions)
covariance_matrix =rWishart(1, dimensions, diag(dimensions))[, , 1]
# Initialize previous with a random value
previous = rmvnorm(1, mean = means, sigma = covariance_matrix)
# Create an empty data frame to store the accepted values in
df = data.frame(matrix(ncol = dimensions, nrow = 0))

# Iterate till we have data points equal to the number of samples needed
for(i in 1 : num_samples + burn_in_threshold){
	# Continue this iteration till we accept a value from the proposal distribution
	while(TRUE) {
		# Generate a random value from proposal distribution
		current = rmvnorm(1, mean = means, sigma = covariance_matrix)
		# Calculate alpha using the current and previous
		numerator = f(current) * dmvnorm(previous, means, covariance_matrix) 
		denominator = f(previous) * dmvnorm(current, means, covariance_matrix) 
		alpha = numerator / denominator
		# Check if the current value is feasible
		if (runif(1) < min(alpha, 1)) {
			# Append the current value in data frame once the burn-in threshold has been surpassed
		  df = rbind(df, current)
			if(i > 29) {
			  print(i)
			  #print(df)
				covariance_matrix=cov(df)
				means=colMeans(df)
			}
			# Update the value of previous to current if current is feasible
			previous = current
			# Move onto the next iteration
			break
		}
	}
}

# Print the accepted values
print(head(df, 25))
# Plot a graph of the accepted values
plot(
	df, 
	col="orange",
	main="Point cloud of accepted values"
)
# Estimated means of accepted samples
print(colMeans(df))
# Original means used for f
print(f_means)

# Estimated covariance matrix of accepted samples
print(cov(df))
# Original covariance matrix used for f
print(f_cov)
