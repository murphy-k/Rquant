#  Write a computer program (Matlab or R) that generates simulated MLE 
# estimators for the mean from 1, 000 Monte Carlo (MC) experiments of a random 
# draw of size K from a normal distribution with true mean 0 and variance σ as 
# input. Your program is required to populate 
# 1000 (MC estimators) × 4 (σ = 1, 2, 3, 4) × 6 (K = 5, 10, 25, 50, 75, 100) × 1 (MLE) = 24, 000 
# number of estimators that can be exported to Excel or alternative in a well 
# designed data structure or table format.

# define likelihood function
nll.normal <- function(data, params){
  return(-sum(log(dnorm(data, mean=params[1], sd=params[2]))))
}

# Monte Carlo Simualation
#K = 100 # (100,75,50,25,10,5)
#var = 4 # (4,3,2,1)
means <- list()
stds <- list()
js <- list()
ks <- list()

for(i in 1:1000){
  for(j in 1:4){
    for(k in c(5,10,25,50,75,100)){
      data <- rnorm(k, mean=0, sd=j)
      print(i)
      print(j)
      print(k)
      print(optim(par=c(1,1), fn=nll.normal, data=data)$par)
      means <- append(means, optim(par=c(1,1), fn=nll.normal, data=data)$par[1])
      stds <- append(stds, optim(par=c(1,1), fn=nll.normal, data=data)$par[2])
      js <- append(js, j)
      ks <- append(ks, k)
    }
  }
}
# Create dataframe with mean and sd estimates, and associated var and K values
df <- do.call(rbind, Map(data.frame, mean=means, stdev=stds, var = js, K=ks))
# Write to a .csv
write_csv(df, "data.csv")
